/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.kernel.services.jdk.contextspace;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Multimap;
import com.google.common.collect.TreeMultimap;
import com.google.inject.Injector;
import io.janusproject.services.contextspace.SpaceRepositoryListener;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DMapListener;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.util.TwoStepConstruction;
import org.arakhne.afc.vmutil.ClassComparator;
import org.arakhne.afc.vmutil.ObjectReferenceComparator;

import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.util.Collections3;

/**
 * A repository of spaces specific to a given context.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@TwoStepConstruction
class SpaceRepository {

	private static final Object[] NO_PARAMETERS = new Object[0];

	private final String distributedSpaceSetName;

	private final Injector injector;

	/**
	 * Listener used as internal implementation for this repository.
	 */
	private SpaceDMapListener internalListener;

	/**
	 * Listener on the events in this repository (basically somewhere in the Context).
	 */
	private final SpaceRepositoryListener externalListener;

	/**
	 * The set of the id of all spaces stored in this repository This set must be distributed and synchronized all over the
	 * network.
	 */
	private final DMap<SpaceID, Object[]> spaceIDs;

	/**
	 * Map linking a space id to its related Space object This is local non-distributed map.
	 */
	private final Map<SpaceID, Space> spaces;

	/**
	 * Map linking a a class of Space specification to its related implementations' ids Use the map <code>spaces</code> to get the
	 * Space object associated to a given id This is local non-distributed map.
	 */
	private final Multimap<Class<? extends SpaceSpecification<?>>, SpaceID> spacesBySpec;

	/**
	 * @param distributedSpaceSetName - the name used to identify distributed map over network
	 * @param distributedDataStructureService - distributed data structure service.
	 * @param injector - injector to used for creating new spaces.
	 * @param listener - listener on the events in the space repository.
	 */
	SpaceRepository(String distributedSpaceSetName, DistributedDataStructureService distributedDataStructureService,
			Injector injector, SpaceRepositoryListener listener) {
		this.distributedSpaceSetName = distributedSpaceSetName;
		this.injector = injector;
		this.externalListener = listener;
		this.spaces = new TreeMap<>();
		this.spacesBySpec = TreeMultimap.create(ClassComparator.SINGLETON, ObjectReferenceComparator.SINGLETON);
		this.spaceIDs = distributedDataStructureService.getMap(this.distributedSpaceSetName, null);
	}

	/**
	 * Finalize the initialization: ensure that the events are fired outside the scope of the SpaceRepository constructor.
	 */
	synchronized void postConstruction() {
		for (Entry<SpaceID, Object[]> e : this.spaceIDs.entrySet()) {
			assert (this.spaceIDs.containsKey(e.getKey()));
			ensureLocalSpaceDefinition(e.getKey(), e.getValue());
		}
		this.internalListener = new SpaceDMapListener();
		this.spaceIDs.addDMapListener(this.internalListener);
	}

	/**
	 * Destroy this repository and releaqse all the resources.
	 */
	public synchronized void destroy() {
		// Unregister from Hazelcast layer.
		if (this.internalListener != null) {
			this.spaceIDs.removeDMapListener(this.internalListener);
		}
		// Delete the spaces. If this function is called, it
		// means that the spaces seems to have no more participant.
		// The process cannot be done through hazelcast.
		List<SpaceID> ids = new ArrayList<>(this.spaceIDs.keySet());
		this.spaceIDs.clear();
		for (SpaceID spaceId : ids) {
			removeLocalSpaceDefinition(spaceId, true);
		}
	}

	private synchronized <S extends Space> S createSpaceInstance(Class<? extends SpaceSpecification<S>> spec, SpaceID spaceID,
			boolean isLocalCreation, Object[] creationParams) {
		S space;
		assert (spaceID.getSpaceSpecification() == null
				|| spaceID.getSpaceSpecification().equals(spec)) : "The specification type is invalid"; //$NON-NLS-1$
		// Split the call to create() to let the JVM to create the "empty" array for creation parameters.
		if (creationParams != null && creationParams.length > 0) {
			space = this.injector.getInstance(spec).create(spaceID, creationParams);
		} else {
			space = this.injector.getInstance(spec).create(spaceID);
		}
		assert (space != null);
		SpaceID id = space.getID();
		assert (id != null);
		this.spaces.put(id, space);
		this.spacesBySpec.put(id.getSpaceSpecification(), id);
		if (isLocalCreation) {
			Object[] sharedParams = NO_PARAMETERS;
			if (creationParams != null && creationParams.length > 0) {
				List<Object> serializableParameters = new ArrayList<>(creationParams.length);
				for (Object creationParameter : creationParams) {
					if (creationParameter instanceof Serializable) {
						serializableParameters.add(creationParameter);
					}
				}
				if (!serializableParameters.isEmpty()) {
					sharedParams = serializableParameters.toArray();
				}
			}
			this.spaceIDs.putIfAbsent(id, sharedParams);
		}
		fireSpaceAdded(space, isLocalCreation);
		return space;
	}

	/**
	 * Add the existing, but not yet known, spaces into this repository.
	 *
	 * @param id - identifier of the space
	 * @param initializationParameters - parameters for initialization.
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected synchronized void ensureLocalSpaceDefinition(SpaceID id, Object[] initializationParameters) {
		if (!this.spaces.containsKey(id)) {
			createSpaceInstance((Class) id.getSpaceSpecification(), id, false, initializationParameters);
		}
	}

	/**
	 * Remove a remote space.
	 *
	 * @param id - identifier of the space
	 * @param isLocalDestruction - indicates if the destruction is initiated by the local kernel.
	 */
	protected synchronized void removeLocalSpaceDefinition(SpaceID id, boolean isLocalDestruction) {
		Space space = this.spaces.remove(id);
		if (space != null) {
			this.spacesBySpec.remove(id.getSpaceSpecification(), id);
			fireSpaceRemoved(space, isLocalDestruction);
		}
	}

	/**
	 * Remove all the remote spaces.
	 *
	 * @param isLocalDestruction - indicates if the destruction is initiated by the local kernel.
	 */
	protected synchronized void removeLocalSpaceDefinitions(boolean isLocalDestruction) {
		if (!this.spaces.isEmpty()) {
			List<Space> removedSpaces = new ArrayList<>(this.spaces.size());
			Iterator<Entry<SpaceID, Space>> iterator = this.spaces.entrySet().iterator();
			Space space;
			SpaceID id;
			while (iterator.hasNext()) {
				Entry<SpaceID, Space> entry = iterator.next();
				id = entry.getKey();
				space = entry.getValue();
				iterator.remove();
				this.spacesBySpec.remove(id.getSpaceSpecification(), id);
				removedSpaces.add(space);
			}
			for (Space s : removedSpaces) {
				fireSpaceRemoved(s, isLocalDestruction);
			}
		}
	}

	/**
	 * Create a space.
	 *
	 * @param <S> - the type of the space to reply.
	 * @param spaceID - ID of the space.
	 * @param spec - specification of the space.
	 * @param creationParams - creation parameters.
	 * @return the new space, or <code>null</code> if the space already exists.
	 */
	public synchronized <S extends io.sarl.lang.core.Space> S createSpace(SpaceID spaceID,
			Class<? extends SpaceSpecification<S>> spec, Object... creationParams) {
		if (!this.spaces.containsKey(spaceID)) {
			return createSpaceInstance(spec, spaceID, true, creationParams);
		}
		return null;
	}

	/**
	 * Retrieve the first space of the given specification, or create a space if none.
	 *
	 * @param <S> - the type of the space to reply.
	 * @param spaceID - ID of the space (used only when creating a space).
	 * @param spec - specification of the space.
	 * @param creationParams - creation parameters (used only when creating a space).
	 * @return the new space.
	 */
	@SuppressWarnings("unchecked")
	public synchronized <S extends io.sarl.lang.core.Space> S getOrCreateSpaceWithSpec(SpaceID spaceID,
			Class<? extends SpaceSpecification<S>> spec, Object... creationParams) {
		Collection<SpaceID> ispaces = this.spacesBySpec.get(spec);
		S firstSpace;
		if (ispaces == null || ispaces.isEmpty()) {
			firstSpace = createSpaceInstance(spec, spaceID, true, creationParams);
		} else {
			firstSpace = (S) this.spaces.get(ispaces.iterator().next());
		}
		assert (firstSpace != null);
		return firstSpace;
	}

	/**
	 * Retrieve the first space of the given identifier, or create a space if none.
	 *
	 * @param <S> - the type of the space to reply.
	 * @param spaceID - ID of the space.
	 * @param spec - specification of the space.
	 * @param creationParams - creation parameters (used only when creating a space).
	 * @return the new space.
	 */
	@SuppressWarnings("unchecked")
	public synchronized <S extends io.sarl.lang.core.Space> S getOrCreateSpaceWithID(SpaceID spaceID,
			Class<? extends SpaceSpecification<S>> spec, Object... creationParams) {
		Space space = this.spaces.get(spaceID);
		if (space == null) {
			space = createSpaceInstance(spec, spaceID, true, creationParams);
		}
		assert (space != null);
		return (S) space;
	}

	/**
	 * Returns the collection of all spaces stored in this repository.
	 *
	 * @return the collection of all spaces stored in this repository.
	 */
	public synchronized SynchronizedCollection<? extends Space> getSpaces() {
		return Collections3.synchronizedCollection(Collections.unmodifiableCollection(this.spaces.values()), this);
	}

	/**
	 * Returns the collection of all spaces with the specified {@link SpaceSpecification} stored in this repository.
	 *
	 * @param <S> - type of the spaces to reply.
	 * @param spec - the specification used to filter the set of stored spaces.
	 * @return the collection of all spaces with the specified {@link SpaceSpecification} stored in this repository
	 */
	@SuppressWarnings("unchecked")
	public synchronized <S extends Space> SynchronizedCollection<S> getSpaces(final Class<? extends SpaceSpecification<S>> spec) {
		return Collections3
				.synchronizedCollection((Collection<S>) Collections2.filter(this.spaces.values(), new Predicate<Space>() {
					@Override
					public boolean apply(Space input) {
						return input.getID().getSpaceSpecification().equals(spec);
					}
				}), this);
	}

	/**
	 * Returns the first instance of a space with the specified SpaceID.
	 *
	 * @param spaceID - the identifier to retreive.
	 * @return the space instance of <code>null</code> if none.
	 */
	public synchronized Space getSpace(SpaceID spaceID) {
		return this.spaces.get(spaceID);
	}

	/**
	 * Notifies the listeners on the space creation.
	 *
	 * @param space - the created space.
	 * @param isLocalCreation - indicates if the creation of the space was initiated on the current kernel.
	 */
	protected void fireSpaceAdded(Space space, boolean isLocalCreation) {
		if (this.externalListener != null) {
			this.externalListener.spaceCreated(space, isLocalCreation);
		}
	}

	/**
	 * Notifies the listeners on the space destruction.
	 *
	 * @param space - the removed space.
	 * @param isLocalDestruction - indicates if the destruction of the space was initiated on the current kernel.
	 */
	protected void fireSpaceRemoved(Space space, boolean isLocalDestruction) {
		if (this.externalListener != null) {
			this.externalListener.spaceDestroyed(space, isLocalDestruction);
		}
	}

	/**
	 * Listener on events related to the space service.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class SpaceDMapListener implements DMapListener<SpaceID, Object[]> {

		/**
		 * Construct.
		 */
		SpaceDMapListener() {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void entryAdded(SpaceID key, Object[] value) {
			assert (SpaceRepository.this.spaceIDs.containsKey(key));
			ensureLocalSpaceDefinition(key, value);
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void entryRemoved(SpaceID key, Object[] value) {
			assert (!SpaceRepository.this.spaceIDs.containsKey(key));
			removeLocalSpaceDefinition(key, false);
		}

		@Override
		public void entryUpdated(SpaceID key, Object[] value) {
			//
		}

		@Override
		public void mapCleared(boolean localClearing) {
			removeLocalSpaceDefinitions(false);
		}

	}

}
