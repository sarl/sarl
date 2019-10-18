/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.kernel.services.jdk.contextspace;

import java.io.Serializable;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.concurrent.locks.ReadWriteLock;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Multimap;
import com.google.common.collect.TreeMultimap;
import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.name.Names;

import io.janusproject.services.contextspace.SpaceRepositoryListener;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DMapListener;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.util.Comparators;
import io.janusproject.util.TwoStepConstruction;

import io.sarl.core.OpenEventSpace;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.util.DefaultSpace;
import io.sarl.util.concurrent.Collections3;

/**
 * A repository of spaces specific to a given context.
 *
 * <p>This repository is thread-safe.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@TwoStepConstruction
public class SpaceRepository {

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

	private final ReadWriteLock spaceIDsLock;

	/**
	 * Map linking a space id to its related Space object This is local non-distributed map.
	 */
	private final Map<SpaceID, Space> spaces;

	private final ReadWriteLock spacesLock;

	/**
	 * Map linking a a class of Space specification to its related implementations' ids Use the map <code>spaces</code> to get the
	 * Space object associated to a given id This is local non-distributed map.
	 */
	private final Multimap<Class<? extends SpaceSpecification<?>>, SpaceID> spacesBySpec;

	/** Reference to the default space of the owning context.
	 * @since 0.10
	 */
	private WeakReference<OpenEventSpace> defaultSpace;

	/** Constructor.
	 * @param distributedSpaceSetName the name used to identify distributed map over network
	 * @param distributedDataStructureService distributed data structure service.
	 * @param injector injector to used for creating new spaces.
	 * @param listener listener on the events in the space repository.
	 */
	SpaceRepository(String distributedSpaceSetName, DistributedDataStructureService distributedDataStructureService,
			Injector injector, SpaceRepositoryListener listener) {
		this.distributedSpaceSetName = distributedSpaceSetName;
		this.injector = injector;
		this.externalListener = listener;
		final Provider<ReadWriteLock> provider = this.injector.getProvider(ReadWriteLock.class);
		this.spaceIDsLock = provider.get();
		this.spacesLock = provider.get();
		this.spaces = new TreeMap<>();
		this.spacesBySpec = TreeMultimap.create(Comparators.CLASS_COMPARATOR, Comparators.OBJECT_COMPARATOR);
		this.spaceIDs = distributedDataStructureService.getMap(this.distributedSpaceSetName, null);
	}

	/**
	 * Finalize the initialization: ensure that the events are fired outside the scope of the SpaceRepository constructor.
	 */
	void postConstruction() {
		if (this.spaceIDs != null) {
			for (final Entry<SpaceID, Object[]> e : this.spaceIDs.entrySet()) {
				assert this.spaceIDs.containsKey(e.getKey());
				ensureLocalSpaceDefinition(e.getKey(), e.getValue());
			}
			this.internalListener = new SpaceDMapListener();
			this.spaceIDs.addDMapListener(this.internalListener);
		}
	}

	/** Replies the lock to be synchronized on the internal space repository.
	 *
	 * @return the lock.
	 */
	protected ReadWriteLock getSpaceRepositoryLock() {
		return this.spacesLock;
	}

	/** Replies the lock to be synchronized on the internal space ID repository.
	 *
	 * @return the lock.
	 */
	protected ReadWriteLock getSpaceIDsLock() {
		return this.spaceIDsLock;
	}

	/**
	 * Destroy this repository and releaqse all the resources.
	 */
	public void destroy() {
		final List<SpaceID> ids;
		// Unregister from Hazelcast layer.
		final ReadWriteLock lock = getSpaceIDsLock();
		lock.writeLock().lock();
		try {
			if (this.internalListener != null) {
				this.spaceIDs.removeDMapListener(this.internalListener);
			}
			// Delete the spaces. If this function is called, it
			// means that the spaces seems to have no more participant.
			// The process cannot be done through hazelcast.
			ids = new ArrayList<>(this.spaceIDs.keySet());
			this.spaceIDs.clear();
		} finally {
			lock.writeLock().unlock();
		}
		for (final SpaceID spaceId : ids) {
			removeLocalSpaceDefinition(spaceId, true);
		}
		// Remove the link to the default space of the ex-owning context
		this.defaultSpace = null;
	}

	private <S extends Space> S createSpaceInstance(Class<? extends SpaceSpecification<S>> spec, SpaceID spaceID,
			boolean isLocalCreation, Object[] creationParams) {
		final S space;
		assert spaceID.getSpaceSpecification() == null
				|| spaceID.getSpaceSpecification().equals(spec) : "The specification type is invalid"; //$NON-NLS-1$

		final OpenEventSpace defaultSpace = this.defaultSpace == null ? null : this.defaultSpace.get();
		final Injector defaultSpaceInjector;
		if (defaultSpace == null) {
			defaultSpaceInjector = this.injector;
		} else {
			final JustInTimeDefaultSpaceInjectionModule defaultSpaceInjectionModule = new JustInTimeDefaultSpaceInjectionModule(
					defaultSpace);
			defaultSpaceInjector = this.injector.createChildInjector(defaultSpaceInjectionModule);
		}
		final SpaceSpecification<S> spaceSpecificationInstance = defaultSpaceInjector.getInstance(spec);
		// Split the call to create() to let the JVM to create the "empty" array for creation parameters.
		if (creationParams != null && creationParams.length > 0) {
			space = spaceSpecificationInstance.create(spaceID, creationParams);
		} else {
			space = spaceSpecificationInstance.create(spaceID);
		}
		assert space != null;
		final SpaceID id = space.getSpaceID();
		assert id != null;
		this.spaces.put(id, space);
		this.spacesBySpec.put(id.getSpaceSpecification(), id);
		if (isLocalCreation) {
			Object[] sharedParams = NO_PARAMETERS;
			if (creationParams != null && creationParams.length > 0) {
				final List<Object> serializableParameters = new ArrayList<>(creationParams.length);
				for (final Object creationParameter : creationParams) {
					if (creationParameter instanceof Serializable) {
						serializableParameters.add(creationParameter);
					}
				}
				if (!serializableParameters.isEmpty()) {
					sharedParams = serializableParameters.toArray();
				}
			}
			final ReadWriteLock lock = getSpaceIDsLock();
			lock.writeLock().lock();
			try {
				this.spaceIDs.putIfAbsent(id, sharedParams);
			} finally {
				lock.writeLock().unlock();
			}
		}
		fireSpaceAdded(space, isLocalCreation);
		return space;
	}

	/**
	 * Add the existing, but not yet known, spaces into this repository.
	 *
	 * @param id identifier of the space
	 * @param initializationParameters parameters for initialization.
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected void ensureLocalSpaceDefinition(SpaceID id, Object[] initializationParameters) {
		final boolean create;
		final ReadWriteLock lock = getSpaceRepositoryLock();
		lock.readLock().lock();
		try {
			create = !this.spaces.containsKey(id);
		} finally {
			lock.readLock().unlock();
		}
		if (create) {
			// Caution: according to the lock's documentation, the writing lock cannot be obtained with reading lock handle
			lock.writeLock().lock();
			try {
				createSpaceInstance((Class) id.getSpaceSpecification(), id, false, initializationParameters);
			} finally {
				lock.writeLock().unlock();
			}
		}
	}

	/**
	 * Remove a remote space.
	 *
	 * @param id identifier of the space
	 * @param isLocalDestruction indicates if the destruction is initiated by the local kernel.
	 */
	protected void removeLocalSpaceDefinition(SpaceID id, boolean isLocalDestruction) {
		final Space space;
		final ReadWriteLock lock = getSpaceRepositoryLock();
		lock.writeLock().lock();
		try {
			space = this.spaces.remove(id);
			if (space != null) {
				this.spacesBySpec.remove(id.getSpaceSpecification(), id);
			}
		} finally {
			lock.writeLock().unlock();
		}
		if (space != null) {
			fireSpaceRemoved(space, isLocalDestruction);
		}
	}

	/**
	 * Remove all the remote spaces.
	 *
	 * @param isLocalDestruction indicates if the destruction is initiated by the local kernel.
	 */
	protected void removeLocalSpaceDefinitions(boolean isLocalDestruction) {
		List<Space> removedSpaces = null;
		final ReadWriteLock lock = getSpaceRepositoryLock();
		lock.readLock().lock();
		try {
			if (!this.spaces.isEmpty()) {
				removedSpaces = new ArrayList<>(this.spaces.size());
			}
		} finally {
			lock.readLock().unlock();
		}
		if (removedSpaces != null) {
			// Caution: according to the lock's documentation, the writing lock cannot be obtained with reading lock handle
			lock.writeLock().lock();
			try {
				final Iterator<Entry<SpaceID, Space>> iterator = this.spaces.entrySet().iterator();
				Space space;
				SpaceID id;
				while (iterator.hasNext()) {
					final Entry<SpaceID, Space> entry = iterator.next();
					id = entry.getKey();
					space = entry.getValue();
					iterator.remove();
					this.spacesBySpec.remove(id.getSpaceSpecification(), id);
					removedSpaces.add(space);
				}
			} finally {
				lock.writeLock().unlock();
			}
			for (final Space s : removedSpaces) {
				fireSpaceRemoved(s, isLocalDestruction);
			}
		}
	}

	/**
	 * Create a space.
	 *
	 * @param <S> - the type of the space to reply.
	 * @param spaceID ID of the space.
	 * @param spec specification of the space.
	 * @param creationParams creation parameters.
	 * @return the new space, or <code>null</code> if the space already exists.
	 */
	public <S extends io.sarl.lang.core.Space> S createSpace(SpaceID spaceID,
			Class<? extends SpaceSpecification<S>> spec, Object... creationParams) {
		final boolean create;
		final ReadWriteLock lock = getSpaceRepositoryLock();
		lock.readLock().lock();
		try {
			create = !this.spaces.containsKey(spaceID);
		} finally {
			lock.readLock().unlock();
		}
		if (create) {
			// Caution: according to the lock's documentation, the writing lock cannot be obtained with reading lock handle
			lock.writeLock().lock();
			try {
				return createSpaceInstance(spec, spaceID, true, creationParams);
			} finally {
				lock.writeLock().unlock();
			}
		}
		return null;
	}

	/**
	 * Retrieve the first space of the given specification, or create a space if none.
	 * The default space is ignored by this function. Consequently, even if the
	 * given specification is an {@code EventSpaceSpecification} or {@code OpenEventSpaceSpecification},
	 * a totally new space will be created if none already exist, except the default space.
	 *
	 * @param <S> - the type of the space to reply.
	 * @param spaceID ID of the space (used only when creating a space).
	 * @param spec specification of the space.
	 * @param creationParams creation parameters (used only when creating a space).
	 * @return the new space.
	 */
	@SuppressWarnings("unchecked")
	public <S extends io.sarl.lang.core.Space> S getOrCreateSpaceWithSpec(SpaceID spaceID,
			Class<? extends SpaceSpecification<S>> spec, Object... creationParams) {
		S firstSpace = null;
		final ReadWriteLock lock = getSpaceRepositoryLock();
		lock.readLock().lock();
		try {
			final Collection<SpaceID> ispaces = this.spacesBySpec.get(spec);
			if (ispaces != null && !ispaces.isEmpty()) {
				final OpenEventSpace defaultSpace = this.defaultSpace == null ? null : this.defaultSpace.get();
				if (defaultSpace == null) {
					final Iterator<SpaceID> idIterator = ispaces.iterator();
					while (firstSpace == null && idIterator.hasNext()) {
						final SpaceID currentId = idIterator.next();
						firstSpace = (S) this.spaces.get(currentId);
					}
				} else {
					final SpaceID defaultSpaceId = defaultSpace.getSpaceID();
					// Search for the first space that is not the default space.
					final Iterator<SpaceID> idIterator = ispaces.iterator();
					while (firstSpace == null && idIterator.hasNext()) {
						final SpaceID currentId = idIterator.next();
						if (!defaultSpaceId.equals(currentId)) {
							firstSpace = (S) this.spaces.get(currentId);
						}
					}
				}
			}
		} finally {
			lock.readLock().unlock();
		}
		if (firstSpace == null) {
			// Caution: according to the lock's documentation, the writing lock cannot be obtained with reading lock handle
			lock.writeLock().lock();
			try {
				firstSpace = createSpaceInstance(spec, spaceID, true, creationParams);
			} finally {
				lock.writeLock().unlock();
			}
		}
		assert firstSpace != null;
		return firstSpace;
	}

	/**
	 * Retrieve the first space of the given identifier, or create a space if none.
	 *
	 * @param <S> - the type of the space to reply.
	 * @param spaceID ID of the space.
	 * @param spec specification of the space.
	 * @param creationParams creation parameters (used only when creating a space).
	 * @return the new space.
	 */
	@SuppressWarnings("unchecked")
	public <S extends io.sarl.lang.core.Space> S getOrCreateSpaceWithID(SpaceID spaceID,
			Class<? extends SpaceSpecification<S>> spec, Object... creationParams) {
		final ReadWriteLock lock = getSpaceRepositoryLock();
		Space space;
		lock.readLock().lock();
		try {
			space = this.spaces.get(spaceID);
		} finally {
			lock.readLock().unlock();
		}
		if (space == null) {
			// Caution: according to the lock's documentation, the writing lock cannot be obtained with reading lock handle
			lock.writeLock().lock();
			try {
				space = createSpaceInstance(spec, spaceID, true, creationParams);
			} finally {
				lock.writeLock().unlock();
			}
		}
		assert space != null;
		return (S) space;
	}

	/**
	 * Returns the collection of all spaces stored in this repository.
	 *
	 * @return the collection of all spaces stored in this repository.
	 */
	public SynchronizedCollection<? extends Space> getSpaces() {
		final ReadWriteLock lock = getSpaceRepositoryLock();
		lock.readLock().lock();
		try {
			return Collections3.unmodifiableSynchronizedCollection(this.spaces.values(), lock);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Returns the collection of all spaces with the specified {@link SpaceSpecification} stored in this repository.
	 *
	 * @param <S> - type of the spaces to reply.
	 * @param spec the specification used to filter the set of stored spaces.
	 * @return the collection of all spaces with the specified {@link SpaceSpecification} stored in this repository
	 */
	@SuppressWarnings("unchecked")
	public <S extends Space> SynchronizedCollection<S> getSpaces(final Class<? extends SpaceSpecification<S>> spec) {
		final ReadWriteLock lock = getSpaceRepositoryLock();
		lock.readLock().lock();
		try {
			final Collection<S> backed = (Collection<S>) Collections2.filter(this.spaces.values(), new Predicate<Space>() {
				@Override
				public boolean apply(Space input) {
					return input.getSpaceID().getSpaceSpecification().equals(spec);
				}
			});
			return Collections3.unmodifiableSynchronizedCollection(backed, lock);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Returns the first instance of a space with the specified SpaceID.
	 *
	 * @param spaceID the identifier to retreive.
	 * @return the space instance of <code>null</code> if none.
	 */
	public Space getSpace(SpaceID spaceID) {
		final ReadWriteLock lock = getSpaceRepositoryLock();
		lock.readLock().lock();
		try {
			return this.spaces.get(spaceID);
		} finally {
			lock.readLock().unlock();
		}
	}

	/**
	 * Notifies the listeners on the space creation.
	 *
	 * @param space the created space.
	 * @param isLocalCreation indicates if the creation of the space was initiated on the current kernel.
	 */
	protected void fireSpaceAdded(Space space, boolean isLocalCreation) {
		if (this.externalListener != null) {
			this.externalListener.spaceCreated(space, isLocalCreation);
		}
	}

	/**
	 * Notifies the listeners on the space destruction.
	 *
	 * @param space the removed space.
	 * @param isLocalDestruction indicates if the destruction of the space was initiated on the current kernel.
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
			assert SpaceRepository.this.spaceIDs.containsKey(key);
			ensureLocalSpaceDefinition(key, value);
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void entryRemoved(SpaceID key, Object[] value) {
			assert !SpaceRepository.this.spaceIDs.containsKey(key);
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

	/** Set the reference to the default space for further space creations.
	 *
	 * @param defaultSpace the default space.
	 * @since 0.10
	 */
	final void setDefaultSpace(OpenEventSpace defaultSpace) {
		if (defaultSpace == null) {
			this.defaultSpace = null;
		} else {
			this.defaultSpace = new WeakReference<>(defaultSpace);
		}
	}

	/**
	 * An injection module that is able to inject the default space instance into another space implementation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class JustInTimeDefaultSpaceInjectionModule extends AbstractModule {

		private static final String NAME = "defaultSpace"; //$NON-NLS-1$

		private final OpenEventSpace defaultSpace;

		JustInTimeDefaultSpaceInjectionModule(OpenEventSpace defaultSpace) {
			assert defaultSpace != null;
			this.defaultSpace = defaultSpace;
		}

		@Override
		public void configure() {
			bind(OpenEventSpace.class).annotatedWith(Names.named(NAME)).toInstance(this.defaultSpace);
			bind(OpenEventSpace.class).annotatedWith(DefaultSpace.class).toInstance(this.defaultSpace);
		}

	}

}
