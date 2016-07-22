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

import java.util.UUID;

import com.google.inject.Injector;
import io.janusproject.services.contextspace.SpaceRepositoryListener;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.logging.LogService;
import io.janusproject.util.TwoStepConstruction;

import io.sarl.core.SpaceCreated;
import io.sarl.core.SpaceDestroyed;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.util.OpenEventSpace;
import io.sarl.util.OpenEventSpaceSpecification;

/**
 * Implementation of an agent context in the Janus platform.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@TwoStepConstruction
class Context implements AgentContext {

	private final UUID id;

	private final SpaceRepository spaceRepository;

	private final UUID defaultSpaceID;

	private OpenEventSpace defaultSpace;

	/**
	 * Constructs a <code>Context</code>.
	 *
	 * <p>
	 * CAUTION: Do not miss to call {@link #postConstruction()}.
	 *
	 * @param id - identifier of the context.
	 * @param defaultSpaceID - identifier of the default space in the context.
	 * @param factory - factory to use for creating the space repository.
	 * @param startUpListener - repository listener which is added just after the creation of the repository, but before the
	 *        creation of the default space.
	 */
	Context(UUID id, UUID defaultSpaceID, SpaceRepositoryFactory factory, SpaceRepositoryListener startUpListener) {
		assert (factory != null);
		this.id = id;
		this.defaultSpaceID = defaultSpaceID;
		this.spaceRepository = factory.newInstance(this, id.toString() + "-spaces", //$NON-NLS-1$
				startUpListener);
	}

	@Override
	public String toString() {
		return this.id.toString();
	}

	/**
	 * Create the default space in this context.
	 *
	 * @return the created space.
	 */
	EventSpace postConstruction() {
		this.spaceRepository.postConstruction();
		this.defaultSpace = createSpace(OpenEventSpaceSpecification.class, this.defaultSpaceID);
		if (this.defaultSpace == null) {
			// The default space could have been created before thanks to Hazelcast,
			// thus createSpace returns null because the space already exist,
			// in this case we return the already existing default space stored in the SpaceRepository
			this.defaultSpace = (OpenEventSpace) this.spaceRepository
					.getSpace(new SpaceID(this.id, this.defaultSpaceID, OpenEventSpaceSpecification.class));
		}
		return this.defaultSpace;
	}

	/**
	 * Destroy any associated resources.
	 */
	public void destroy() {
		this.spaceRepository.destroy();
	}

	@Override
	public UUID getID() {
		return this.id;
	}

	@Override
	public OpenEventSpace getDefaultSpace() {
		return this.defaultSpace;
	}

	@Override
	public <S extends Space> SynchronizedCollection<S> getSpaces(Class<? extends SpaceSpecification<S>> spec) {
		return this.spaceRepository.getSpaces(spec);
	}

	@Override
	public SynchronizedCollection<? extends io.sarl.lang.core.Space> getSpaces() {
		return this.spaceRepository.getSpaces();
	}

	@Override
	public <S extends io.sarl.lang.core.Space> S createSpace(Class<? extends SpaceSpecification<S>> spec, UUID spaceUUID,
			Object... creationParams) {
		return this.spaceRepository.createSpace(new SpaceID(this.id, spaceUUID, spec), spec, creationParams);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @deprecated see {@link #getOrCreateSpaceWithSpec(Class, UUID, Object...)}.
	 */
	@Override
	@Deprecated
	public <S extends Space> S getOrCreateSpace(Class<? extends SpaceSpecification<S>> spec, UUID spaceUUID,
			Object... creationParams) {
		return getOrCreateSpaceWithSpec(spec, spaceUUID, creationParams);
	}

	@Override
	public <S extends Space> S getOrCreateSpaceWithSpec(Class<? extends SpaceSpecification<S>> spec, UUID spaceUUID,
			Object... creationParams) {
		return this.spaceRepository.getOrCreateSpaceWithSpec(new SpaceID(this.id, spaceUUID, spec), spec, creationParams);
	}

	@Override
	public <S extends Space> S getOrCreateSpaceWithID(UUID spaceUUID, Class<? extends SpaceSpecification<S>> spec,
			Object... creationParams) {
		return this.spaceRepository.getOrCreateSpaceWithID(new SpaceID(this.id, spaceUUID, spec), spec, creationParams);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <S extends io.sarl.lang.core.Space> S getSpace(UUID spaceUUID) {
		// Type safety: assume that any ClassCastException will be thrown in the caller context.
		return (S) this.spaceRepository.getSpace(
				// The space specification parameter
				// could be null because it will
				// not be used during the search.
				new SpaceID(this.id, spaceUUID, null));
	}

	/**
	 * Listener on the events in the space repository.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SpaceListener implements SpaceRepositoryListener {

		private final Context context;

		private final SpaceRepositoryListener relay;

		private final LogService logger;

		/**
		 * @param context - the context that is owner this space listener.
		 * @param logger - the logging service to use.
		 * @param relay - the space repository listener to register at initialization time.
		 */
		SpaceListener(Context context, LogService logger, SpaceRepositoryListener relay) {
			assert (context != null);
			assert (logger != null);
			assert (relay != null);
			this.context = context;
			this.logger = logger;
			this.relay = relay;
		}

		@Override
		public void spaceCreated(Space space, boolean isLocalCreation) {
			this.logger.info(Context.class, "SPACE_CREATED", space.getID()); //$NON-NLS-1$
			// Notify the relays (other services)
			this.relay.spaceCreated(space, isLocalCreation);
			// Send the event in the default space of the context.
			if (isLocalCreation) {
				EventSpace defSpace = this.context.getDefaultSpace();
				// defSpace may be null if the created space is the default space.
				if (defSpace != null) {
					defSpace.emit(new SpaceCreated(new Address(defSpace.getID(), this.context.getID()), space.getID()));
				}
			}
		}

		@Override
		public void spaceDestroyed(Space space, boolean isLocalDestruction) {
			this.logger.info(Context.class, "SPACE_DESTROYED", space.getID()); //$NON-NLS-1$
			// Send the event in the default space of the context.
			if (isLocalDestruction) {
				EventSpace defSpace = this.context.getDefaultSpace();
				// defSpace may be null if the created space is the default space.
				if (defSpace != null) {
					defSpace.emit(new SpaceDestroyed(new Address(defSpace.getID(), this.context.getID()), space.getID()));
				}
			}
			// Notify the relays (other services)
			this.relay.spaceDestroyed(space, isLocalDestruction);
		}

	}

	/**
	 * Factory for the space repository in a context.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DefaultSpaceRepositoryFactory implements SpaceRepositoryFactory {

		private final DistributedDataStructureService dataStructureService;

		private final Injector injector;

		private final LogService logger;

		/**
		 * @param injector - instance of the injector to be used.
		 * @param distributedDataStructure - service that permits to obtain distributed data structure.
		 * @param logger - logging service.
		 */
		DefaultSpaceRepositoryFactory(Injector injector, DistributedDataStructureService distributedDataStructure,
				LogService logger) {
			this.dataStructureService = distributedDataStructure;
			this.injector = injector;
			this.logger = logger;
		}

		/**
		 * {@inheritDoc}
		 *
		 * <p>
		 * In opposite to {@link #newInstanceWithPrivateSpaceListener(Context, String, SpaceRepositoryListener)}, this function
		 * wraps the listener into a private space listener proxy before giving this wrapper to the space repository.
		 */
		@Override
		public final SpaceRepository newInstance(Context context, String distributedSpaceSetName,
				SpaceRepositoryListener listener) {
			return newInstanceWithPrivateSpaceListener(context, distributedSpaceSetName,
					new SpaceListener(context, this.logger, listener));
		}

		/**
		 * Create an instance of the space repository.
		 *
		 * <p>
		 * In opposite to {@link #newInstance(Context, String, SpaceRepositoryListener)}, this function gives the listener to the
		 * space repository, without wrapping it into the private space listener proxy.
		 *
		 * @param context - the context in which the space repository must be created.
		 * @param distributedSpaceSetName - name of the distribued data structure used by the space repository.
		 * @param listener - the listener on the space repository events to be register at initialization stage.
		 * @return the space repository
		 */
		protected SpaceRepository newInstanceWithPrivateSpaceListener(Context context, String distributedSpaceSetName,
				SpaceRepositoryListener listener) {
			return new SpaceRepository(distributedSpaceSetName, this.dataStructureService, this.injector, listener);
		}

	}

}
