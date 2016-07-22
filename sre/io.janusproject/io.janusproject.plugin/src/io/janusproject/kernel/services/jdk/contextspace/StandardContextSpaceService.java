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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.util.concurrent.Service;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import io.janusproject.JanusConfig;
import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.contextspace.ContextRepositoryListener;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.contextspace.SpaceRepositoryListener;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DMapListener;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.util.ListenerCollection;
import io.janusproject.util.TwoStepConstruction;

import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.util.Collections3;

/**
 * A repository of Agent's context and spaces that is based on the other Janus platform services.
 *
 * @author $Author: ngaud$
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
@TwoStepConstruction
public class StandardContextSpaceService extends AbstractDependentService implements ContextSpaceService {

	private final ListenerCollection<?> listeners = new ListenerCollection<>();

	/**
	 * Factory of contexts.
	 */
	private ContextFactory contextFactory;

	/**
	 * Factory of space repositories.
	 */
	private SpaceRepositoryFactory spaceRepositoryFactory;

	/**
	 * Map linking a context id to its associated default space id. This map must be distributed and synchronized all over the
	 * network
	 */
	private DMap<UUID, SpaceID> defaultSpaces;

	/**
	 * Map linking a context id to its related Context object This is local non-distributed map.
	 */
	private Map<UUID, AgentContext> contexts = new TreeMap<>();

	/**
	 * Internal listener on the space repository changes.
	 */
	private ContextDMapListener dmapListener;

	private LogService logger;

	/**
	 * Constructs <code>ContextRepository</code>.
	 */
	public StandardContextSpaceService() {
		//
	}

	@Override
	public final Class<? extends Service> getServiceType() {
		return ContextSpaceService.class;
	}

	@Override
	public Collection<java.lang.Class<? extends Service>> getServiceDependencies() {
		return Arrays.<Class<? extends Service>>asList(DistributedDataStructureService.class, NetworkService.class,
				KernelDiscoveryService.class);
	}

	/**
	 * Replies the factory used to create contexts.
	 *
	 * @return the context factory.
	 */
	ContextFactory getContextFactory() {
		return this.contextFactory;
	}

	/**
	 * Change the factory used to create contexts.
	 *
	 * @param factory - the context factory.
	 */
	void setContextFactory(ContextFactory factory) {
		if (factory != null) {
			this.contextFactory = factory;
		}
	}

	/**
	 * Replies the factory used to create space repositories.
	 *
	 * @return the context factory.
	 */
	SpaceRepositoryFactory getSpaceRepositoryFactory() {
		return this.spaceRepositoryFactory;
	}

	/**
	 * Change the factory used to create space repositories.
	 *
	 * @param factory - the space repository factory.
	 */
	void setSpaceRepositoryFactory(SpaceRepositoryFactory factory) {
		if (factory != null) {
			this.spaceRepositoryFactory = factory;
		}
	}

	@Override
	public final Object mutex() {
		return this;
	}

	/**
	 * Initialize this service with injected objects.
	 *
	 * @param janusID - injected identifier.
	 * @param dataStructureService - service that permits to obtain distributed data structure.
	 * @param logService - service of logging.
	 * @param injector - the injector to use.
	 */
	@Inject
	synchronized void postConstruction(@Named(JanusConfig.DEFAULT_CONTEXT_ID_NAME) UUID janusID,
			DistributedDataStructureService dataStructureService, LogService logService, Injector injector) {
		this.logger = logService;
		this.contextFactory = new DefaultContextFactory();
		this.spaceRepositoryFactory = new Context.DefaultSpaceRepositoryFactory(injector, dataStructureService, logService);
		this.defaultSpaces = dataStructureService.getMap(janusID.toString(), null);
	}

	@Override
	public synchronized boolean isEmptyContextRepository() {
		return this.contexts.isEmpty();
	}

	@Override
	public synchronized int getNumberOfContexts() {
		return this.contexts.size();
	}

	@Override
	public synchronized boolean containsContext(UUID contextID) {
		return this.contexts.containsKey(contextID);
	}

	@Override
	public synchronized AgentContext createContext(UUID contextID, UUID defaultSpaceUUID) {
		assert (contextID != null) : "The contextID cannot be null"; //$NON-NLS-1$
		assert (defaultSpaceUUID != null) : "The defaultSpaceUUID cannot be null"; //$NON-NLS-1$
		assert (this.contexts != null) : "Internal Error: the context container must not be null"; //$NON-NLS-1$
		AgentContext context = this.contexts.get(contextID);
		if (context == null) {
			Context ctx = this.contextFactory.newInstance(contextID, defaultSpaceUUID, this.spaceRepositoryFactory,
					new SpaceEventProxy());
			assert (ctx != null) : "The internal Context cannot be null"; //$NON-NLS-1$
			assert (this.contexts != null) : "Internal Error: the context container must not be null"; //$NON-NLS-1$
			this.contexts.put(contextID, ctx);
			fireContextCreated(ctx);
			Space defaultSpace = ctx.postConstruction();
			assert (defaultSpace != null) : "The default space in the context " //$NON-NLS-1$
					+ contextID + " cannot be null"; //$NON-NLS-1$
			this.defaultSpaces.putIfAbsent(ctx.getID(), defaultSpace.getID());
			return ctx;
		}
		return context;
	}

	@Override
	public final void removeContext(AgentContext context) {
		removeContext(context.getID());
	}

	@Override
	public synchronized void removeContext(UUID contextID) {
		this.defaultSpaces.remove(contextID);
		AgentContext context = this.contexts.remove(contextID);
		if (context != null) {
			((Context) context).destroy();
			fireContextDestroyed(context);
		}
	}

	@Override
	public synchronized Collection<AgentContext> getContexts() {
		return Collections.unmodifiableCollection(Collections3.synchronizedCollection(this.contexts.values(), mutex()));
	}

	@Override
	public synchronized Collection<AgentContext> getContexts(final Collection<UUID> contextIDs) {
		return Collections2.filter(this.contexts.values(), new Predicate<AgentContext>() {
			@Override
			public boolean apply(AgentContext input) {
				return contextIDs.contains(input.getID());
			}
		});
	}

	@Override
	public synchronized Set<UUID> getContextIDs() {
		return Collections.unmodifiableSet(Collections3.synchronizedSet(this.contexts.keySet(), mutex()));
	}

	@Override
	public synchronized AgentContext getContext(UUID contextID) {
		return this.contexts.get(contextID);
	}

	@Override
	public void addContextRepositoryListener(ContextRepositoryListener listener) {
		this.listeners.add(ContextRepositoryListener.class, listener);
	}

	@Override
	public void removeContextRepositoryListener(ContextRepositoryListener listener) {
		this.listeners.remove(ContextRepositoryListener.class, listener);
	}

	/**
	 * Notifies the listeners about a context creation.
	 *
	 * @param context - reference to the created context.
	 */
	protected void fireContextCreated(AgentContext context) {
		ContextRepositoryListener[] ilisteners = this.listeners.getListeners(ContextRepositoryListener.class);
		this.logger.info(StandardContextSpaceService.class, "CONTEXT_CREATED", context.getID()); //$NON-NLS-1$
		for (ContextRepositoryListener listener : ilisteners) {
			listener.contextCreated(context);
		}
	}

	/**
	 * Notifies the listeners about a context destruction.
	 *
	 * @param context - reference to the destroyed context.
	 */
	protected void fireContextDestroyed(AgentContext context) {
		ContextRepositoryListener[] ilisteners = this.listeners.getListeners(ContextRepositoryListener.class);
		this.logger.info(StandardContextSpaceService.class, "CONTEXT_DESTROYED", context.getID()); //$NON-NLS-1$
		for (ContextRepositoryListener listener : ilisteners) {
			listener.contextDestroyed(context);
		}
	}

	@Override
	public void addSpaceRepositoryListener(SpaceRepositoryListener listener) {
		this.listeners.add(SpaceRepositoryListener.class, listener);
	}

	@Override
	public void removeSpaceRepositoryListener(SpaceRepositoryListener listener) {
		this.listeners.remove(SpaceRepositoryListener.class, listener);
	}

	/**
	 * Notifies the listeners on the space creation.
	 *
	 * @param space - reference to the created space.
	 * @param isLocalCreation - indicates if the space was initially created on the current kernel.
	 */
	protected void fireSpaceCreated(Space space, boolean isLocalCreation) {
		for (SpaceRepositoryListener listener : this.listeners.getListeners(SpaceRepositoryListener.class)) {
			listener.spaceCreated(space, isLocalCreation);
		}
	}

	/**
	 * Notifies the listeners on the space destruction.
	 *
	 * @param space - reference to the destroyed space.
	 * @param isLocalDestruction - indicates if the space was destroyed in the current kernel.
	 */
	protected void fireSpaceDestroyed(Space space, boolean isLocalDestruction) {
		for (SpaceRepositoryListener listener : this.listeners.getListeners(SpaceRepositoryListener.class)) {
			listener.spaceDestroyed(space, isLocalDestruction);
		}
	}

	/**
	 * Update the internal data structure when a default space was discovered.
	 *
	 * @param spaceID - identifier of the space to initialize.
	 */
	protected synchronized void ensureDefaultSpaceDefinition(SpaceID spaceID) {
		UUID contextID = spaceID.getContextID();
		createContext(contextID, spaceID.getID());
	}

	/**
	 * Update the internal data structure when a default space was removed.
	 *
	 * @param spaceID - identifier of the space to remove.
	 */
	protected synchronized void removeDefaultSpaceDefinition(SpaceID spaceID) {
		AgentContext context = this.contexts.remove(spaceID.getContextID());
		if (context != null) {
			fireContextDestroyed(context);
		}
	}

	@Override
	protected synchronized void doStart() {
		for (SpaceID space : this.defaultSpaces.values()) {
			ensureDefaultSpaceDefinition(space);
		}
		this.dmapListener = new ContextDMapListener();
		this.defaultSpaces.addDMapListener(this.dmapListener);
		notifyStarted();
	}

	@Override
	protected synchronized void doStop() {
		if (this.dmapListener != null) {
			this.defaultSpaces.removeDMapListener(this.dmapListener);
		}
		// Unconnect the default space collection from remote clusters
		// Not needed becasue the Kernel will be stopped: this.defaultSpaces.destroy();
		// Delete the contexts from this repository
		Map<UUID, AgentContext> old = this.contexts;
		this.contexts = new TreeMap<>();
		for (AgentContext context : old.values()) {
			((Context) context).destroy();
			fireContextDestroyed(context);
		}
		notifyStopped();
	}

	/**
	 * Listener on map events from Hazelcast.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class ContextDMapListener implements DMapListener<UUID, SpaceID> {

		/**
		 * Construct.
		 */
		ContextDMapListener() {
			//
		}

		@Override
		public void entryAdded(UUID key, SpaceID value) {
			assert (value != null);
			ensureDefaultSpaceDefinition(value);
		}

		@Override
		public void entryRemoved(UUID key, SpaceID value) {
			assert (value != null);
			removeDefaultSpaceDefinition(value);
		}

		@Override
		public void entryUpdated(UUID key, SpaceID value) {
			//
		}

		@Override
		public void mapCleared(boolean localClearing) {
			throw new UnsupportedOperationException();
		}

	}

	/**
	 * Proxy for space events.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class SpaceEventProxy implements SpaceRepositoryListener {

		/**
		 * Construct.
		 */
		SpaceEventProxy() {
			//
		}

		@Override
		public void spaceCreated(Space space, boolean isLocalCreation) {
			fireSpaceCreated(space, isLocalCreation);
		}

		@Override
		public void spaceDestroyed(Space space, boolean isLocalDestruction) {
			fireSpaceDestroyed(space, isLocalDestruction);
		}

	}

	/**
	 * Factory of contexts.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class DefaultContextFactory implements ContextFactory {

		/**
		 * Create an instance of context.
		 */
		DefaultContextFactory() {
			//
		}

		@Override
		public Context newInstance(UUID contextId, UUID defaultSpaceId, SpaceRepositoryFactory factory,
				SpaceRepositoryListener listener) {
			return new Context(contextId, defaultSpaceId, factory, listener);
		}

	}

}
