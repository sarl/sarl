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

package io.janusproject.kernel.services.jdk.spawn;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;

import javax.inject.Inject;

import com.google.common.base.Strings;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.util.concurrent.Service;
import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.Singleton;
import io.janusproject.kernel.bic.BuiltinCapacityUtil;
import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.spawn.KernelAgentSpawnListener;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.services.spawn.SpawnServiceListener;
import io.janusproject.util.ListenerCollection;
import org.arakhne.afc.vmutil.locale.Locale;

import io.sarl.core.AgentKilled;
import io.sarl.core.AgentSpawned;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.Collections3;

/**
 * Implementation of a spawning service that is based on the other services of the Janus platform.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class StandardSpawnService extends AbstractDependentService implements SpawnService {

	private final ListenerCollection<?> globalListeners = new ListenerCollection<>();

	private final Multimap<UUID, SpawnServiceListener> agentLifecycleListeners = ArrayListMultimap.create();

	private final Map<UUID, Agent> agents = new TreeMap<>();

	private final Injector injector;

	/**
	 * Constructs the service with the given (injected) injector.
	 *
	 * @param injector the injector that should be used by this service for creating the agents.
	 */
	@Inject
	public StandardSpawnService(Injector injector) {
		this.injector = injector;
	}

	@Override
	public final Class<? extends Service> getServiceType() {
		return SpawnService.class;
	}

	@Override
	public Collection<Class<? extends Service>> getServiceDependencies() {
		return Arrays.<Class<? extends Service>>asList(ContextSpaceService.class);
	}

	private static void ensureSarlSpecificationVersion(Class<? extends Agent> agentClazz) {
		SarlSpecification annotation = agentClazz.getAnnotation(SarlSpecification.class);
		if (annotation != null) {
			String value = annotation.value();
			if (!Strings.isNullOrEmpty(value) && SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING.equals(value)) {
				return;
			}
		}
		throw new InvalidSarlSpecificationException(agentClazz);
	}

	@Override
	public synchronized UUID spawn(AgentContext parent, UUID agentID, Class<? extends Agent> agentClazz, Object... params) {
		if (isRunning()) {
			try {
				// Check if the version of the SARL agent class is compatible.
				ensureSarlSpecificationVersion(agentClazz);
				JustInTimeAgentInjectionModule agentInjectionModule = new JustInTimeAgentInjectionModule(this.injector,
						agentClazz, parent.getID(), agentID);
				Injector agentInjector = this.injector.createChildInjector(agentInjectionModule);
				Agent agent = agentInjector.getInstance(Agent.class);
				assert (agent != null);
				this.agents.put(agent.getID(), agent);
				fireAgentSpawned(parent, agent, params);
				return agent.getID();
			} catch (Throwable e) {
				throw new CannotSpawnException(agentClazz, e);
			}
		}
		throw new SpawnDisabledException(parent.getID(), agentClazz);
	}

	@Override
	public synchronized void killAgent(UUID agentID) throws AgentKillException {
		boolean error = !isRunning();
		// We should check if it is possible to kill the agent BEFORE killing it.
		Agent agent = this.agents.get(agentID);
		if (agent != null) {
			if (canKillAgent(agent)) {
				this.agents.remove(agentID);
				fireAgentDestroyed(agent);
				if (this.agents.isEmpty()) {
					fireKernelAgentDestroy();
				}
				if (error) {
					throw new SpawnServiceStopException(agentID);
				}
			} else {
				throw new AgentKillException(agentID);
			}
		}
	}

	/**
	 * Replies the registered agents.
	 *
	 * @return the registered agents.
	 */
	public synchronized SynchronizedSet<UUID> getAgents() {
		return Collections3.synchronizedSet(this.agents.keySet(), this);
	}

	/**
	 * Replies the registered agent.
	 *
	 * @param id is the identifier of the agent.
	 * @return the registered agent, or <code>null</code>.
	 */
	synchronized Agent getAgent(UUID id) {
		assert (id != null);
		return this.agents.get(id);
	}

	@Override
	public void addKernelAgentSpawnListener(KernelAgentSpawnListener listener) {
		this.globalListeners.add(KernelAgentSpawnListener.class, listener);
	}

	@Override
	public void removeKernelAgentSpawnListener(KernelAgentSpawnListener listener) {
		this.globalListeners.remove(KernelAgentSpawnListener.class, listener);
	}

	/**
	 * Notifies the listeners about the kernel agent creation.
	 */
	protected void fireKernelAgentSpawn() {
		for (KernelAgentSpawnListener l : this.globalListeners.getListeners(KernelAgentSpawnListener.class)) {
			l.kernelAgentSpawn();
		}
	}

	/**
	 * Notifies the listeners about the kernel agent destruction.
	 */
	protected void fireKernelAgentDestroy() {
		for (KernelAgentSpawnListener l : this.globalListeners.getListeners(KernelAgentSpawnListener.class)) {
			l.kernelAgentDestroy();
		}
	}

	@Override
	public void addSpawnServiceListener(UUID id, SpawnServiceListener agentLifecycleListener) {
		synchronized (this.agentLifecycleListeners) {
			this.agentLifecycleListeners.put(id, agentLifecycleListener);
		}
	}

	@Override
	public void addSpawnServiceListener(SpawnServiceListener agentLifecycleListener) {
		this.globalListeners.add(SpawnServiceListener.class, agentLifecycleListener);
	}

	@Override
	public void removeSpawnServiceListener(UUID id, SpawnServiceListener agentLifecycleListener) {
		synchronized (this.agentLifecycleListeners) {
			this.agentLifecycleListeners.remove(id, agentLifecycleListener);
		}
	}

	@Override
	public void removeSpawnServiceListener(SpawnServiceListener agentLifecycleListener) {
		this.globalListeners.remove(SpawnServiceListener.class, agentLifecycleListener);
	}

	/**
	 * Notifies the listeners about the agent creation.
	 *
	 * @param context - context in which the agent is spawn.
	 * @param agent - the spawn agent.
	 * @param initializationParameters - list of the values to pass as initialization parameters.
	 */
	protected void fireAgentSpawned(AgentContext context, Agent agent, Object[] initializationParameters) {
		// Notify the listeners on the spawn events (not restricted to a
		// single agent)
		for (SpawnServiceListener l : this.globalListeners.getListeners(SpawnServiceListener.class)) {
			l.agentSpawned(context, agent, initializationParameters);
		}

		// Notify the listeners on the lifecycle events on
		// the just spawned agent.
		// Usually, only BICs and the AgentLifeCycleSupport in
		// io.janusproject.kernel.bic.StandardBuiltinCapacitiesProvider
		// is invoked.
		SpawnServiceListener[] agentListeners;
		synchronized (this.agentLifecycleListeners) {
			Collection<SpawnServiceListener> list = this.agentLifecycleListeners.get(agent.getID());
			agentListeners = new SpawnServiceListener[list.size()];
			list.toArray(agentListeners);
		}
		for (SpawnServiceListener l : agentListeners) {
			l.agentSpawned(context, agent, initializationParameters);
		}

		// Send the event in the default space.
		UUID agentID = agent.getID();
		assert (agentID != null) : "Empty agent identifier"; //$NON-NLS-1$
		EventSpace defSpace = context.getDefaultSpace();
		assert (defSpace != null) : "A context does not contain a default space"; //$NON-NLS-1$
		Address agentAddress = defSpace.getAddress(agentID);
		assert (agentAddress != null) : "Cannot find an address in the default space for " + agentID; //$NON-NLS-1$

		defSpace.emit(new AgentSpawned(agentAddress, agentID, agent.getClass().getName()));
	}

	/**
	 * Replies if the given agent can be killed.
	 *
	 * @param agent - agent to test.
	 * @return <code>true</code> if the given agent can be killed, otherwise <code>false</code>.
	 */
	@SuppressWarnings("static-method")
	public synchronized boolean canKillAgent(Agent agent) {
		try {
			AgentContext ac = BuiltinCapacityUtil.getContextIn(agent);
			if (ac != null) {
				Set<UUID> participants = ac.getDefaultSpace().getParticipants();
				if (participants != null
						&& (participants.size() > 1 || (participants.size() == 1 && !participants.contains(agent.getID())))) {
					return false;
				}
			}
			return true;
		} catch (Throwable exception) {
			return false;
		}
	}

	/**
	 * Notifies the listeners about the agent destruction.
	 *
	 * @param agent - the destroyed agent.
	 */
	protected void fireAgentDestroyed(Agent agent) {
		SpawnServiceListener[] ilisteners;
		synchronized (this.agentLifecycleListeners) {
			Collection<SpawnServiceListener> list = this.agentLifecycleListeners.get(agent.getID());
			ilisteners = new SpawnServiceListener[list.size()];
			list.toArray(ilisteners);
		}

		SpawnServiceListener[] ilisteners2 = this.globalListeners.getListeners(SpawnServiceListener.class);

		try {
			SynchronizedCollection<AgentContext> sc = BuiltinCapacityUtil.getContextsOf(agent);
			synchronized (sc.mutex()) {
				for (AgentContext context : sc) {
					EventSpace defSpace = context.getDefaultSpace();
					defSpace.emit(new AgentKilled(defSpace.getAddress(agent.getID()), agent.getID(), agent.getClass().getName()));
				}
			}
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		for (SpawnServiceListener l : ilisteners) {
			l.agentDestroy(agent);
		}
		for (SpawnServiceListener l : ilisteners2) {
			l.agentDestroy(agent);
		}
	}

	@Override
	protected synchronized void doStart() {
		// Assume that when the service is starting, the kernel agent is up.
		fireKernelAgentSpawn();
		notifyStarted();
	}

	@Override
	protected synchronized void doStop() {
		synchronized (this.agentLifecycleListeners) {
			this.agentLifecycleListeners.clear();
		}
		notifyStopped();
	}

	/**
	 * This exception is thrown when the spawning service of agents is disabled.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class SpawnDisabledException extends RuntimeException {

		private static final long serialVersionUID = -380402400888610762L;

		/**
		 * @param parentID - the identifier of the parent entity that is creating the agent.
		 * @param agentClazz - the type of the agent to spawn.
		 */
		public SpawnDisabledException(UUID parentID, Class<? extends Agent> agentClazz) {
			super(Locale.getString(StandardSpawnService.class, "SPAWN_DISABLED", parentID, agentClazz)); //$NON-NLS-1$
		}

	}

	/**
	 * This exception is thrown when the spawning service is not running when the killing function on an agent is called.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class SpawnServiceStopException extends RuntimeException {

		private static final long serialVersionUID = 8104012713598435249L;

		/**
		 * @param agentID - the identifier of the agent.
		 */
		public SpawnServiceStopException(UUID agentID) {
			super(Locale.getString(StandardSpawnService.class, "KILL_DISABLED", agentID)); //$NON-NLS-1$
		}

	}

	/**
	 * This exception is thrown when the agent to spawn is not generated according to a valid SARL specification version.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class InvalidSarlSpecificationException extends RuntimeException {

		private static final long serialVersionUID = -3194494637438344108L;

		/**
		 * @param agentType the invalid type of agent.
		 */
		public InvalidSarlSpecificationException(Class<? extends Agent> agentType) {
			super(Locale.getString(StandardSpawnService.class, "INVALID_SARL_SPECIFICATION", agentType.getName())); //$NON-NLS-1$
		}

	}

	/**
	 * This exception is thrown when an agent cannot be spawned.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class CannotSpawnException extends RuntimeException {

		private static final long serialVersionUID = -380402400888610762L;

		/**
		 * @param agentClazz - the type of the agent to spawn.
		 * @param cause - the cause of the exception.
		 */
		public CannotSpawnException(Class<? extends Agent> agentClazz, Throwable cause) {
			super(Locale.getString(StandardSpawnService.class, "CANNOT_INSTANCIATE_AGENT", agentClazz, //$NON-NLS-1$
					(cause == null) ? null : cause.getLocalizedMessage()), cause);
		}

	}

	/**
	 * An injection module that is able to inject the parent ID and agent ID when creating an agent.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class JustInTimeAgentInjectionModule extends AbstractModule implements Provider<Agent> {

		private final Injector injector;

		private final Class<? extends Agent> agentType;

		private final UUID parentID;

		private final UUID agentID;

		JustInTimeAgentInjectionModule(Injector injector, Class<? extends Agent> agentType, UUID parentID, UUID agentID) {
			assert (injector != null);
			assert (agentType != null);
			assert (parentID != null);
			this.injector = injector;
			this.agentType = agentType;
			this.parentID = parentID;
			this.agentID = (agentID == null) ? UUID.randomUUID() : agentID;
		}

		@Override
		public void configure() {
			bind(Agent.class).toProvider(this);
		}

		@Override
		public Agent get() {
			try {
				BuiltinCapacitiesProvider capacityProvider = this.injector.getInstance(BuiltinCapacitiesProvider.class);
				Constructor<? extends Agent> constructor = this.agentType.getConstructor(BuiltinCapacitiesProvider.class,
						UUID.class, UUID.class);
				return constructor.newInstance(capacityProvider, this.parentID, this.agentID);
			} catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException
					| IllegalArgumentException | InvocationTargetException exception) {
				throw new CannotSpawnException(this.agentType, exception);
			}
		}

	}

}
