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

package io.janusproject.kernel;

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.google.common.util.concurrent.Service;
import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Singleton;

import io.janusproject.kernel.services.jdk.spawn.CannotSpawnException;
import io.janusproject.services.IServiceManager;
import io.janusproject.services.Services;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.spawn.KernelAgentSpawnListener;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.util.TwoStepConstruction;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.util.SynchronizedSet;

/**
 * This class represents the Kernel of the Janus platform.
 *
 * <p><strong>The Kernel is a singleton.</strong>
 *
 * <p>The Kernel is assimilated to an agent that is omniscient and distributed other the network. It is containing all the other
 * agents.
 *
 * <p>To create a Kernel, you should use the function {@link #create(Module...)}.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
@TwoStepConstruction(names = { "setJanusContext" })
public class Kernel {

	private AgentContext janusContext;

	private final IServiceManager serviceManager;

	private final SpawnService spawnService;

	private final LogService loggingService;

	private final AtomicBoolean isRunning = new AtomicBoolean(true);

	/**
	 * Constructs a Janus kernel.
	 *
	 * @param serviceManager is the instance of the service manager that must be used by the kernel.
	 * @param spawnService is the instance of the spawn service.
	 * @param loggingService is the instance of the logging service.
	 * @param exceptionHandler is the handler of the uncaught exceptions.
	 */
	@Inject
	Kernel(IServiceManager serviceManager, SpawnService spawnService, LogService loggingService,
			UncaughtExceptionHandler exceptionHandler) {
		// Initialize the fields
		this.serviceManager = serviceManager;
		this.spawnService = spawnService;
		this.loggingService = loggingService;

		// Ensure that all the threads has a default hander.
		Thread.setDefaultUncaughtExceptionHandler(exceptionHandler);

		// Listen on the kernel's events
		this.spawnService.addKernelAgentSpawnListener(new KernelStoppingListener());

		// Start the services NOW to ensure that the default context and space
		// of the Janus agent are catched by the modules;
		Services.startServices(this.serviceManager);
	}

	/**
	 * Create an instance of {@link Kernel}.
	 *
	 * @param modules modules to link to the new kernel.
	 * @return the new kernel.
	 */
	public static final Kernel create(Module... modules) {
		final Injector injector = Guice.createInjector(modules);
		final Kernel k = injector.getInstance(Kernel.class);
		return k;
	}

	/**
	 * Replies a behavior that stops the kernel.
	 *
	 * @return the logger of the kernel.
	 * @since 0.10
	 */
	public Runnable getStopBehavior() {
		return () -> {
			final SpawnService service = getService(SpawnService.class);
			if (service != null) {
				final SynchronizedSet<UUID> agents = service.getAgents();
				final List<UUID> agentIds;
				final ReadWriteLock lock = agents.getLock();
				lock.readLock().lock();
				try {
					agentIds = new ArrayList<>(agents);
				} finally {
					lock.readLock().unlock();
				}
				boolean killed = false;
				for (final UUID agentId : agentIds) {
					if (service.killAgent(agentId, true)) {
						killed = true;
					}
				}
				if (!killed) {
					new StopTheKernel().startAsync();
				}
			}
		};
	}

	/**
	 * Replies if the kernel is running or not.
	 *
	 * @return <code>true</code> if the kernel is running; <code>false</code> otherwise.
	 */
	public boolean isRunning() {
		return this.isRunning.get();
	}

	/**
	 * Replies the number of agents that are registered on this kernel.
	 * This function does not consider the agents that are on remote kernels.
	 *
	 * @return the number of agents on this local kernel.
	 * @since 0.10
	 */
	public int getAgentCount() {
		final SpawnService service = getService(SpawnService.class);
		if (service != null) {
			final SynchronizedSet<UUID> agents = service.getAgents();
			final ReadWriteLock lock = agents.getLock();
			lock.readLock().lock();
			try {
				return agents.size();
			} finally {
				lock.readLock().unlock();
			}
		}
		return 0;
	}

	/**
	 * Spawn an agent of the given type, and pass the parameters to its initialization function.
	 *
	 * @param agent the type of the agent to spawn.
	 * @param params the list of the parameters to pass to the agent initialization function.
	 * @return the identifier of the agent, never <code>null</code>.
	 */
	public UUID spawn(Class<? extends Agent> agent, Object... params) {
		final List<UUID> ids = this.spawnService.spawn(1, null, this.janusContext, null, agent, params);
		if (ids.isEmpty()) {
			throw new CannotSpawnException(agent, null);
		}
		return ids.get(0);
	}

	/**
	 * Spawn agents of the given type, and pass the parameters to its initialization function.
	 *
	 * @param nbAgents the number of agents to spawn.
	 * @param agent the type of the agents to spawn.
	 * @param params the list of the parameters to pass to the agent initialization function.
	 * @return the identifiers of the agents, never <code>null</code>.
	 */
	public List<UUID> spawn(int nbAgents, Class<? extends Agent> agent, Object... params) {
		return this.spawnService.spawn(nbAgents, null, this.janusContext, null, agent, params);
	}

	/**
	 * Spawn an agent of the given type, and pass the parameters to its initialization function.
	 *
	 * @param agentID the identifier of the agent to spawn. If <code>null</code> the identifier is randomly selected.
	 * @param agent the type of the agent to spawn.
	 * @param params the list of the parameters to pass to the agent initialization function.
	 * @return the identifier of the agent, never <code>null</code>.
	 */
	public UUID spawn(UUID agentID, Class<? extends Agent> agent, Object... params) {
		final List<UUID> ids = this.spawnService.spawn(1, null, this.janusContext, agentID, agent, params);
		if (ids.isEmpty()) {
			return null;
		}
		return ids.get(0);
	}

	/**
	 * Replies a kernel service that is alive.
	 *
	 * @param <S> - type of the type to reply.
	 * @param type type of the type to reply.
	 * @return the service, or <code>null</code>.
	 */
	public <S extends Service> S getService(Class<S> type) {
		for (final Service serv : this.serviceManager.servicesByState().values()) {
			if (serv.isRunning() && type.isInstance(serv)) {
				return type.cast(serv);
			}
		}
		return null;
	}

	/**
	 * Replies the logger used by the kernel.
	 *
	 * @return the logger of the kernel.
	 */
	public Logger getLogger() {
		return this.loggingService.getKernelLogger();
	}

	/**
	 * Change the Janus context of the kernel.
	 *
	 * @param janusContext the new janus kernel. It must be never <code>null</code>.
	 */
	@Inject
	void setJanusContext(@io.janusproject.kernel.annotations.Kernel AgentContext janusContext) {
		assert janusContext != null;
		this.janusContext = janusContext;
	}

	/**
	 * Replies the Janus context of the kernel.
	 *
	 * @return the Janus root context. It must be never <code>null</code>.
	 * @since 2.0.7.0
	 */
	public AgentContext getJanusContext() {
		assert this.janusContext != null;
		return this.janusContext;
	}

	/**
	 * Listener on platform events.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class KernelStoppingListener implements KernelAgentSpawnListener {

		/**
		 * Construct.
		 */
		KernelStoppingListener() {
			//
		}

		@Override
		public void kernelAgentSpawn() {
			//
		}

		@Override
		public void kernelAgentDestroy() {
			// CAUTION: EXECUTE THE STOP FUNCTION IN A THREAD THAT
			// IS INDEPENDENT TO THE ONES FROM THE EXECUTORS
			// CREATED BY THE EXECUTORSERVICE.
			// THIS AVOID THE STOP FUNCTION TO BE INTERRUPTED
			// BECAUSE THE EXECUTORSERVICE WAS SHUTTED DOWN.
			final StopTheKernel t = new StopTheKernel();
			t.startAsync();
		}
	}

	/**
	 * Runner for stopping the kernel.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class StopTheKernel implements ThreadFactory, Runnable, UncaughtExceptionHandler {

		/**
		 * Construct.
		 */
		StopTheKernel() {
			//
		}

		/**
		 * Start the thread.
		 * The thread invokes the {@link #run()} function asynchronously.
		 *
		 * @return the thread.
		 */
		public Thread startAsync() {
			final Thread t = newThread(this);
			t.start();
			return t;
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void run() {
			final Logger logger = getLogger();
			logger.info(Messages.Kernel_0);
			Services.stopServices(Kernel.this.serviceManager);
			logger.info(Messages.Kernel_1);
			Kernel.this.isRunning.set(false);
		}

		@Override
		public Thread newThread(Runnable runnable) {
			final Thread t = Executors.defaultThreadFactory().newThread(runnable);
			t.setName("Janus kernel shutdown"); //$NON-NLS-1$
			t.setDaemon(false);
			t.setUncaughtExceptionHandler(this);
			return t;
		}

		@Override
		public void uncaughtException(Thread thread, Throwable exception) {
			assert thread != null;
			assert exception != null;
			final LogRecord record = new LogRecord(Level.SEVERE, exception.getLocalizedMessage());
			record.setThrown(exception);
			final StackTraceElement elt = exception.getStackTrace()[0];
			assert elt != null;
			record.setSourceClassName(elt.getClassName());
			record.setSourceMethodName(elt.getMethodName());
			final Logger logger = getLogger();
			logger.log(record);
		}

	}

}
