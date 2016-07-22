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

package io.janusproject.kernel;

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.google.common.util.concurrent.Service;
import com.google.inject.Guice;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Singleton;
import io.janusproject.services.IServiceManager;
import io.janusproject.services.Services;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.spawn.KernelAgentSpawnListener;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.util.LoggerCreator;
import io.janusproject.util.TwoStepConstruction;
import org.arakhne.afc.vmutil.locale.Locale;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;

/**
 * This class represents the Kernel of the Janus platform.
 *
 * <p>
 * <strong>The Kernel is a singleton.</strong>
 *
 * <p>
 * The Kernel is assimilated to an agent that is omniscient and distributed other the network. It is containing all the other
 * agents.
 *
 * <p>
 * To create a Kernel, you should use the function {@link #create(Module...)}.
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

	/**
	 * Logger used by the kernel, but not linked to the logging kernel's service.
	 */
	private Logger rawLogger;

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
	 * @param modules - modules to link to the new kernel.
	 * @return the new kernel.
	 */
	public static final Kernel create(Module... modules) {
		Injector injector = Guice.createInjector(modules);
		Kernel k = injector.getInstance(Kernel.class);
		return k;
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
	 * Spawn an agent of the given type, and pass the parameters to its initialization function.
	 *
	 * @param agent - the type of the agent to spawn.
	 * @param params - the list of the parameters to pass to the agent initialization function.
	 * @return the identifier of the agent, never <code>null</code>.
	 */
	public UUID spawn(Class<? extends Agent> agent, Object... params) {
		return this.spawnService.spawn(this.janusContext, null, agent, params);
	}

	/**
	 * Spawn an agent of the given type, and pass the parameters to its initialization function.
	 *
	 * @param agentID - the identifier of the agent to spawn. If <code>null</code> the identifier is randomly selected.
	 * @param agent - the type of the agent to spawn.
	 * @param params - the list of the parameters to pass to the agent initialization function.
	 * @return the identifier of the agent, never <code>null</code>.
	 */
	public UUID spawn(UUID agentID, Class<? extends Agent> agent, Object... params) {
		return this.spawnService.spawn(this.janusContext, agentID, agent, params);
	}

	/**
	 * Replies a kernel service that is alive.
	 *
	 * @param <S> - type of the type to reply.
	 * @param type - type of the type to reply.
	 * @return the service, or <code>null</code>.
	 */
	public <S extends Service> S getService(Class<S> type) {
		for (Service serv : this.serviceManager.servicesByState().values()) {
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
		Logger log = this.loggingService.getLogger();
		if (log == null) {
			if (this.rawLogger == null) {
				this.rawLogger = LoggerCreator.createLogger(Kernel.class.getName());
			}
			log = this.rawLogger;
		}
		return log;
	}

	/**
	 * Change the Janus context of the kernel.
	 *
	 * @param janusContext - the new janus kernel. It must be never <code>null</code>.
	 */
	@Inject
	void setJanusContext(@io.janusproject.kernel.annotations.Kernel AgentContext janusContext) {
		assert (janusContext != null);
		this.janusContext = janusContext;
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
			StopTheKernel t = new StopTheKernel();
			t.start();
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
		 */
		public void start() {
			Thread t = newThread(this);
			t.start();
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void run() {
			Logger logger = getLogger();
			logger.info(Locale.getString(Kernel.class, "STOP_KERNEL_SERVICES")); //$NON-NLS-1$
			Services.stopServices(Kernel.this.serviceManager);
			logger.info(Locale.getString(Kernel.class, "KERNEL_SERVICES_STOPPED")); //$NON-NLS-1$
			Kernel.this.isRunning.set(false);
		}

		@Override
		public Thread newThread(Runnable runnable) {
			Thread t = Executors.defaultThreadFactory().newThread(runnable);
			t.setName("Janus kernel shutdown"); //$NON-NLS-1$
			t.setDaemon(false);
			t.setUncaughtExceptionHandler(this);
			return t;
		}

		@Override
		public void uncaughtException(Thread thread, Throwable exception) {
			assert (thread != null);
			assert (exception != null);
			LogRecord record = new LogRecord(Level.SEVERE, exception.getLocalizedMessage());
			record.setThrown(exception);
			StackTraceElement elt = exception.getStackTrace()[0];
			assert (elt != null);
			record.setSourceClassName(elt.getClassName());
			record.setSourceMethodName(elt.getMethodName());
			Logger logger = getLogger();
			logger.log(record);
		}

	}

}
