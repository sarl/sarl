/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.janusproject.tests.testutils;

import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.inject.Module;
import com.google.inject.util.Modules;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import io.janusproject.Boot;
import io.janusproject.Bootstrap;
import io.janusproject.kernel.Kernel;
import io.janusproject.modules.StandardJanusPlatformModule;
import io.janusproject.services.executor.EarlyExitException;

import io.sarl.core.Initialize;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.tests.api.Nullable;

/**
 * Abstract class for creating unit tests that needs to launch a Janus instance.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractJanusRunTest extends AbstractJanusTest {

	/** Short timeout in seconds.
	 *
	 * @see #STANDARD_TIMEOUT
	 * @see #EXTRA_TIMEOUT
	 * @see #NO_TIMEOUT
	 */
	public static final int SHORT_TIMEOUT = 10;

	/** Standard timeout in seconds.
	 *
	 * @see #EXTRA_TIMEOUT
	 * @see #NO_TIMEOUT
	 * @see #SHORT_TIMEOUT
	 */
	public static final int STANDARD_TIMEOUT = 40;
	
	/** Extra timeout in seconds.
	 *
	 * @see #STANDARD_TIMEOUT
	 * @see #NO_TIMEOUT
	 * @see #SHORT_TIMEOUT
	 */
	public static final int EXTRA_TIMEOUT = 240;

	/** No timeout.
	 *
	 * @see #STANDARD_TIMEOUT
	 * @see #EXTRA_TIMEOUT
	 * @see #SHORT_TIMEOUT
	 */
	public static final int NO_TIMEOUT = -1;

	/**
	 * Reference to the instance of the Janus kernel.
	 */
	protected Kernel janusKernel;

	@Nullable
	private Map<UUID, List<Object>> results;

	@Nullable
	private UUID bootAgent;

	@Rule
	public TestWatcher janusRunWatcher = new TestWatcher() {
		@Override
		protected void starting(Description description) {
			JanusRun skipRun = description.getAnnotation(JanusRun.class);
			if (skipRun != null) {
				try {
					runJanus(skipRun.agent(), skipRun.enableLogging());
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}

		@Override
		protected void finished(Description description) {
			if (AbstractJanusRunTest.this.janusKernel != null) {
				AbstractJanusRunTest.this.janusKernel = null;
			}
		}
	};

	/** Replies the identifier of the lastly booted agent.
	 *
	 * @return the identifier or {@code null} if no agent was booted.
	 */
	protected UUID getBootAgent() {
		return this.bootAgent;
	}

	/**
	 * Replies the number of results provided by the ran platform.
	 *
	 * @return the number of results.
	 */
	protected int getNumberOfResults() {
		return getNumberOfResults(getBootAgent());
	}

	/**
	 * Replies the number of results provided by the ran platform.
	 *
	 * @param agentID the identifier of the agent that provides the results.
	 * @return the number of results.
	 * @since 0.8
	 */
	protected int getNumberOfResults(UUID agentID) {
		if (this.results != null) {
			final List<Object> res = this.results.get(agentID);
			if (res != null) {
				return res.size();
			}
		}
		return 0;
	}

	/**
	 * Test if the number of results provided by the Janus platform is equal to the given number.
	 *
	 * @param expected the expected number of results.
	 */
	protected void assertNumberOfResults(int expected) {
		assertEquals("Invalid number of results provided by the platform.", expected, this.results.size());
	}

	/**
	 * Replies result at the given index of the run of the agent.
	 * 
	 * @param type the type of the result.
	 * @param index the index of the result.
	 * @return the value; or <code>null</code> if no result.
	 */
	protected <T> T getResult(Class<T> type, int index) {
		return getResult(getBootAgent(), type, index);
	}

	/**
	 * Replies result at the given index of the run of the agent.
	 * 
	 * @param agentId the identifier of the agent that provides the result.
	 * @param type the type of the result.
	 * @param index the index of the result.
	 * @return the value; or <code>null</code> if no result.
	 * @since 0.8
	 */
	protected <T> T getResult(UUID agentId, Class<T> type, int index) {
		if (this.results != null) {
			final List<Object> res = this.results.get(agentId);
			if (res != null) {
				try {
					return type.cast(res.get(index));
				} catch (Throwable exception) {
					//
				}
			}
		}
		return null;
	}

	/**
	 * Replies result at the given index of the run of the agent.
	 * @return the results.
	 */
	protected List<Object> getResults() {
		return getResults(getBootAgent());
	}

	/**
	 * Replies result at the given index of the run of the agent.
	 * @return the results.
	 * @since 0.8
	 */
	protected Map<UUID, List<Object>> getAllResults() {
		return this.results == null ? Collections.emptyMap() : Collections.unmodifiableMap(this.results);
	}

	/**
	 * Replies result at the given index of the run of the agent.
	 * @return the results.
	 * @since 0.8
	 */
	protected List<Object> getResults(UUID agentID) {
		if (this.results != null && agentID != null) {
			final List<Object> res = this.results.get(agentID);
			if (res != null) {
				return Collections.unmodifiableList(res);
			}
		}
		return Collections.emptyList();
	}

	/**
	 * Replies the initialization parameters for the agents.
	 * @return the parameters.
	 */
	protected Object[] getAgentInitializationParameters() {
		return new Object[] {
				this.results,
		};
	}

	/**
	 * Replies the index of the first result of the given type.
	 * 
	 * @param type the type of the result.
	 * @return the index; or <code>-1</code> if not found.
	 */
	protected int indexOfResult(Class<?> type) {
		return indexOfResult(type, 0);
	}

	/**
	 * Replies the index of the first result of the given type starting at the given index.
	 * 
	 * @param type the type of the result.
	 * @param fromIndex the start index.
	 * @return the index; or <code>-1</code> if not found.
	 */
	protected int indexOfResult(Class<?> type, int fromIndex) {
		if (this.results != null) {
			try {
				for (int i = fromIndex; i < this.results.size(); ++i) {
					Object r = this.results.get(i);
					if (type.isInstance(r)) {
						return i;
					}
				}
			} catch (Throwable exception) {
				//
			}
		}
		return -1;
	}

	/**
	 * Start the Janus platform offline.
	 *
	 * This function has no timeout for the end of the run.
	 *
	 * @param type the type of the agent to launch at start-up.
	 * @param enableLogging indicates if the logging is enable or not.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void runJanus(Class<? extends TestingAgent> type, boolean enableLogging) throws Exception {
		runJanus(type, enableLogging, true, NO_TIMEOUT);
	}

	/**
	 * Start the Janus platform offline with logging enabled.
	 *
	 * This function enables logging and has no timeout for the end of the run.
	 * 
	 * @param type the type of the agent to launch at start-up.
	 * @return the kernel.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void runJanus(Class<? extends TestingAgent> type) throws Exception {
		runJanus(type, true, true, NO_TIMEOUT);
	}

	/**
	 * Start the Janus platform.
	 * 
	 * @param type the type of the agent to launch at start-up.
	 * @param enableLogging indicates if the logging is enable or not.
	 * @param offline indicates if the Janus platform is offline
	 * @param timeout the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
	 *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void runJanus(Class<? extends TestingAgent> type, boolean enableLogging, boolean offline, int timeout)
			throws Exception {
		runJanus(type, enableLogging, offline, timeout, getDefaultJanusModule());
	}

	/** Replies the injection module to be used for running the Janus platform.
	 *
	 * @return the module.
	 * @since 0.8
	 */
	protected Module getDefaultJanusModule() {
		return new StandardJanusPlatformModule();
	}

	/**
	 * Start the Janus platform.
	 * 
	 * @param type the type of the agent to launch at start-up.
	 * @param enableLogging indicates if the logging is enable or not.
	 * @param offline indicates if the Janus platform is offline
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void startJanusWithDefaultProcess(Class<? extends TestingAgent> type, boolean enableLogging, boolean offline,
			Module module)
			throws Exception {
		final Module injectionModule = prepareJanus(enableLogging, offline, module);
		startJanusKernel(injectionModule, type);
		postKernelLaunch(enableLogging, Boot.getBootAgentIdentifier());
	}

	/**
	 * Start the Janus platform.
	 * 
	 * @param type the type of the agent to launch at start-up.
	 * @param enableLogging indicates if the logging is enable or not.
	 * @param offline indicates if the Janus platform is offline
	 * @param timeout the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
	 *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void runJanus(Class<? extends TestingAgent> type, boolean enableLogging, boolean offline, int timeout,
			Module module)
			throws Exception {
		startJanusWithDefaultProcess(type, enableLogging, offline, module);
		waitForTheKernel(timeout);
	}

	/**
	 * Forget any reference to a Janus kernel.
	 */
	protected void forgetTheKernel() {
		this.janusKernel = null;
	}
	
	/**
	 * Prepare the Janus platform for launching
	 * 
	 * @param enableLogging indicates if the logging is enable or not.
	 * @param offline indicates if the Janus platform is offline
	 * @param module the injection module to be used.
	 * @return the injection module
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected Module prepareJanus(boolean enableLogging, boolean offline, Module module) throws Exception {
		PrintStream cons = new PrintStream(new OutputStream() {
			@Override
			public void write(int b) throws IOException {
				//
			}
			@Override
			public void write(byte[] b) throws IOException {
				//
			}
			@Override
			public void write(byte[] b, int off, int len) throws IOException {
				//
			}
		});
		Boot.setErrorConsoleLogger(cons);
		Boot.setStandardConsoleLogger(cons);
		this.results = new TreeMap<>();
		Module injectionModule = module;
		if (!enableLogging) {
			injectionModule = Modules.override(injectionModule).with(new NoLogTestingModule());
		} else {
			injectionModule = Modules.override(injectionModule).with(new ErrorLogTestingModule(this.results));
		}
		Boot.setOffline(offline);
		return injectionModule;
	}
	
	/**
	 * Start the Janus platform.
	 * 
	 * @param module the injection module to be used.
	 * @param type the type of the agent to launch at start-up.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void startJanusKernel(Module injectionModule, Class<? extends TestingAgent> type) throws Exception {
		assertNull("Janus already launched.", this.janusKernel);
		this.janusKernel = Boot.startJanusWithModule(injectionModule, type, getAgentInitializationParameters());
	}
	
	/**
	 * Start the Janus platform.
	 * 
	 * @param module the injection module to be used.
	 * @param type the type of the agent to launch at start-up.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void startJanusKernel(Module injectionModule) throws Exception {
		assertNull("Janus already launched.", this.janusKernel);
		this.janusKernel = Boot.startWithoutAgent(injectionModule);
	}

	/**
	 * Post starting stage.
	 * 
	 * @param enableLogging indicates if the logging is enable or not.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void postKernelLaunch(boolean enableLogging, UUID bootAgent) {
		this.bootAgent = bootAgent;
		Logger current = this.janusKernel.getLogger();
		while (current.getParent() != null && current.getParent() != current) {
			current = current.getParent();
		}
		if (current != null) {
			current.setLevel(enableLogging ? Level.SEVERE : Level.OFF);
		}
	}
	
	/**
	 * Wait for the end of the Janus platform.
	 * 
	 * @param timeout the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
	 *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	public void waitForTheKernel(int timeout) throws Exception {
		long endTime;
		if (timeout >= 0) {
			endTime = System.currentTimeMillis() + timeout * 1000;
		} else {
			endTime = -1;
		}
		boolean isJanusRunning = this.janusKernel.isRunning();
		while (isJanusRunning && (endTime == -1 || System.currentTimeMillis() <= endTime)) {
			isJanusRunning = this.janusKernel.isRunning();
			Thread.yield();
		}
		Boot.setErrorConsoleLogger(null);
		Boot.setStandardConsoleLogger(null);
		if (isJanusRunning) {
			throw new TimeoutException();
		}
	}

	/**
	 * Wait for the end of the Janus platform.
	 * 
	 * @param timeout the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
	 *     See {@link #STANDARD_TIMEOUT}, {@link #EXTRA_TIMEOUT} or {@link #NO_TIMEOUT}.
	 * @param predicate the predicate to use as stop condition.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	public void waitForTheKernel(int timeout, Function1<Map<UUID, List<Object>>, Boolean> predicate) throws Exception {
		long endTime;
		if (timeout >= 0) {
			endTime = System.currentTimeMillis() + timeout * 1000;
		} else {
			endTime = -1;
		}
		boolean isJanusRunning = this.janusKernel.isRunning();
		while (isJanusRunning && (endTime == -1 || System.currentTimeMillis() <= endTime)) {
			isJanusRunning = this.janusKernel.isRunning() || !(predicate.apply(this.results));
			Thread.yield();
		}
		Boot.setErrorConsoleLogger(null);
		Boot.setStandardConsoleLogger(null);
		if (isJanusRunning) {
			throw new TimeoutException();
		}
	}

	protected Bootstrap getTestingBootstrap() {
		Bootstrap b = new Bootstrap();
		b.setKernel(getTestingKernel());
		return b;
	}
	
	protected final Kernel getTestingKernel() {
		return this.janusKernel;
	}

	/**
	 * Interface that permits to mark a method that is manually launching the Janus.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Documented
	@Retention(RetentionPolicy.RUNTIME)
	@Target(ElementType.TYPE)
	protected @interface JanusRun {

		/**
		 * The type of the agent to launch.
		 *
		 * @return the type of the agent to launch.
		 */
		Class<? extends TestingAgent> agent();

		/**
		 * Indicates if the logging is enabled.
		 *
		 * @return <code>true</code> if the logging is enabled; <code>false</code> otherwise.
		 */
		boolean enableLogging() default false;

	}

	/**
	 * Abstract implementation of an agent that is used for testing Janus
	 * 
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static abstract class TestingAgent extends Agent {

		private Map<UUID, List<Object>> results;

		private Object[] initializationParameters;

		/**
		 * @param provider the provider of builtin capacities.
		 * @param parentID the identifier of the parent's agent.
		 * @param agentID the identifier of the agent.
		 */
		public TestingAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
			super(provider, parentID, agentID);
		}

		/**
		 * @param parentID the identifier of the parent's agent.
		 * @param agentID the identifier of the agent.
		 */
		public TestingAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		/**
		 * Add a result.
		 * 
		 * @param result the result.
		 */
		protected synchronized void addResult(Object result) {
			List<Object> res = this.results.get(getID());
			if (res == null) {
				res = new ArrayList<>();
				this.results.put(getID(), res);
			}
			res.add(result);
		}
		
		/**
		 * Replies the number of results provided by the ran platform.
		 *
		 * @return the number of results.
		 */
		protected int getNumberOfResults() {
			final List<Object> res = this.results.get(getID());
			return res == null ? 0 : res.size();
		}

		/**
		 * Add a result.
		 * 
		 * @param result the result.
		 */
		protected synchronized void addResults(Collection<?> results) {
			List<Object> res = this.results.get(getID());
			if (res == null) {
				res = new ArrayList<>();
				this.results.put(getID(), res);
			}
			res.addAll(results);
		}

		/**
		 * Replies result at the given index of the run of the agent.
		 * @return the results.
		 */
		protected synchronized List<Object> getResults() {
			if (this.results != null) {
				final List<Object> res = this.results.get(getID());
				if (res != null) {
					return Collections.unmodifiableList(res);
				}
			}
			return Collections.emptyList();
		}

		/** Replies the initialization parameters of the agents.
		 *
		 * @return the initialization parameters.
		 */
		protected Object[] getAgentInitializationParameters() {
			return this.initializationParameters;
		}

		@PerceptGuardEvaluator
		private void $guardEvaluator$Initialize(final Initialize occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {
			assert occurrence != null;
			assert ___SARLlocal_runnableCollection != null;
			___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Initialize$0(occurrence));
		}

		/**
		 * Invoked at the start of the agent.
		 * 
		 * @param occurrence the initialization event.
		 */
		private void $behaviorUnit$Initialize$0(final Initialize occurrence) {
			this.initializationParameters = occurrence.parameters;
			this.results = (Map<UUID, List<Object>>) occurrence.parameters[0];
			try {
				if (runAgentTest()) {
					getSkill(Schedules.class).in(1000, (it) -> forceKillMe());
				}
			} catch (Throwable exception) {
				if (!(exception instanceof EarlyExitException)) {
					addResult(exception);
				}
				throw exception;
			}
		}

		protected void forceKillMe() {
			getSkill(Lifecycle.class).killMe();
		}

		/**
		 * Invoked to run the unit test. This function is invoked at agent initialization
		 *
		 * @return <code>true</code> for killing the agent during its initialization.
		 */
		protected abstract boolean runAgentTest();

	}

}
