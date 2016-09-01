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

import static org.junit.Assert.assertEquals;
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
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeoutException;

import com.google.inject.Module;
import com.google.inject.util.Modules;
import io.janusproject.Boot;
import io.janusproject.kernel.Kernel;
import io.janusproject.modules.StandardJanusPlatformModule;
import org.eclipse.jdt.annotation.Nullable;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Rule;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

import io.sarl.core.Initialize;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;

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

	/**
	 * Reference to the instance of the Janus kernel.
	 */
	protected Kernel janusKernel;

	@Nullable
	private List<Object> results;

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

	/**
	 * Replies the number of results provided by the ran platform.
	 *
	 * @return the number of results.
	 */
	protected int getNumberOfResults() {
		return this.results.size();
	}

	/**
	 * Test if the number of results provided by the Janus platform is equal to the given number.
	 *
	 * @param expected - the expected number of results.
	 */
	protected void assertNumberOfResults(int expected) {
		assertEquals("Invalid number of results provided by the platform.", expected, this.results.size());
	}

	/**
	 * Replies result at the given index of the run of the agent.
	 * 
	 * @param type - the type of the result.
	 * @param index - the index of the result.
	 * @return the value; or <code>null</code> if no result.
	 */
	protected <T> T getResult(Class<T> type, int index) {
		if (this.results != null) {
			try {
				return type.cast(this.results.get(index));
			} catch (Throwable exception) {
				//
			}
		}
		return null;
	}

	/**
	 * Replies the index of the first result of the given type.
	 * 
	 * @param type - the type of the result.
	 * @return the index; or <code>-1</code> if not found.
	 */
	protected int indexOfResult(Class<?> type) {
		return indexOfResult(type, 0);
	}

	/**
	 * Replies the index of the first result of the given type starting at the given index.
	 * 
	 * @param type - the type of the result.
	 * @param fromIndex - the start index.
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
	 * @param type - the type of the agent to launch at start-up.
	 * @param enableLogging - indicates if the logging is enable or not.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void runJanus(Class<? extends TestingAgent> type, boolean enableLogging) throws Exception {
		runJanus(type, enableLogging, true, -1);
	}

	/**
	 * Start the Janus platform offline with logging enabled.
	 *
	 * This function enables logging and has no timeout for the end of the run.
	 * 
	 * @param type - the type of the agent to launch at start-up.
	 * @param enableLogging - indicates if the logging is enable or not.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void runJanus(Class<? extends TestingAgent> type) throws Exception {
		runJanus(type, true, true, -1);
	}

	/**
	 * Start the Janus platform.
	 * 
	 * @param type - the type of the agent to launch at start-up.
	 * @param enableLogging - indicates if the logging is enable or not.
	 * @param offline - indicates if the Janus platform is offline
	 * @param timeout - the maximum waiting time in seconds, or <code>-1</code> to ignore the timeout.
	 * @throws Exception - if the kernel cannot be launched.
	 */
	protected void runJanus(Class<? extends TestingAgent> type, boolean enableLogging, boolean offline, int timeout)
			throws Exception {
		assertNull("Janus already launched.", this.janusKernel);
		Module module = new StandardJanusPlatformModule();
		Boot.setConsoleLogger(new PrintStream(new OutputStream() {
			@Override
			public void write(int b) throws IOException {
				//
			}
		}));
		this.results = new ArrayList<>();
		if (!enableLogging) {
			module = Modules.override(new StandardJanusPlatformModule()).with(new NoLogTestingModule());
		} else {
			module = Modules.override(new StandardJanusPlatformModule()).with(new ErrorLogTestingModule(this.results));
		}
		Boot.setOffline(offline);
		this.janusKernel = Boot.startJanus(module, type, results);
		long endTime;
		if (timeout >= 0) {
			endTime = System.currentTimeMillis() + timeout * 1000;
		} else {
			endTime = -1;
		}
		while (this.janusKernel.isRunning() && (endTime == -1 || System.currentTimeMillis() <= endTime)) {
			Thread.yield();
		}
		boolean isTimedOut = this.janusKernel.isRunning();
		Boot.setConsoleLogger(null);
		if (isTimedOut) {
			throw new TimeoutException();
		}
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

		private List<Object> results;

		/**
		 * @param provider - the provider of builtin capacities.
		 * @param parentID - the identifier of the parent's agent.
		 * @param agentID - the identifier of the agent.
		 */
		public TestingAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
			super(provider, parentID, agentID);
		}

		/**
		 * Add a result.
		 * 
		 * @param result - the result.
		 */
		protected void addResult(Object result) {
			this.results.add(result);
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
		 * @param occurrence - the initialization event.
		 */
		private void $behaviorUnit$Initialize$0(final Initialize occurrence) {
			this.results = (List<Object>) occurrence.parameters[0];
			if (runAgentTest()) {
				getSkill(Schedules.class).in(1000, new Procedure1<Agent>() {
					@Override
					public void apply(Agent it) {
						getSkill(Lifecycle.class).killMe();
					}
				});
			}
		}

		/**
		 * Invoked to run the unit test.
		 *
		 * @return <code>true</code> for killing the agent 1 second after its initialization.
		 */
		protected abstract boolean runAgentTest();

	}

}
