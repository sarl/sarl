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
package io.janusproject.testutils;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.junit.Rule;
import org.junit.Test;
import org.junit.internal.runners.statements.RunBefores;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.Statement;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.google.common.collect.Lists;
import com.google.common.util.concurrent.Service;
import com.google.common.util.concurrent.Service.Listener;
import com.google.common.util.concurrent.Service.State;

/**
 * Abstract class that permits to test the implementation of a service.
 *
 * @param <S> - the type of the service.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractServiceTest<S extends Service> extends AbstractJanusTest {

	@InjectMocks
	protected S service;

	/**
	 * This rule permits to start the service according to the annotation {@link StartServiceForTest}.
	 * 
	 */
	@Rule
	public TestWatcher serviceWatcher = new TestWatcher() {

		private Statement nextStatement(Statement n) {
			try {
				Field f = n.getClass().getDeclaredField("next");
				boolean isAcc = f.isAccessible();
				try {
					f.setAccessible(true);
					if (Statement.class.isAssignableFrom(f.getType())) {
						return Statement.class.cast(f.get(n));
					}
				} finally {
					f.setAccessible(isAcc);
				}
			} catch (Throwable exception) {
				//
			}
			return null;
		}

		@Override
		public Statement apply(Statement base, Description description) {
			// Search for the statement "RunBefores" that decribes the
			// @Before calls.
			Statement n = base;
			while (n != null && !(n instanceof RunBefores)) {
				n = nextStatement(n);
			}
			if (n instanceof RunBefores) {
				RunBefores rb = (RunBefores) n;
				try {
					// Insert a new Statement for creating/launching the service.
					// The insertion point is just before the @Before statement
					// since the statements are run from the end to the start
					// of the linked list.
					Field f = rb.getClass().getDeclaredField("befores");
					boolean isAcc = f.isAccessible();
					try {
						f.setAccessible(true);
						if (List.class.isAssignableFrom(f.getType())) {
							List<FrameworkMethod> befores = (List<FrameworkMethod>) f.get(n);
							boolean foundStart = false;
							boolean foundInit = false;
							Iterator<FrameworkMethod> iterator = befores.iterator();
							while (!foundStart && !foundInit && iterator.hasNext()) {
								FrameworkMethod fm = iterator.next();
								if ("startService".equals(fm.getMethod().getName())) {
									foundStart = true;
								}
								if ("createAndInitService".equals(fm.getMethod().getName())) {
									foundStart = true;
								}
							}
							StartServiceForTest startAnnot = description.getTestClass().getAnnotation(StartServiceForTest.class);
							AvoidServiceStartForTest avoidAnnot = description.getAnnotation(AvoidServiceStartForTest.class);
							List<FrameworkMethod> tmp = null;
							if (!foundInit && startAnnot != null && startAnnot.createAfterSetUp()) {
								Method createServiceMethod = AbstractServiceTest.class.getMethod("createAndInitService");
								FrameworkMethod fm = new FrameworkMethod(createServiceMethod);
								tmp = Lists.newArrayList(befores);
								tmp.add(fm);
							}
							if (!foundStart && avoidAnnot == null && startAnnot != null
									&& (tmp != null || startAnnot.startAfterSetUp())) {
								Method startServiceMethod = AbstractServiceTest.class.getMethod("startService");
								FrameworkMethod fm = new FrameworkMethod(startServiceMethod);
								if (tmp == null) {
									tmp = Lists.newArrayList(befores);
								}
								tmp.add(fm);
							}
							if (tmp != null) {
								f.set(n, Collections.unmodifiableList(tmp));
							}
						}
					} finally {
						f.setAccessible(isAcc);
					}
				} catch (Throwable e) {
					throw new Error(e);
				}
			}
			return super.apply(base, description);
		}

		@Override
		protected void starting(Description description) {
			// Create the instance of the service
			StartServiceForTest startAnnot = description.getTestClass().getAnnotation(StartServiceForTest.class);
			if (startAnnot == null || !startAnnot.createAfterSetUp()) {
				createAndInitService();

				// Start the service according to the @StartServiceForTest annotation
				AvoidServiceStartForTest avoidAnnot = description.getAnnotation(AvoidServiceStartForTest.class);
				if (avoidAnnot == null && startAnnot != null && !startAnnot.startAfterSetUp()) {
					startService();
				}
			}
		}

		@Override
		protected void finished(Description description) {
			// Stop the service
			if (AbstractServiceTest.this.service != null) {
				AbstractServiceTest.this.service.stopAsync();
				AbstractServiceTest.this.service = null;
			}
		}
	};

	/**
	 * Start the tested service.
	 * 
	 * This function should not be called directly. It is invoked by the watcher implemented in the {@link AbstractServiceTest}.
	 */
	public void startService() {
		AbstractServiceTest.this.service.startAsync();
		try {
			AbstractServiceTest.this.service.awaitRunning(2, TimeUnit.SECONDS);
		} catch (TimeoutException e) {
			throw new Error(e);
		}
	}

	/**
	 * Create and initialize the service
	 * 
	 * This function should not be called directly. It is invoked by the watcher implemented in the {@link AbstractServiceTest}.
	 */
	public void createAndInitService() {
		AbstractServiceTest.this.service = newService();
		MockitoAnnotations.initMocks(AbstractServiceTest.this);
	}

	/**
	 * Replies the instance of the service under test.
	 *
	 * @return the tested service.
	 */
	public abstract S newService();

	@Test
	@AvoidServiceStartForTest
	public void notificationOnStart() throws Exception {
		Listener listener = mock(Listener.class);
		this.service.addListener(listener, new SyncExecutor());
		this.service.startAsync();
		this.service.awaitRunning(10, TimeUnit.SECONDS);
		verify(listener, times(1)).starting();
		// verify(listener, times(1)).running();
	}

	@Test
	public void notificationOnStop() throws Exception {
		Listener listener = mock(Listener.class);
		this.service.addListener(listener, new SyncExecutor());
		this.service.stopAsync();
		this.service.awaitTerminated(10, TimeUnit.SECONDS);
		ArgumentCaptor<State> arg = ArgumentCaptor.forClass(State.class);
		verify(listener, times(1)).stopping(arg.capture());
		assertEquals(State.RUNNING, arg.getValue());
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static class SyncExecutor implements Executor {

		/**
		 */
		public SyncExecutor() {
			//
		}

		@Override
		public void execute(Runnable command) {
			command.run();
		}

	}

}
