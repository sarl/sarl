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
package io.janusproject.tests.kernel.bic.internaleventdispatching;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

import javax.inject.Inject;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;

import io.janusproject.kernel.bic.internaleventdispatching.AgentInternalEventsDispatcher;
import io.janusproject.kernel.services.jdk.executors.JdkExecutorService;
import io.janusproject.services.executor.ExecutorService;

import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Event;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
		AgentInternalEventsDispatcherTest.StandardTests.class,
		AgentInternalEventsDispatcherTest.RuntimeTests.class,
})
@SuppressWarnings("all")
public class AgentInternalEventsDispatcherTest {

	public static class StandardTests extends AbstractSarlTest {

		@Nullable
		private AgentInternalEventsDispatcher dispatcher;
	
		@Mock
		private ExecutorService executorService;
	
		@Before
		public void setUp() {
			Mockito.doAnswer((it) -> {
				((Runnable) it.getArgument(0)).run();
				return null;
			}).when(this.executorService).execute(Mockito.any(Runnable.class));
			this.dispatcher = new AgentInternalEventsDispatcher(this.executorService);
		}
	
		@Test
		public void immediateDispatch_noRegister_event() {
			this.dispatcher.immediateDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_noRegister_myEvent() {
			this.dispatcher.immediateDispatch(new MyEvent(0));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_event() {
			this.dispatcher.register(new MyAgent());
			this.dispatcher.immediateDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_myEvent_n0() {
			this.dispatcher.register(new MyAgent());
			this.dispatcher.immediateDispatch(new MyEvent(0));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_myEvent_n1() {
			this.dispatcher.register(new MyAgent());
			this.dispatcher.immediateDispatch(new MyEvent(1));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_event_withValidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> true);
			this.dispatcher.immediateDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_myEvent_n0_withValidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> true);
			this.dispatcher.immediateDispatch(new MyEvent(0));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_myEvent_n1_withValidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> true);
			this.dispatcher.immediateDispatch(new MyEvent(1));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_event_withInvalidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> false);
			this.dispatcher.immediateDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_myEvent_n0_withInvalidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> false);
			this.dispatcher.immediateDispatch(new MyEvent(0));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
		}
	
		@Test
		public void immediateDispatch_register_myEvent_n1_withInvalidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> false);
			this.dispatcher.immediateDispatch(new MyEvent(1));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_noRegister_event() {
			this.dispatcher.asyncDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_noRegister_myEvent() {
			this.dispatcher.asyncDispatch(new MyEvent(0));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.only()).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_event() {
			this.dispatcher.register(new MyAgent());
			this.dispatcher.asyncDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_myEvent_n0() {
			this.dispatcher.register(new MyAgent());
			this.dispatcher.asyncDispatch(new MyEvent(0));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_myEvent_n1() {
			this.dispatcher.register(new MyAgent());
			this.dispatcher.asyncDispatch(new MyEvent(1));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(3)).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_event_withValidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> true);
			this.dispatcher.asyncDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_myEvent_n0_withValidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> true);
			this.dispatcher.asyncDispatch(new MyEvent(0));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(2)).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_myEvent_n1_withValidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> true);
			this.dispatcher.asyncDispatch(new MyEvent(1));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(3)).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_event_withInvalidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> false);
			this.dispatcher.asyncDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(1)).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_myEvent_n0_withInvalidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> false);
			this.dispatcher.asyncDispatch(new MyEvent(0));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(1)).execute(argument.capture());
		}
	
		@Test
		public void asyncDispatch_register_myEvent_n1_withInvalidFilter() {
			this.dispatcher.register(new MyAgent(), (event) -> false);
			this.dispatcher.asyncDispatch(new MyEvent(1));
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.times(1)).execute(argument.capture());
		}
	
		@Test
		public void unregister() {
			Object ag = new MyAgent();
			this.dispatcher.register(ag);
			this.dispatcher.unregister(ag);
			this.dispatcher.immediateDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
		}
	
		@Test
		public void unregisterAll() {
			this.dispatcher.register(new MyAgent());
			this.dispatcher.register(new MyAgent2());
			this.dispatcher.register(new MyAgent2());
			this.dispatcher.unregisterAll();
			this.dispatcher.immediateDispatch(new Event() { });
			ArgumentCaptor<Runnable> argument = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, Mockito.never()).execute(argument.capture());
		}
	
		public static class MyEvent extends Event {
			public final int n;
			public MyEvent(int n) {
				this.n = n;
			}
		}
	
		public static class MyAgent extends Agent {
	
			public MyAgent() {
				super(null, UUID.randomUUID(), UUID.randomUUID());
			}
	
			@PerceptGuardEvaluator
			private void $perception$guard$evaluator1(Event event, Collection<Runnable> runners) {
				runners.add(() -> $perception$guard$callback1(event, event));
			}
	
			private void $perception$guard$callback1(Event occurrence, Event it) {
			}
	
			@PerceptGuardEvaluator
			private void $perception$guard$evaluator2(MyEvent event, Collection<Runnable> runners) {
				if (event.n > 0) {
					runners.add(() -> $perception$guard$callback2(event, event));
				}
			}
	
			private void $perception$guard$callback2(MyEvent occurrence, MyEvent it) {
			}
	
		}
	
		public static class MyAgent2 extends Agent {
	
			public MyAgent2() {
				super(null, UUID.randomUUID(), UUID.randomUUID());
			}
	
			@PerceptGuardEvaluator
			private void $perception$guard$evaluator1(Event event, Collection<Runnable> runners) {
				runners.add(() -> $perception$guard$callback1(event, event));
			}
	
			private void $perception$guard$callback1(Event occurrence, Event it) {
			}
	
			@PerceptGuardEvaluator
			private void $perception$guard$evaluator2(MyEvent event, Collection<Runnable> runners) {
				runners.add(() -> $perception$guard$callback2(event, event));
			}
	
			private void $perception$guard$callback2(MyEvent occurrence, MyEvent it) {
			}
	
		}
	
	}

	public static class RuntimeTests extends AbstractSarlTest {

		@Inject
		private ReflectExtensions reflect;
		
		@Nullable
		private AgentInternalEventsDispatcher dispatcher;

		@Nullable
		private java.util.concurrent.ExecutorService jvmExecutorService;

		@Nullable
		private ScheduledExecutorService jvmScheduledExecutorService;

		@Nullable
		private JdkExecutorService executorService;
	
		@Before
		public void setUp() throws Exception {
			this.executorService = new JdkExecutorService();
			this.jvmExecutorService = Executors.newFixedThreadPool(5);
			this.jvmScheduledExecutorService = Executors.newScheduledThreadPool(1);
			this.reflect.invoke(this.executorService, "setExecutorService", this.jvmExecutorService);
			this.reflect.invoke(this.executorService, "setScheduledExecutorService", this.jvmScheduledExecutorService);
			this.executorService.startAsync();
			this.executorService.awaitRunning();
			this.dispatcher = new AgentInternalEventsDispatcher(this.executorService);
		}

		@After
		public void tearDown() throws Exception {
			this.executorService.stopAsync();
			this.executorService.awaitTerminated();
			this.jvmExecutorService.shutdownNow();
			this.jvmScheduledExecutorService.shutdownNow();
		}

		@Test
		public void async_withValidGuard_withValidFilter() throws Exception {
			MyAgent agent = new MyAgent();
			this.dispatcher.register(agent, (event) -> true);
			//
			MyEvent event = new MyEvent(true);
			this.dispatcher.asyncDispatch(event);
			//
			Thread.sleep(1000); // Wait for handler execution.
			assertContains(agent.events, event);
			assertContains(agent.myEvents, event);
		}

		@Test
		public void async_withValidGuard_withInvalidFilter() throws Exception {
			MyAgent agent = new MyAgent();
			this.dispatcher.register(agent, (event) -> false);
			//
			MyEvent event = new MyEvent(true);
			this.dispatcher.asyncDispatch(event);
			//
			Thread.sleep(1000); // Wait for handler execution.
			assertTrue(agent.events.isEmpty());
			assertTrue(agent.myEvents.isEmpty());
		}

		@Test
		public void async_withInvalidGuard_withValidFilter() throws Exception {
			MyAgent agent = new MyAgent();
			this.dispatcher.register(agent, (event) -> true);
			//
			MyEvent event = new MyEvent(false);
			this.dispatcher.asyncDispatch(event);
			//
			Thread.sleep(1000); // Wait for handler execution.
			assertContains(agent.events, event);
			assertTrue(agent.myEvents.isEmpty());
		}

		@Test
		public void async_withInvalidGuard_withInvalidFilter() throws Exception {
			MyAgent agent = new MyAgent();
			this.dispatcher.register(agent, (event) -> false);
			//
			MyEvent event = new MyEvent(false);
			this.dispatcher.asyncDispatch(event);
			//
			Thread.sleep(1000); // Wait for handler execution.
			assertTrue(agent.events.isEmpty());
			assertTrue(agent.myEvents.isEmpty());
		}

		@Test
		public void immediate_withValidGuard_withValidFilter() throws Exception {
			MyAgent agent = new MyAgent();
			this.dispatcher.register(agent, (event) -> true);
			//
			MyEvent event = new MyEvent(true);
			this.dispatcher.immediateDispatch(event);
			//
			assertContains(agent.events, event);
			assertContains(agent.myEvents, event);
		}

		@Test
		public void immediate_withValidGuard_withInvalidFilter() throws Exception {
			MyAgent agent = new MyAgent();
			this.dispatcher.register(agent, (event) -> false);
			//
			MyEvent event = new MyEvent(true);
			this.dispatcher.immediateDispatch(event);
			//
			assertTrue(agent.events.isEmpty());
			assertTrue(agent.myEvents.isEmpty());
		}

		@Test
		public void immediate_withInvalidGuard_withValidFilter() throws Exception {
			MyAgent agent = new MyAgent();
			this.dispatcher.register(agent, (event) -> true);
			//
			MyEvent event = new MyEvent(false);
			this.dispatcher.immediateDispatch(event);
			//
			assertContains(agent.events, event);
			assertTrue(agent.myEvents.isEmpty());
		}

		@Test
		public void immediate_withInvalidGuard_withInvalidFilter() throws Exception {
			MyAgent agent = new MyAgent();
			this.dispatcher.register(agent, (event) -> false);
			//
			MyEvent event = new MyEvent(false);
			this.dispatcher.immediateDispatch(event);
			//
			assertTrue(agent.events.isEmpty());
			assertTrue(agent.myEvents.isEmpty());
		}

		public static class MyEvent extends Event {
			public final boolean valid;
			public MyEvent(boolean valid) {
				this.valid = valid;
			}
		}

		public static class MyAgent extends Agent {

			public final List<MyEvent> myEvents = new ArrayList<>();

			public final List<Event> events = new ArrayList<>();

			public MyAgent() {
				super(null, UUID.randomUUID(), UUID.randomUUID());
			}
	
			@PerceptGuardEvaluator
			private void $perception$guard$evaluator1(Event event, Collection<Runnable> runners) {
				runners.add(() -> $perception$guard$callback1(event, event));
			}
	
			private void $perception$guard$callback1(Event occurrence, Event it) {
				this.events.add(it);
			}
	
			@PerceptGuardEvaluator
			private void $perception$guard$evaluator2(MyEvent event, Collection<Runnable> runners) {
				if (event.valid) {
					runners.add(() -> $perception$guard$callback2(event, event));
				}
			}
	
			private void $perception$guard$callback2(MyEvent occurrence, MyEvent it) {
				this.myEvents.add(it);
			}
	
		}

	}

}
