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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util.tests.eventdispatching;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

import javax.inject.Inject;

import com.google.common.collect.Collections2;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;

import io.sarl.eventdispatching.BehaviorGuardEvaluator;
import io.sarl.eventdispatching.BehaviorGuardEvaluatorRegistry;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	BehaviorGuardEvaluatorRegistryTest.NotConcurrentRegistryTest.class,
	BehaviorGuardEvaluatorRegistryTest.ConcurrentRegistryTest.class,
})
@SuppressWarnings("all")
public class BehaviorGuardEvaluatorRegistryTest {

	public static abstract class AbstractBehaviorGuardEvaluatorRegistryTest extends AbstractSarlTest {

		@Inject
		private ReflectExtensions reflect;
	
		@Nullable
		private BehaviorGuardEvaluatorRegistry registry;
	
		@Nullable
		private MyAgent agent;
	
		protected abstract BehaviorGuardEvaluatorRegistry createRegistry();

		@Before
		public void setUp() throws Exception {
			this.agent = new MyAgent();
			this.registry = createRegistry();
		}

		private Collection<String> transform(Iterable<BehaviorGuardEvaluator> iterable) {
			final List<String> result = new ArrayList<>();
			for (final BehaviorGuardEvaluator evaluator : iterable) {
				result.add(evaluator.toString());
			}
			return result;
		}
	
		@Test
		public void getBehaviorGuardEvaluators_noRegister_event() {
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertFalse(evaluators.iterator().hasNext());
		}
	
		@Test
		public void getBehaviorGuardEvaluators_myEvent() {
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new MyEvent());
			assertFalse(evaluators.iterator().hasNext());
		}
	
		@Test
		public void getBehaviorGuardEvaluators_register_event() {
			this.registry.register(this.agent);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertContains(transform(evaluators), "$perception$guard$evaluator1");
		}

		@Test
		public void getBehaviorGuardEvaluators_register_myEvent() {
			this.registry.register(this.agent);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new MyEvent());
			assertContains(transform(evaluators), "$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void getBehaviorGuardEvaluators_register_event_withValidFilter() {
			this.registry.register(this.agent, (event) -> true);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertContains(transform(evaluators), "$perception$guard$evaluator1");
		}

		@Test
		public void getBehaviorGuardEvaluators_register_myEvent_withValidFilter() {
			this.registry.register(this.agent, (event) -> true);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new MyEvent());
			assertContains(transform(evaluators), "$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void getBehaviorGuardEvaluators_register_event_withInvalidFilter() {
			this.registry.register(this.agent, (event) -> false);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertFalse(evaluators.iterator().hasNext());
		}
	
		@Test
		public void getBehaviorGuardEvaluators_register_myEvent_withInvalidFilter() {
			this.registry.register(this.agent, (event) -> false);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new MyEvent());
			assertFalse(evaluators.iterator().hasNext());
		}

		@Test
		public void unregister() {
			this.registry.register(this.agent);
			this.registry.unregister(this.agent);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertFalse(evaluators.iterator().hasNext());
		}
	
		@Test
		public void unregisterAll() {
			this.registry.register(this.agent);
			this.registry.register(new MyAgent2());
			this.registry.register(new MyAgent2());
			this.registry.unregisterAll();
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertFalse(evaluators.iterator().hasNext());
		}
	
		@Test
		public void unregisterAll_withCallback() {
			Object subscriber1 = new MyAgent2();
			Object subscriber2 = new MyAgent2();
			this.registry.register(this.agent);
			this.registry.register(subscriber1);
			this.registry.register(subscriber2);
			Procedure1<Object> callback = Mockito.mock(Procedure1.class);
			this.registry.unregisterAll(callback);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertFalse(evaluators.iterator().hasNext());
			ArgumentCaptor<Object> argument = ArgumentCaptor.forClass(Object.class);
			Mockito.verify(callback, Mockito.times(3)).apply(argument.capture());
			assertContains(argument.getAllValues(), this.agent, subscriber1, subscriber2);
		}

		@Test
		public void getBehaviorGuardEvaluatorsFor_noRegister_event() {
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new Event() { }, new Object());
			assertFalse(evaluators.iterator().hasNext());
		}
	
		@Test
		public void getBehaviorGuardEvaluatorsFor_noRegister_event_otherRegistered() {
			this.registry.register(this.agent);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new Event() { }, new Object());
			assertFalse(evaluators.iterator().hasNext());
		}

		@Test
		public void getBehaviorGuardEvaluatorsFor_register_event() {
			this.registry.register(this.agent);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new Event() { }, this.agent);
			assertContains(transform(evaluators), "$perception$guard$evaluator1");
		}

		@Test
		public void getBehaviorGuardEvaluatorsFor_register_myEvent() {
			this.registry.register(this.agent);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new MyEvent(), this.agent);
			assertContains(transform(evaluators), "$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void getBehaviorGuardEvaluatorsFor_register_event_withValidFilter() {
			this.registry.register(this.agent, (event) -> true);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new Event() { }, this.agent);
			assertContains(transform(evaluators), "$perception$guard$evaluator1");
		}

		@Test
		public void getBehaviorGuardEvaluatorsFor_register_myEvent_withValidFilter() {
			this.registry.register(this.agent, (event) -> true);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new MyEvent(), this.agent);
			assertContains(transform(evaluators), "$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void getBehaviorGuardEvaluatorsFor_register_event_withInvalidFilter() {
			this.registry.register(this.agent, (event) -> false);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new Event() { }, this.agent);
			assertContains(transform(evaluators), "$perception$guard$evaluator1");
		}
	
		@Test
		public void getBehaviorGuardEvaluatorsFor_register_myEvent_withInvalidFilter() {
			this.registry.register(this.agent, (event) -> false);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new MyEvent(), this.agent);
			assertContains(transform(evaluators), "$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void getBehaviorGuardEvaluatorsFor_twoRegisteredAgents_myEvent_01() {
			Object otherAgent = new MyAgent();
			this.registry.register(this.agent);
			this.registry.register(otherAgent);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new MyEvent(), this.agent);
			assertContains(transform(evaluators), "$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void getBehaviorGuardEvaluatorsFor_twoRegisteredAgents_myEvent_02() {
			Object otherAgent = new MyAgent();
			this.registry.register(otherAgent);
			this.registry.register(this.agent);
			Iterable<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluatorsFor(new MyEvent(), this.agent);
			assertContains(transform(evaluators), "$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void register_withCallBack_01() {
			Procedure1<Object> callback = Mockito.mock(Procedure1.class);
			this.registry.register(this.agent, callback);
			ArgumentCaptor<Object> argument = ArgumentCaptor.forClass(Object.class);
			Mockito.verify(callback, Mockito.only()).apply(argument.capture());
			assertSame(this.agent, argument.getValue());
		}

		@Test
		public void register_withCallBack_02() {
			Procedure1<Object> callback = Mockito.mock(Procedure1.class);
			this.registry.register(this.agent, callback);
			this.registry.register(this.agent, callback);
			ArgumentCaptor<Object> argument = ArgumentCaptor.forClass(Object.class);
			Mockito.verify(callback, Mockito.only()).apply(argument.capture());
			assertSame(this.agent, argument.getValue());
		}

		@Test
		public void unregister_withCallBack_01() {
			Procedure1<Object> callback = Mockito.mock(Procedure1.class);
			this.registry.register(this.agent);
			this.registry.unregister(this.agent, callback);
			ArgumentCaptor<Object> argument = ArgumentCaptor.forClass(Object.class);
			Mockito.verify(callback, Mockito.only()).apply(argument.capture());
			assertSame(this.agent, argument.getValue());
		}

		@Test(expected = IllegalArgumentException.class)
		public void unregister_withCallBack_02() {
			Procedure1<Object> callback = Mockito.mock(Procedure1.class);
			this.registry.register(this.agent);
			this.registry.unregister(this.agent, callback);
			this.registry.unregister(this.agent, callback);
		}

		@Test
		public void hasRegisteredEventListener() {
			assertFalse(this.registry.hasRegisteredEventListener(Object.class));
			//
			FakeEventListener eventListener = Mockito.mock(FakeEventListener.class);
			this.registry.register(eventListener);
			//
			assertTrue(this.registry.hasRegisteredEventListener(Object.class));
			assertTrue(this.registry.hasRegisteredEventListener(FakeEventListener.class));
			assertFalse(this.registry.hasRegisteredEventListener(EventListener.class));
		}

		@Test
		public void getRegisteredEventListeners() {
			Collection<Object> collection;
			//
			collection = new ArrayList<>();
			assertEquals(0, this.registry.getRegisteredEventListeners(Object.class, collection));
			assertContains(collection);
			//
			FakeEventListener eventListener = Mockito.mock(FakeEventListener.class);
			this.registry.register(eventListener);
			//
			collection = new ArrayList<>();
			assertEquals(1, this.registry.getRegisteredEventListeners(Object.class, collection));
			assertContains(collection, eventListener);
			//
			collection = new ArrayList<>();
			assertEquals(1, this.registry.getRegisteredEventListeners(FakeEventListener.class, collection));
			assertContains(collection, eventListener);
			//
			collection = new ArrayList<>();
			assertEquals(0, this.registry.getRegisteredEventListeners(EventListener.class, collection));
			assertContains(collection);
		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class FakeEventListener {
			@PerceptGuardEvaluator
			public void handler(Event event, Collection<Runnable> handlers) {
				//
			}
		}

	}
	
	public static class ConcurrentRegistryTest extends AbstractBehaviorGuardEvaluatorRegistryTest {

		@Override
		protected BehaviorGuardEvaluatorRegistry createRegistry() {
			return new BehaviorGuardEvaluatorRegistry(true);
		}

	}

	public static class NotConcurrentRegistryTest extends AbstractBehaviorGuardEvaluatorRegistryTest {

		@Override
		protected BehaviorGuardEvaluatorRegistry createRegistry() {
			return new BehaviorGuardEvaluatorRegistry(false);
		}

	}

	public static class MyEvent extends Event {
		public int n;
	}

	public static class MyAgent extends Agent {

		public MyAgent() {
			super(UUID.randomUUID(), UUID.randomUUID());
		}

		@PerceptGuardEvaluator
		private void $perception$guard$evaluator1(Event event, Collection<Runnable> runners) {
			runners.add(() -> $perception$guard$callback1(event, event));
		}

		private void $perception$guard$callback1(Event occurrence, Event it) {
			
		}

		@PerceptGuardEvaluator
		private void $perception$guard$evaluator2(MyEvent event, Collection<Runnable> runners) {
			if (event.n < 0) {
				runners.add(() -> $perception$guard$callback2(event, event));
			}
		}

		private void $perception$guard$callback2(MyEvent occurrence, MyEvent it) {
			
		}

	}

	public static class MyAgent2 extends Agent {

		public MyAgent2() {
			super(UUID.randomUUID(), UUID.randomUUID());
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
