/*
 * Copyright (C) 2014-2017 the original authors or authors.
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

import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.UUID;

import javax.inject.Inject;

import com.google.common.collect.Collections2;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.eventdispatching.BehaviorGuardEvaluator;
import io.sarl.eventdispatching.BehaviorGuardEvaluatorRegistry;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Event;
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
	
		@Test
		public void getBehaviorGuardEvaluators_noRegister_event() {
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertTrue(evaluators.isEmpty());
		}
	
		@Test
		public void getBehaviorGuardEvaluators_myEvent() {
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new MyEvent());
			assertTrue(evaluators.isEmpty());
		}
	
		@Test
		public void getBehaviorGuardEvaluators_register_event() {
			this.registry.register(this.agent);
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertContains(Collections2.transform(evaluators, (it) -> it.toString()),
					"$perception$guard$evaluator1");
		}

		@Test
		public void getBehaviorGuardEvaluators_register_myEvent() {
			this.registry.register(this.agent);
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new MyEvent());
			assertContains(Collections2.transform(evaluators, (it) -> it.toString()),
					"$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void getBehaviorGuardEvaluators_register_event_withValidFilter() {
			this.registry.register(this.agent, (event) -> true);
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertContains(Collections2.transform(evaluators, (it) -> it.toString()),
					"$perception$guard$evaluator1");
		}

		@Test
		public void getBehaviorGuardEvaluators_register_myEvent_withValidFilter() {
			this.registry.register(this.agent, (event) -> true);
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new MyEvent());
			assertContains(Collections2.transform(evaluators, (it) -> it.toString()),
					"$perception$guard$evaluator1", "$perception$guard$evaluator2");
		}

		@Test
		public void getBehaviorGuardEvaluators_register_event_withInvalidFilter() {
			this.registry.register(this.agent, (event) -> false);
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertTrue(evaluators.isEmpty());
		}
	
		@Test
		public void getBehaviorGuardEvaluators_register_myEvent_withInvalidFilter() {
			this.registry.register(this.agent, (event) -> false);
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new MyEvent());
			assertTrue(evaluators.isEmpty());
		}

		@Test
		public void unregister() {
			this.registry.register(this.agent);
			this.registry.unregister(this.agent);
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertTrue(evaluators.isEmpty());
		}
	
		@Test
		public void unregisterAll() {
			this.registry.register(this.agent);
			this.registry.register(new MyAgent2());
			this.registry.register(new MyAgent2());
			this.registry.unregisterAll();
			Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
			assertTrue(evaluators.isEmpty());
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
			if (event.n < 0) {
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