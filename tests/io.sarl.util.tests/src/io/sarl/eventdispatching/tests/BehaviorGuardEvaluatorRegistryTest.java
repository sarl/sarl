/*
 * Copyright (C) 2014-2016 the original authors or authors.
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
package io.sarl.eventdispatching.tests;

import static org.junit.Assert.*;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArraySet;

import javax.inject.Inject;

import com.google.common.base.MoreObjects;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.Mockito;

import io.sarl.eventdispatching.BehaviorGuardEvaluator;
import io.sarl.eventdispatching.BehaviorGuardEvaluatorRegistry;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Event;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BehaviorGuardEvaluatorRegistryTest extends AbstractSarlTest {

	@Inject
	private ReflectExtensions reflect;

	@Nullable
	private BehaviorGuardEvaluatorRegistry registry;

	@Nullable
	private MyAgent agent;

	@Before
	public void setUp() throws Exception {
		this.agent = new MyAgent(null, UUID.randomUUID(), UUID.randomUUID());
		this.registry = new BehaviorGuardEvaluatorRegistry();
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
	public void unregister() {
		this.registry.register(this.agent);
		this.registry.unregister(this.agent);
		Collection<BehaviorGuardEvaluator> evaluators = this.registry.getBehaviorGuardEvaluators(new Event() { });
		assertTrue(evaluators.isEmpty());
	}

	public static class MyEvent extends Event {
		public int n;
	}

	public static class MyAgent extends Agent {

		public MyAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
			super(provider, parentID, agentID);
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

}