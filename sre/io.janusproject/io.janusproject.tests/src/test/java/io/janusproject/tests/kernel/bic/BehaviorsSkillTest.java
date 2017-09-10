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
package io.janusproject.tests.kernel.bic;

import static org.junit.Assert.*;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import io.janusproject.kernel.bic.BehaviorsSkill;
import io.janusproject.kernel.bic.InternalEventBusCapacity;
import io.janusproject.kernel.bic.SchedulesSkill;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.InnerContextAccess;
import io.sarl.core.Schedules;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacities;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.ClearableReference;
import io.sarl.tests.api.Nullable;
import io.sarl.util.Scopes;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class BehaviorsSkillTest extends AbstractJanusTest {

	@Nullable
	private EventListener eventListener;

	@Nullable
	private InternalEventBusCapacity busCapacity;

	@Nullable
	private InnerContextAccess innerCapacity;

	@Nullable
	private SchedulesSkill schedulesCapacity;

	@Nullable
	private EventSpace innerSpace;

	@Nullable
	private Address address;

	@Nullable
	private BehaviorsSkill skill;

	@Nullable
	private Skill1 specificCapacity;

	@Nullable
	private TestAgent agent;

	@Before
	public void setUp() throws Exception {
		this.eventListener = Mockito.mock(EventListener.class);
		this.address = new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), EventSpaceSpecification.class),
				UUID.randomUUID());
		this.busCapacity = Mockito.mock(InternalEventBusCapacity.class);
		Mockito.when(this.busCapacity.asEventListener()).thenReturn(this.eventListener);

		this.innerSpace = Mockito.mock(EventSpace.class);
		Mockito.when(this.innerSpace.getAddress(ArgumentMatchers.any(UUID.class))).thenReturn(this.address);
		AgentContext innerContext = Mockito.mock(AgentContext.class);
		Mockito.when(innerContext.getDefaultSpace()).thenReturn(this.innerSpace);
		this.innerCapacity = Mockito.mock(InnerContextAccess.class);
		Mockito.when(this.innerCapacity.getInnerContext()).thenReturn(innerContext);
		this.schedulesCapacity = Mockito.mock(SchedulesSkill.class);

		this.agent = new TestAgent(this);
		this.skill = this.reflect.newInstance(BehaviorsSkill.class, this.agent, this.address);

		this.specificCapacity = new Skill1(this.agent);
	}

	@Test
	public void asEventListener() {
		assertSame(this.eventListener, this.skill.asEventListener());
	}

	@Test
	public void registerBehavior() {
		Behavior b = new TestBehavior(this.agent);
		b = spy(b);
		assertSame(b, this.skill.registerBehavior(b));
		ArgumentCaptor<Behavior> argument1 = ArgumentCaptor.forClass(Behavior.class);
		ArgumentCaptor<Boolean> argument2 = ArgumentCaptor.forClass(Boolean.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument3 = ArgumentCaptor.forClass(Function1.class);
		Mockito.verify(this.busCapacity).registerEventListener(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(b, argument1.getValue());
		assertTrue(argument2.getValue());
		assertNull(argument3.getValue());
	}

	@Test
	public void registerBehavior_null() {
		Behavior b = new TestBehavior(this.agent);
		b = spy(b);
		assertSame(b, this.skill.registerBehavior(b, null));
		ArgumentCaptor<Behavior> argument1 = ArgumentCaptor.forClass(Behavior.class);
		ArgumentCaptor<Boolean> argument2 = ArgumentCaptor.forClass(Boolean.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument3 = ArgumentCaptor.forClass(Function1.class);
		Mockito.verify(this.busCapacity).registerEventListener(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(b, argument1.getValue());
		assertTrue(argument2.getValue());
		assertNull(argument3.getValue());
	}

	@Test
	public void registerBehavior_validFilter() {
		Behavior b = new TestBehavior(this.agent);
		b = spy(b);
		Function1<? super Event, ? extends Boolean> filter = (event) -> true;
		assertSame(b, this.skill.registerBehavior(b, filter));
		ArgumentCaptor<Behavior> argument1 = ArgumentCaptor.forClass(Behavior.class);
		ArgumentCaptor<Boolean> argument2 = ArgumentCaptor.forClass(Boolean.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument3 = ArgumentCaptor.forClass(Function1.class);
		Mockito.verify(this.busCapacity).registerEventListener(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(b, argument1.getValue());
		assertTrue(argument2.getValue());
		assertSame(filter, argument3.getValue());
	}

	@Test
	public void registerBehavior_invalidFilter() {
		Behavior b = new TestBehavior(this.agent);
		b = spy(b);
		Function1<? super Event, ? extends Boolean> filter = (event) -> false;
		assertSame(b, this.skill.registerBehavior(b, filter));
		ArgumentCaptor<Behavior> argument1 = ArgumentCaptor.forClass(Behavior.class);
		ArgumentCaptor<Boolean> argument2 = ArgumentCaptor.forClass(Boolean.class);
		ArgumentCaptor<Function1<? super Event, ? extends Boolean>> argument3 = ArgumentCaptor.forClass(Function1.class);
		Mockito.verify(this.busCapacity).registerEventListener(argument1.capture(), argument2.capture(), argument3.capture());
		assertSame(b, argument1.getValue());
		assertTrue(argument2.getValue());
		assertSame(filter, argument3.getValue());
	}

	@Test
	public void unregisterBehavior() {
		Behavior b = new TestBehavior(this.agent);
		b = spy(b);
		this.skill.registerBehavior(b);
		//
		assertSame(b, this.skill.unregisterBehavior(b));
		ArgumentCaptor<Behavior> argument1 = ArgumentCaptor.forClass(Behavior.class);
		ArgumentCaptor<Boolean> argument2 = ArgumentCaptor.forClass(Boolean.class);
		Mockito.verify(this.busCapacity, Mockito.times(1)).unregisterEventListener(argument1.capture(), argument2.capture());
		assertSame(b, argument1.getValue());
		assertTrue(argument2.getValue());
	}

	@Test
	public void hasRegisteredBehavior() {
		Mockito.doReturn(false).when(this.busCapacity).hasRegisteredEventListener(ArgumentMatchers.any());
		assertFalse(this.skill.hasRegisteredBehavior());
		//
		Mockito.doReturn(true).when(this.busCapacity).hasRegisteredEventListener(ArgumentMatchers.any());
		assertTrue(this.skill.hasRegisteredBehavior());
	}

	@Test
	public void getRegisteredBehaviors() {
		Mockito.doReturn(0).when(this.busCapacity).getRegisteredEventListeners(ArgumentMatchers.any(), ArgumentMatchers.any());
		assertContains(this.skill.getRegisteredBehaviors());
		//
		Object behaviorListener = new TestBehavior(this.agent);
		Mockito.doAnswer((it) -> {
			((Collection) it.getArgument(1)).add(behaviorListener);
			return 1;
		}).when(this.busCapacity).getRegisteredEventListeners(ArgumentMatchers.any(), ArgumentMatchers.any());
		assertContains(this.skill.getRegisteredBehaviors(), behaviorListener);
	}

	@Test
	public void wake_noScope() {
		Event event = mock(Event.class);
		this.skill.wake(event);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope<Address>> argument3 = ArgumentCaptor.forClass(Scope.class);
		Mockito.verify(this.innerSpace).emit(argument1.capture(), argument2.capture(), argument3.capture());
		assertEquals(this.agent.getID(), argument1.getValue());
		assertSame(event, argument2.getValue());
		assertNull(argument3.getValue());
	}

	@Test
	public void wake_scope_all() {
		Event event = mock(Event.class);
		this.skill.wake(event, Scopes.allParticipants());
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope<Address>> argument3 = ArgumentCaptor.forClass(Scope.class);
		Mockito.verify(this.innerSpace).emit(argument1.capture(), argument2.capture(), argument3.capture());
		assertEquals(this.agent.getID(), argument1.getValue());
		assertSame(event, argument2.getValue());
		assertNotNull(argument3.getValue());
		assertSame(Scopes.allParticipants(), argument3.getValue());
	}

	@Test
	public void wake_scope_any() {
		Event event = mock(Event.class);
		SpaceID spaceID = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), null);
		Scope<Address> scope = Scopes.addresses(new Address(spaceID, UUID.randomUUID()));
		this.skill.wake(event, scope);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope<Address>> argument3 = ArgumentCaptor.forClass(Scope.class);
		Mockito.verify(this.innerSpace).emit(argument1.capture(), argument2.capture(), argument3.capture());
		assertEquals(this.agent.getID(), argument1.getValue());
		assertSame(event, argument2.getValue());
		assertNotNull(argument3.getValue());
		assertSame(scope, argument3.getValue());
	}

	@Test
	public void contextAwareCapacityCall() throws Exception {
		TestBehavior b = new TestBehavior(this.agent);
		//
		b.runContextAwareCapacityCallTest();
		//
		assertNotNull(this.specificCapacity.caller);
		assertSame(b, this.specificCapacity.caller);
	}

	public static interface Capacity1 extends Capacity {
		void callCapacity();
		public static class ContextAwareCapacityWrapper<C extends Capacity1> extends Capacity.ContextAwareCapacityWrapper<C> implements Capacity1 {
			public ContextAwareCapacityWrapper(C capacity, AgentTrait caller) {
				super(capacity, caller);
			}
			public void callCapacity() {
				try {
					ensureCallerInLocalThread();
					this.capacity.callCapacity();
				} finally {
					resetCallerInLocalThread();
				}
			}
		}
	}

	public static class Skill1 extends Skill implements Capacity1 {

		public Object caller;

		public Skill1(Agent agent) {
			super(agent);
		}

		@Override
		public void callCapacity() {
			Object caller = Capacities.getCaller();
			this.caller = caller; 
		}

	}

	public static class TestAgent extends Agent {

		private final BehaviorsSkillTest test;

		public TestAgent(BehaviorsSkillTest test) {
			super(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null);
			this.test = test;
		}

		@Override
		protected ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
			if (Capacity1.class.equals(capacity))
				return new ClearableReference(this.test.specificCapacity);
			if (InternalEventBusCapacity.class.equals(capacity))
				return new ClearableReference(this.test.busCapacity);
			if (InnerContextAccess.class.equals(capacity))
				return new ClearableReference(this.test.innerCapacity);
			if (Schedules.class.equals(capacity))
				return new ClearableReference(this.test.schedulesCapacity);
			return new ClearableReference<>(null);
		}

	}

	public static class TestBehavior extends Behavior {

		public TestBehavior(Agent owner) {
			super(owner);
		}

		public void runContextAwareCapacityCallTest() {
			Capacity1 skill = getSkill(Capacity1.class);
			assertNotNull(skill);
			assertInstanceOf(Capacity1.ContextAwareCapacityWrapper.class, skill);
			Capacity1 original = ((Capacity1.ContextAwareCapacityWrapper<?>) skill).getDelegate();
			assertInstanceOf(Skill1.class, original);
			Skill1 skill1 = (Skill1) original;
			skill.callCapacity();
			assertSame(this, skill1.caller);
		}

	}

}