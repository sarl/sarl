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

import java.lang.reflect.Constructor;
import java.util.Collection;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.mockito.internal.verification.Times;

import io.janusproject.kernel.bic.BehaviorsSkill;
import io.janusproject.kernel.bic.InnerContextSkill;
import io.janusproject.kernel.bic.InternalEventBusCapacity;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.InnerContextAccess;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.Nullable;
import io.sarl.util.Scopes;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	BehaviorsSkillTest.WithInnerSpace.class,
	BehaviorsSkillTest.WithoutInnerSpace.class,
})
@SuppressWarnings("all")
public class BehaviorsSkillTest {

	public static class WithInnerSpace extends AbstractJanusTest {
		
		@Nullable
		private EventListener eventListener;
	
		@Nullable
		private InternalEventBusCapacity busCapacity;
	
		@Nullable
		private InnerContextAccess innerCapacity;
	
		@Nullable
		private EventSpace innerSpace;
	
		@Nullable
		private Address address;
	
		@Nullable
		private BehaviorsSkill skill;
	
		@Before
		public void setUp() throws Exception {
			this.eventListener = Mockito.mock(EventListener.class);
			this.address = new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), EventSpaceSpecification.class),
					UUID.randomUUID());
			this.busCapacity = Mockito.mock(InternalEventBusCapacity.class);
			Mockito.when(this.busCapacity.asEventListener()).thenReturn(this.eventListener);
			Mockito.when(this.busCapacity.getInnerDefaultSpaceAddress()).thenReturn(this.address);
	
			this.innerSpace = Mockito.mock(EventSpace.class);
			Mockito.when(this.innerSpace.getAddress(ArgumentMatchers.any(UUID.class))).thenReturn(this.address);
			AgentContext innerContext = Mockito.mock(AgentContext.class);
			Mockito.when(innerContext.getDefaultSpace()).thenReturn(this.innerSpace);
			this.innerCapacity = Mockito.mock(InnerContextAccess.class);
			Mockito.when(this.innerCapacity.getInnerContext()).thenReturn(innerContext);
	
			Agent agent = new TestAgent(this);
			this.skill = this.reflect.newInstance(BehaviorsSkill.class, agent, this.address);
		}
	
		@Test
		public void asEventListener() {
			assertSame(this.eventListener, this.skill.asEventListener());
		}
	
		@Test
		public void registerBehavior() {
			Behavior b = new TestBehavior();
			b = spy(b);
			assertSame(b, this.skill.registerBehavior(b));
			ArgumentCaptor<Behavior> argument = ArgumentCaptor.forClass(Behavior.class);
			Mockito.verify(this.busCapacity).registerEventListener(argument.capture());
			assertSame(b, argument.getValue());
		}
	
		@Test
		public void unregisterBehavior() {
			Behavior b = new TestBehavior();
			b = spy(b);
			this.skill.registerBehavior(b);
			//
			assertSame(b, this.skill.unregisterBehavior(b));
			ArgumentCaptor<Behavior> argument = ArgumentCaptor.forClass(Behavior.class);
			Mockito.verify(this.busCapacity).unregisterEventListener(argument.capture());
			assertSame(b, argument.getValue());
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
			Object behaviorListener = new TestBehavior();
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
			ArgumentCaptor<Event> argument1 = ArgumentCaptor.forClass(Event.class);
			ArgumentCaptor<Scope<Address>> argument2 = ArgumentCaptor.forClass(Scope.class);
			Mockito.verify(this.innerSpace).emit(argument1.capture(), argument2.capture());
			assertSame(event, argument1.getValue());
			assertNull(argument2.getValue());
			ArgumentCaptor<Address> argument3 = ArgumentCaptor.forClass(Address.class);
			Mockito.verify(event).setSource(argument3.capture());
			assertEquals(this.address, argument3.getValue());
		}
	
		@Test
		public void wake_scope_all() {
			Event event = mock(Event.class);
			this.skill.wake(event, Scopes.allParticipants());
			ArgumentCaptor<Event> argument1 = ArgumentCaptor.forClass(Event.class);
			ArgumentCaptor<Scope<Address>> argument2 = ArgumentCaptor.forClass(Scope.class);
			Mockito.verify(this.innerSpace).emit(argument1.capture(), argument2.capture());
			assertSame(event, argument1.getValue());
			assertNotNull(argument2.getValue());
			assertSame(Scopes.allParticipants(), argument2.getValue());
			ArgumentCaptor<Address> argument3 = ArgumentCaptor.forClass(Address.class);
			Mockito.verify(event).setSource(argument3.capture());
			assertEquals(this.address, argument3.getValue());
		}
	
		@Test
		public void wake_scope_any() {
			Event event = mock(Event.class);
			SpaceID spaceID = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), null);
			Scope<Address> scope = Scopes.addresses(new Address(spaceID, UUID.randomUUID()));
			this.skill.wake(event, scope);
			ArgumentCaptor<Event> argument1 = ArgumentCaptor.forClass(Event.class);
			ArgumentCaptor<Scope<Address>> argument2 = ArgumentCaptor.forClass(Scope.class);
			Mockito.verify(this.innerSpace).emit(argument1.capture(), argument2.capture());
			assertSame(event, argument1.getValue());
			assertNotNull(argument2.getValue());
			assertSame(scope, argument2.getValue());
			ArgumentCaptor<Address> argument3 = ArgumentCaptor.forClass(Address.class);
			Mockito.verify(event).setSource(argument3.capture());
			assertEquals(this.address, argument3.getValue());
		}

		public static class TestAgent extends Agent {

			private final WithInnerSpace test;
			
			public TestAgent(WithInnerSpace test) {
				super(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null);
				this.test = test;
			}

			@Override
			protected <S extends Capacity> S getSkill(Class<S> capacity) {
				if (InternalEventBusCapacity.class.equals(capacity))
					return capacity.cast(this.test.busCapacity);
				return capacity.cast(this.test.innerCapacity);
			}

		}

	}

	public static class WithoutInnerSpace extends AbstractJanusTest {
		
		@Nullable
		private EventListener eventListener;
	
		@Nullable
		private InternalEventBusCapacity busCapacity;
	
		@Nullable
		private InnerContextAccess innerCapacity;
	
		@Nullable
		private EventSpace innerSpace;
	
		@Nullable
		private Address address;
	
		@Nullable
		private BehaviorsSkill skill;
	
		@Before
		public void setUp() throws Exception {
			this.eventListener = Mockito.mock(EventListener.class);
			this.address = new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), EventSpaceSpecification.class),
					UUID.randomUUID());
			this.busCapacity = Mockito.mock(InternalEventBusCapacity.class);
			Mockito.when(this.busCapacity.asEventListener()).thenReturn(this.eventListener);
			Mockito.when(this.busCapacity.getInnerDefaultSpaceAddress()).thenReturn(this.address);
	
			this.innerSpace = Mockito.mock(EventSpace.class);
			Mockito.when(this.innerSpace.getAddress(ArgumentMatchers.any(UUID.class))).thenReturn(this.address);
			AgentContext innerContext = Mockito.mock(AgentContext.class);
			Mockito.when(innerContext.getDefaultSpace()).thenReturn(this.innerSpace);

			Agent agent = new TestAgent(this);

			InnerContextSkill innerSkill = this.reflect.newInstance(InnerContextSkill.class, agent, this.address);
			innerSkill = Mockito.spy(innerSkill);
			Mockito.doReturn(false).when(innerSkill).hasInnerContext();
			Mockito.doReturn(innerContext).when(innerSkill).getInnerContext();
			this.innerCapacity = innerSkill;
	
			this.skill = this.reflect.newInstance(BehaviorsSkill.class, agent, this.address);
		}
	
		@Test
		public void asEventListener() {
			assertSame(this.eventListener, this.skill.asEventListener());
		}
	
		@Test
		public void registerBehavior() {
			Behavior b = new TestBehavior();
			b = spy(b);
			assertSame(b, this.skill.registerBehavior(b));
			ArgumentCaptor<Behavior> argument = ArgumentCaptor.forClass(Behavior.class);
			Mockito.verify(this.busCapacity).registerEventListener(argument.capture());
			assertSame(b, argument.getValue());
		}
	
		@Test
		public void unregisterBehavior() {
			Behavior b = new TestBehavior();
			b = spy(b);
			this.skill.registerBehavior(b);
			//
			assertSame(b, this.skill.unregisterBehavior(b));
			ArgumentCaptor<Behavior> argument = ArgumentCaptor.forClass(Behavior.class);
			Mockito.verify(this.busCapacity).unregisterEventListener(argument.capture());
			assertSame(b, argument.getValue());
		}
	
		@Test
		public void wake_noScope() {
			Event event = mock(Event.class);
			this.skill.wake(event);
			ArgumentCaptor<Event> argument1 = ArgumentCaptor.forClass(Event.class);
			Mockito.verify(this.eventListener).receiveEvent(argument1.capture());
			assertSame(event, argument1.getValue());
			ArgumentCaptor<Address> argument2 = ArgumentCaptor.forClass(Address.class);
			Mockito.verify(event).setSource(argument2.capture());
			assertEquals(this.address, argument2.getValue());
		}
	
		@Test
		public void wake_scope_all() {
			Event event = mock(Event.class);
			this.skill.wake(event, Scopes.allParticipants());
			ArgumentCaptor<Event> argument1 = ArgumentCaptor.forClass(Event.class);
			Mockito.verify(this.eventListener).receiveEvent(argument1.capture());
			assertSame(event, argument1.getValue());
			ArgumentCaptor<Address> argument2 = ArgumentCaptor.forClass(Address.class);
			Mockito.verify(event).setSource(argument2.capture());
			assertEquals(this.address, argument2.getValue());
		}
	
		@Test
		public void wake_scope_any() {
			Event event = mock(Event.class);
			SpaceID spaceID = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), null);
			Scope<Address> scope = Scopes.addresses(new Address(spaceID, UUID.randomUUID()));
			this.skill.wake(event, scope);
			ArgumentCaptor<Event> argument1 = ArgumentCaptor.forClass(Event.class);
			Mockito.verify(this.eventListener, Mockito.never()).receiveEvent(argument1.capture());
		}

		public static class TestAgent extends Agent {

			private final WithoutInnerSpace test;
			
			public TestAgent(WithoutInnerSpace test) {
				super(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null);
				this.test = test;
			}

			@Override
			protected <S extends Capacity> S getSkill(Class<S> capacity) {
				if (InternalEventBusCapacity.class.equals(capacity))
					return capacity.cast(this.test.busCapacity);
				return capacity.cast(this.test.innerCapacity);
			}

		}

	}

	public static class TestBehavior extends Behavior {

		public TestBehavior() {
			super(null);
		}

	}

}
