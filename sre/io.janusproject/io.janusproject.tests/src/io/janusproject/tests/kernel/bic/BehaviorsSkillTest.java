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

import java.util.UUID;

import io.janusproject.kernel.bic.BehaviorsSkill;
import io.janusproject.kernel.bic.InternalEventBusCapacity;
import io.janusproject.tests.testutils.AbstractJanusTest;

import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;

import io.sarl.core.InnerContextAccess;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.ClearableReference;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.Nullable;

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
		Behavior b = new TestBehavior();
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
		Behavior b = new TestBehavior();
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
		Behavior b = new TestBehavior();
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
		Behavior b = new TestBehavior();
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
	public void wake() {
		Event event = mock(Event.class);
		this.skill.wake(event);
		ArgumentCaptor<Event> argument1 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.innerSpace).emit(argument1.capture());
		assertSame(event, argument1.getValue());
		ArgumentCaptor<Address> argument2 = ArgumentCaptor.forClass(Address.class);
		Mockito.verify(event).setSource(argument2.capture());
		assertEquals(this.address, argument2.getValue());
	}

	public static class TestAgent extends Agent {

		private final BehaviorsSkillTest test;
		
		public TestAgent(BehaviorsSkillTest test) {
			super(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null);
			this.test = test;
		}

		@Override
		protected ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
			if (InternalEventBusCapacity.class.equals(capacity))
				return new ClearableReference(this.test.busCapacity);
			return new ClearableReference(this.test.innerCapacity);
		}

	}

	public static class TestBehavior extends Behavior {

		public TestBehavior() {
			super(null);
		}

	}

}
