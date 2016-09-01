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

import static org.junit.Assert.assertSame;

import java.util.UUID;

import io.janusproject.kernel.bic.BehaviorsSkill;
import io.janusproject.kernel.bic.InternalEventBusCapacity;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
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
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.SpaceID;

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

		Agent agent = new Agent(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null) {
			/**
			 * {@inheritDoc}
			 */
			@Override
			protected <S extends Capacity> S getSkill(Class<S> capacity) {
				if (InternalEventBusCapacity.class.equals(capacity))
					return capacity.cast(BehaviorsSkillTest.this.busCapacity);
				return capacity.cast(BehaviorsSkillTest.this.innerCapacity);
			}
		};

		this.skill = this.reflect.newInstance(BehaviorsSkill.class, agent, this.address);
	}

	@Test
	public void asEventListener() {
		assertSame(this.eventListener, this.skill.asEventListener());
	}

	@Test
	public void registerBehavior() {
		Behavior b = Mockito.mock(Behavior.class);
		assertSame(b, this.skill.registerBehavior(b));
		ArgumentCaptor<Behavior> argument = ArgumentCaptor.forClass(Behavior.class);
		Mockito.verify(this.busCapacity).registerEventListener(argument.capture());
		assertSame(b, argument.getValue());
	}

	@Test
	public void unregisterBehavior() {
		Behavior b = Mockito.mock(Behavior.class);
		this.skill.registerBehavior(b);
		//
		assertSame(b, this.skill.unregisterBehavior(b));
		ArgumentCaptor<Behavior> argument = ArgumentCaptor.forClass(Behavior.class);
		Mockito.verify(this.busCapacity).unregisterEventListener(argument.capture());
		assertSame(b, argument.getValue());
	}

	@Test
	public void wake() {
		Event event = Mockito.mock(Event.class);
		this.skill.wake(event);
		ArgumentCaptor<Event> argument1 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.innerSpace).emit(argument1.capture());
		assertSame(event, argument1.getValue());
		ArgumentCaptor<Address> argument2 = ArgumentCaptor.forClass(Address.class);
		Mockito.verify(event).setSource(argument2.capture());
		assertEquals(this.address, argument2.getValue());
	}

}
