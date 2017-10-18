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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.mockito.internal.verification.Times;

import io.janusproject.kernel.bic.DefaultContextInteractionsSkill;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.Lifecycle;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.Space;
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
public class DefaultContextInteractionsSkillTest extends AbstractJanusTest {

	@Nullable
	private Agent agent;

	@Nullable
	private EventSpace defaultSpace;

	@Nullable
	private AgentContext parentContext;

	@Nullable
	private DefaultContextInteractionsSkill skill;

	@Nullable
	private Address address;

	@Nullable
	private Lifecycle lifeCapacity;

	@Nullable
	private SpaceID defaultSpaceID;

	@Nullable
	private UUID defaultSpaceUUID;

	@Nullable
	private UUID defaultContextUUID;

	@Before
	public void setUp() throws Exception {
		this.defaultSpaceUUID = UUID.randomUUID();
		this.defaultContextUUID = UUID.randomUUID();

		this.defaultSpaceID = new SpaceID(this.defaultContextUUID, this.defaultSpaceUUID, EventSpaceSpecification.class);

		this.address = new Address(this.defaultSpaceID, UUID.randomUUID());

		this.defaultSpace = mock(EventSpace.class);
		when(this.defaultSpace.getAddress(ArgumentMatchers.any(UUID.class))).thenReturn(this.address);
		when(this.defaultSpace.getSpaceID()).thenReturn(this.defaultSpaceID);

		this.parentContext = mock(AgentContext.class);
		when(this.parentContext.getDefaultSpace()).thenReturn(this.defaultSpace);
		when(this.parentContext.getID()).thenReturn(this.defaultContextUUID);

		this.lifeCapacity = mock(Lifecycle.class);

		this.agent = new Agent(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null) {
			@Override
			protected ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
				return new ClearableReference(DefaultContextInteractionsSkillTest.this.lifeCapacity);
			}
		};
		this.skill = this.reflect.newInstance(DefaultContextInteractionsSkill.class, agent, this.parentContext);
	}

	@Test
	public void getDefaultContext() throws Exception {
		assertSame(this.parentContext, this.skill.getDefaultContext());
	}

	@Test
	public void getDefaultSpace() throws Exception {
		assertNull(this.skill.getDefaultSpace());
		this.reflect.invoke(this.skill, "install");
		assertSame(this.defaultSpace, this.skill.getDefaultSpace());
	}

	@Test
	public void getDefaultAddress() throws Exception {
		this.reflect.invoke(this.skill, "install");
		assertSame(this.address, this.skill.getDefaultAddress());
	}

	@Test
	public void install() throws Exception {
		assertNull(this.skill.getDefaultSpace());
		this.reflect.invoke(this.skill, "install");
		assertSame(this.defaultSpace, this.skill.getDefaultSpace());
	}

	@Test
	public void emitEventScope() throws Exception {
		this.reflect.invoke(this.skill, "install");
		Event event = mock(Event.class);
		Scope<Address> scope = Scopes.allParticipants();
		this.skill.emit(event, scope);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope> argument3 = ArgumentCaptor.forClass(Scope.class);
		verify(this.defaultSpace, new Times(1)).emit(argument1.capture(), argument2.capture(), argument3.capture());
		assertEquals(this.agent.getID(), argument1.getValue());
		assertSame(event, argument2.getValue());
		assertSame(scope, argument3.getValue());
	}

	@Test
	public void emitEvent() throws Exception {
		this.reflect.invoke(this.skill, "install");
		Event event = mock(Event.class);
		this.skill.emit(event);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope> argument3 = ArgumentCaptor.forClass(Scope.class);
		verify(this.defaultSpace, new Times(1)).emit(argument1.capture(), argument2.capture(), argument3.capture());
		assertEquals(this.agent.getID(), argument1.getValue());
		assertSame(event, argument2.getValue());
		assertNull(argument3.getValue());
	}

	@Test
	@Deprecated
	public void spawn() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.spawn(Agent.class, "a", "b", "c"); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		ArgumentCaptor<Class> argument1 = ArgumentCaptor.forClass(Class.class);
		ArgumentCaptor<AgentContext> argument2 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<String> argument3 = ArgumentCaptor.forClass(String.class);
		verify(this.lifeCapacity, times(1)).spawnInContext(argument1.capture(), argument2.capture(), argument3.capture());
		assertEquals(Agent.class, argument1.getValue());
		assertSame(this.parentContext, argument2.getValue());
		assertArrayEquals(new String[] { "a", "b", "c" }, argument3.getAllValues().toArray()); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
	}

	@Test(expected = NullPointerException.class)
	public void isDefaultSpaceSpace_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isDefaultSpace((Space) null);
	}

	@Test
	public void isDefaultSpaceSpace_defaultSpace() throws Exception {
		this.reflect.invoke(this.skill, "install");
		assertTrue(this.skill.isDefaultSpace(this.defaultSpace));
	}

	@Test
	public void isDefaultSpaceSpace_otherSpace() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		SpaceID spaceId = mock(SpaceID.class);
		when(spaceId.getID()).thenReturn(id);
		EventSpace otherSpace = mock(EventSpace.class);
		when(otherSpace.getSpaceID()).thenReturn(spaceId);
		//
		assertFalse(this.skill.isDefaultSpace(otherSpace));
	}

	@Test(expected = NullPointerException.class)
	public void isDefaultSpaceSpaceID_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isDefaultSpace((SpaceID) null);
	}

	@Test
	public void isDefaultSpaceSpaceID_defaultSpace() throws Exception {
		this.reflect.invoke(this.skill, "install");
		assertTrue(this.skill.isDefaultSpace(this.defaultSpace.getSpaceID()));
	}

	@Test
	public void isDefaultSpaceSpaceID_otherSpace() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		SpaceID spaceId = mock(SpaceID.class);
		when(spaceId.getID()).thenReturn(id);
		//
		assertFalse(this.skill.isDefaultSpace(spaceId));
	}

	@Test(expected = NullPointerException.class)
	public void isDefaultSpaceUUID_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isDefaultSpace((UUID) null);
	}

	@Test
	public void isDefaultSpaceUUID_defaultSpace() throws Exception {
		this.reflect.invoke(this.skill, "install");
		assertTrue(this.skill.isDefaultSpace(this.defaultSpaceUUID));
	}

	@Test
	public void isDefaultSpaceUUID_otherSpace() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		assertFalse(this.skill.isDefaultSpace(id));
	}

	@Test
	public void isInDefaultSpaceEvent_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		assertFalse(this.skill.isInDefaultSpace(null));
	}

	@Test
	public void isInDefaultSpaceEvent_defaultSpace() throws Exception {
		this.reflect.invoke(this.skill, "install");
		Event event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		//
		assertTrue(this.skill.isInDefaultSpace(event));
	}

	@Test
	public void isInDefaultSpaceEvent_otherSpace() throws Exception {
		this.reflect.invoke(this.skill, "install");
		SpaceID spaceID = new SpaceID(this.defaultSpaceID.getContextID(), UUID.randomUUID(), EventSpaceSpecification.class);
		Address adr = mock(Address.class);
		when(adr.getSpaceID()).thenReturn(spaceID);
		Event event = mock(Event.class);
		when(event.getSource()).thenReturn(adr);
		//
		assertFalse(this.skill.isInDefaultSpace(event));
	}

	@Test(expected = NullPointerException.class)
	public void isDefaultContextAgentContext_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isDefaultContext((AgentContext) null);
	}

	@Test
	public void isDefaultContextAgentContext_defaultContext() throws Exception {
		this.reflect.invoke(this.skill, "install");
		assertTrue(this.skill.isDefaultContext(this.parentContext));
	}

	@Test
	public void isDefaultContextAgentContext_otherContext() throws Exception {
		this.reflect.invoke(this.skill, "install");
		AgentContext context = mock(AgentContext.class);
		when(context.getID()).thenReturn(UUID.randomUUID());
		//
		assertFalse(this.skill.isDefaultContext(context));
	}

	@Test(expected = NullPointerException.class)
	public void isDefaultContextUUID_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isDefaultContext((UUID) null);
	}

	@Test
	public void isDefaultContextUUID_defaultContext() throws Exception {
		this.reflect.invoke(this.skill, "install");
		assertTrue(this.skill.isDefaultContext(this.parentContext.getID()));
	}

	@Test
	public void isDefaultContextUUID_otherContext() throws Exception {
		this.reflect.invoke(this.skill, "install");
		assertFalse(this.skill.isDefaultContext(UUID.randomUUID()));
	}

}
