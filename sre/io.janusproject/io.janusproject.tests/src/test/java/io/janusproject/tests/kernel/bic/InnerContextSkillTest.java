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

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.internal.verification.Times;

import io.janusproject.kernel.bic.InnerContextSkill;
import io.janusproject.kernel.bic.InternalEventBusCapacity;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.OpenEventSpace;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.Skill.UninstallationStage;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.ClearableReference;
import io.sarl.lang.util.SynchronizedIterable;
import io.sarl.tests.api.ManualMocking;
import io.sarl.tests.api.Nullable;
import io.sarl.util.concurrent.Collections3;
import io.sarl.util.concurrent.NoReadWriteLock;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@ManualMocking
public class InnerContextSkillTest extends AbstractJanusTest {

	@Nullable
	private UUID agentId;

	@Mock
	private EventListener eventListener;

	@Mock
	private AgentContext innerContext;

	@Mock(name = "innerSpace")
	private OpenEventSpace innerSpace;

	@Mock
	private ContextSpaceService contextService;

	@Mock
	private InternalEventBusCapacity busCapacity;

	@InjectMocks
	private InnerContextSkill skill;

	// It is an attribute to avoid to loose the weak references (eg. in AgentTraits).
	@Nullable
	private Agent agent;

	@Nullable
	private UUID innerContextUUID;

	@Nullable
	private UUID innerSpaceUUID;

	@Nullable
	private SpaceID innerSpaceID;

	@Before
	public void setUp() throws Exception {
		this.agentId = UUID.randomUUID();
		Address address = new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), EventSpaceSpecification.class),
				this.agentId);
		this.agent = new TestAgent(this, this.agentId);
		this.agent = spy(this.agent);
		address = spy(address);
		this.skill = this.reflect.newInstance(InnerContextSkill.class, this.agent, address);
		this.skill.setLockProvider(() -> NoReadWriteLock.SINGLETON);
		MockitoAnnotations.initMocks(this);
		when(this.agent.getID()).thenReturn(this.agentId);
		when(this.contextService.createContext(ArgumentMatchers.any(UUID.class), ArgumentMatchers.any(UUID.class))).thenReturn(this.innerContext);
		this.innerContextUUID = UUID.randomUUID();
		when(this.innerContext.getDefaultSpace()).thenReturn(this.innerSpace);
		when(this.innerContext.getID()).thenReturn(this.innerContextUUID);
		when(this.busCapacity.asEventListener()).thenReturn(this.eventListener);
		when(this.innerSpace.getParticipants()).thenReturn(Collections3.<UUID> synchronizedSingleton(this.agentId));
		this.innerSpaceUUID = UUID.randomUUID();
		this.innerSpaceID = new SpaceID(this.innerContextUUID, this.innerSpaceUUID, EventSpaceSpecification.class);
		when(this.innerSpace.getSpaceID()).thenReturn(this.innerSpaceID);
	}

	@Test
	public void getInnerContext() throws Exception {
		// Things are already injected
		this.reflect.invoke(this.skill, "resetInnerContext");
		assertFalse((Boolean) this.reflect.invoke(this.skill, "hasInnerContext"));
		//
		AgentContext ctx = this.skill.getInnerContext();
		assertSame(this.innerContext, ctx);
		assertTrue((Boolean) this.reflect.invoke(this.skill, "hasInnerContext"));
		ArgumentCaptor<EventListener> argument = ArgumentCaptor.forClass(EventListener.class);
		verify(this.innerSpace, new Times(1)).register(argument.capture());
		assertSame(this.eventListener, argument.getValue());
	}

	@Test
	public void uninstall_Post() throws Exception {
		// Things are already injected
		this.reflect.invoke(this.skill, "resetInnerContext");
		assertFalse((Boolean) this.reflect.invoke(this.skill, "hasInnerContext"));
		this.skill.getInnerContext();
		assertTrue((Boolean) this.reflect.invoke(this.skill, "hasInnerContext"));
		//
		this.reflect.invoke(this.skill, "uninstall", UninstallationStage.POST_DESTROY_EVENT);
		assertFalse((Boolean) this.reflect.invoke(this.skill, "hasInnerContext"));
		//
		ArgumentCaptor<EventListener> argument = ArgumentCaptor.forClass(EventListener.class);
		verify(this.innerSpace, new Times(1)).unregister(argument.capture());
		assertSame(this.eventListener, argument.getValue());
		//
		ArgumentCaptor<AgentContext> argument2 = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextService, new Times(1)).removeContext(argument2.capture());
		assertSame(this.innerContext, argument2.getValue());
	}

	@Test
	public void hasMemberAgent_nomember() {
		assertFalse(this.skill.hasMemberAgent());
	}

	@Test
	public void hasMemberAgent_member() {
		when(this.innerSpace.getParticipants())
				.thenReturn(Collections3.synchronizedSet(new HashSet<>(Arrays.asList(this.agentId, UUID.randomUUID())), NoReadWriteLock.SINGLETON));
		assertTrue(this.skill.hasMemberAgent());
	}

	@Test
	public void getMemberAgentCount_nomember() {
		assertEquals(0, this.skill.getMemberAgentCount());
	}

	@Test
	public void getMemberAgentCount_member() {
		when(this.innerSpace.getParticipants())
				.thenReturn(Collections3.synchronizedSet(new HashSet<>(Arrays.asList(this.agentId, UUID.randomUUID())), NoReadWriteLock.SINGLETON));
		assertEquals(1, this.skill.getMemberAgentCount());
	}

	@Test
	public void getMemberAgents_nomember() {
		SynchronizedIterable<UUID> set = this.skill.getMemberAgents();
		assertNotNull(set);
		assertFalse(set.iterator().hasNext());
	}

	@Test
	public void getMemberAgents_member() {
		UUID otherAgent = UUID.randomUUID();
		when(this.innerSpace.getParticipants())
				.thenReturn(Collections3.synchronizedSet(new HashSet<>(Arrays.asList(this.agentId, otherAgent)), NoReadWriteLock.SINGLETON));
		SynchronizedIterable<UUID> set = this.skill.getMemberAgents();
		assertNotNull(set);
		final List<UUID> list = new ArrayList<>();
		Iterator<UUID> iterator = set.iterator();
		while (iterator.hasNext()) {
			list.add(iterator.next());
		}
		assertFalse(list.isEmpty());
		assertEquals(1, list.size());
		assertTrue(list.contains(otherAgent));
	}

	@Test(expected = NullPointerException.class)
	public void isInnerDefaultSpaceSpace_null() {
		this.skill.isInnerDefaultSpace((Space) null);
	}

	@Test
	public void isInnerDefaultSpaceSpace_defaultSpace() {
		assertTrue(this.skill.isInnerDefaultSpace(this.innerSpace));
	}

	@Test
	public void isInnerDefaultSpaceSpace_otherSpace() {
		UUID id = UUID.randomUUID();
		SpaceID spaceId = mock(SpaceID.class);
		when(spaceId.getID()).thenReturn(id);
		EventSpace otherSpace = mock(EventSpace.class);
		when(otherSpace.getSpaceID()).thenReturn(spaceId);
		//
		assertFalse(this.skill.isInnerDefaultSpace(otherSpace));
	}

	@Test(expected = NullPointerException.class)
	public void isInnerDefaultSpaceSpaceID_null() {
		this.skill.isInnerDefaultSpace((SpaceID) null);
	}

	@Test
	public void isInnerDefaultSpaceSpaceID_defaultSpace() {
		assertTrue(this.skill.isInnerDefaultSpace(this.innerSpaceID));
	}

	@Test
	public void isInnerDefaultSpaceSpaceID_otherSpace() {
		UUID id = UUID.randomUUID();
		SpaceID spaceId = mock(SpaceID.class);
		when(spaceId.getID()).thenReturn(id);
		//
		assertFalse(this.skill.isInnerDefaultSpace(spaceId));
	}

	@Test
	public void isInInnerDefaultSpaceEvent_null() {
		assertFalse(this.skill.isInInnerDefaultSpace(null));
	}

	@Test
	public void isInInnerDefaultSpaceEvent_inside() {
		Event event = mock(Event.class);
		Address adr = mock(Address.class);
		when(adr.getSpaceID()).thenReturn(this.innerSpaceID);
		when(event.getSource()).thenReturn(adr);
		//
		assertTrue(this.skill.isInInnerDefaultSpace(event));
	}

	@Test
	public void isInInnerDefaultSpaceEvent_outside() {
		Event event = mock(Event.class);
		SpaceID spaceId = mock(SpaceID.class);
		when(spaceId.getID()).thenReturn(UUID.randomUUID());
		Address adr = mock(Address.class);
		when(adr.getSpaceID()).thenReturn(spaceId);
		when(event.getSource()).thenReturn(adr);
		//
		assertFalse(this.skill.isInInnerDefaultSpace(event));
	}

	public static class TestAgent extends Agent {
		
		private final InnerContextSkillTest test;

		public TestAgent(InnerContextSkillTest test, UUID agentId) {
			super(Mockito.mock(BuiltinCapacitiesProvider.class), agentId, null);
			this.test = test;
		}

		@Override
		protected ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
			return new ClearableReference(this.test.busCapacity);
		}

	}

}
