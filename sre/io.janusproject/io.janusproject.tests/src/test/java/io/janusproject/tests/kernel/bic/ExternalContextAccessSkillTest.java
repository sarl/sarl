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

import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
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
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.janusproject.kernel.bic.ExternalContextAccessSkill;
import io.janusproject.kernel.bic.InternalEventBusCapacity;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.Behaviors;
import io.sarl.core.ContextJoined;
import io.sarl.core.ContextLeft;
import io.sarl.core.MemberJoined;
import io.sarl.core.MemberLeft;
import io.sarl.core.OpenEventSpace;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.Skill.UninstallationStage;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.ClearableReference;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.tests.api.ManualMocking;
import io.sarl.tests.api.Nullable;
import io.sarl.util.Scopes;
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
public class ExternalContextAccessSkillTest extends AbstractJanusTest {

	@Nullable
	private SynchronizedCollection<AgentContext> contexts;

	@Mock
	private ContextSpaceService contextRepository;

	@InjectMocks
	private ExternalContextAccessSkill skill;

	@Nullable
	private InternalEventBusCapacity busCapacity;

	@Nullable
	private Behaviors behaviorCapacity;

	@Nullable
	private OpenEventSpace defaultSpace;

	@Nullable
	private Agent agent;

	@Nullable
	private EventListener eventListener;

	@Before
	public void setUp() throws Exception {
		UUID parentId = UUID.randomUUID();

		this.eventListener = mock(EventListener.class);
		this.behaviorCapacity = mock(Behaviors.class);

		this.busCapacity = mock(InternalEventBusCapacity.class);
		when(this.busCapacity.asEventListener()).thenReturn(this.eventListener);

		final List<AgentContext> contextsList = new ArrayList<>();
		for (int i = 0; i < 10; ++i) {
			UUID contextId = i == 0 ? parentId : UUID.randomUUID();
			assert contextId != null;
			OpenEventSpace defaultSpace = mock(OpenEventSpace.class);
			if (i == 0) {
				this.defaultSpace = defaultSpace;
			}
			final SpaceID spaceId = new SpaceID(contextId, UUID.randomUUID(), EventSpaceSpecification.class);
			when(defaultSpace.getSpaceID()).thenReturn(spaceId);
			AgentContext c = mock(AgentContext.class);
			when(c.getID()).thenReturn(contextId);
			when(c.getDefaultSpace()).thenReturn(defaultSpace);
			contextsList.add(c);
		}
		this.contexts = Collections3.synchronizedCollection(contextsList, NoReadWriteLock.SINGLETON);
		this.agent = new TestAgent(this);
		this.agent = spy(this.agent);
		when(this.agent.getParentID()).thenReturn(parentId);

		this.skill = this.reflect.newInstance(ExternalContextAccessSkill.class, this.agent);

		MockitoAnnotations.initMocks(this);

		when(this.contextRepository.getLock()).thenReturn(NoReadWriteLock.SINGLETON);
		when(this.contextRepository.getContexts()).thenReturn(this.contexts);
		when(this.contextRepository.getContexts(ArgumentMatchers.anyCollection())).then(new Answer<Collection>() {
			@Override
			public Collection answer(InvocationOnMock invocation) throws Throwable {
				Collection<UUID> ids = (Collection<UUID>) invocation.getArguments()[0];
				List<AgentContext> l = new ArrayList<>();
				for (AgentContext ctx : ExternalContextAccessSkillTest.this.contexts) {
					if (ids.contains(ctx.getID())) {
						l.add(ctx);
					}
				}
				return Collections3.unmodifiableSynchronizedCollection(l, NoReadWriteLock.SINGLETON);
			}
		});
		when(this.contextRepository.getContext(ArgumentMatchers.any(UUID.class))).then(new Answer<AgentContext>() {
			@Override
			public AgentContext answer(InvocationOnMock invocation) throws Throwable {
				UUID id = (UUID) invocation.getArguments()[0];
				for (AgentContext ctx : ExternalContextAccessSkillTest.this.contexts) {
					if (id.equals(ctx.getID())) {
						return ctx;
					}
				}
				return null;
			}
		});
	}

	@Test
	public void getAllContexts() {
		SynchronizedCollection<AgentContext> c = this.skill.getAllContexts();
		assertTrue(c.isEmpty());
	}

	@Test(expected = IllegalArgumentException.class)
	public void getContext() {
		for (AgentContext c : this.contexts) {
			this.skill.getContext(c.getID());
		}
	}

	@Test
	public void join() {
		int nb = 0;
		for (AgentContext c : this.contexts) {
			final SpaceID spaceId = c.getDefaultSpace().getSpaceID();
			final UUID id = spaceId.getID();
			this.skill.join(c.getID(), id);
			//
			AgentContext ctx = this.skill.getContext(c.getID());
			assertSame(c, ctx);
			//
			verify(c.getDefaultSpace(), times(3)).getSpaceID();
			ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
			ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
			ArgumentCaptor<Scope<Address>> argument3 = ArgumentCaptor.forClass(Scope.class);
			verify(c.getDefaultSpace(), times(1)).emit(argument1.capture(), argument2.capture(), argument3.capture());
			assertNull(argument1.getValue());
			Event evt = argument2.getValue();
			assertNotNull(evt);
			assertTrue(evt instanceof MemberJoined);
			assertEquals(this.skill.getOwner().getID(), ((MemberJoined) evt).getSource().getUUID());
			assertEquals(this.agent.getID(), ((MemberJoined) evt).agentID);
			assertNotNull(argument3.getValue());
			//
			ArgumentCaptor<Event> argument4 = ArgumentCaptor.forClass(Event.class);
			ArgumentCaptor<Scope<Address>> argument5 = ArgumentCaptor.forClass(Scope.class);
			++nb;
			verify(this.behaviorCapacity, times(nb)).wake(argument4.capture(), argument5.capture());
			evt = argument4.getValue();
			assertNotNull(evt);
			assertTrue(evt instanceof ContextJoined);
			assertEquals(c.getID(), ((ContextJoined) evt).holonContextID);
			assertEquals(c.getDefaultSpace().getSpaceID().getID(), ((ContextJoined) evt).defaultSpaceID);
			assertNotNull(argument5.getValue());
		}
		Collection<AgentContext> c = this.skill.getAllContexts();
		assertEquals(this.contexts.size(), c.size());
		for (AgentContext ctx : c) {
			assertTrue(this.contexts.contains(ctx));
		}
	}

	@Test
	public void leave() {
		int nb = 0;
		for (AgentContext c : this.contexts) {
			this.skill.join(c.getID(), c.getDefaultSpace().getSpaceID().getID());
			++nb;
		}
		//
		List<AgentContext> remaining = new ArrayList<>(this.contexts);
		for (AgentContext c : this.contexts) {
			this.skill.leave(c.getID());
			//
			remaining.remove(c);
			Collection<AgentContext> in = this.skill.getAllContexts();
			assertEquals(remaining.size(), in.size());
			for (AgentContext ctx : in) {
				assertTrue(remaining.contains(ctx));
			}
			//
			ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
			ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
			ArgumentCaptor<Scope<Address>> argument3 = ArgumentCaptor.forClass(Scope.class);
			// 2 times: 1 for MemberJoined, 1 for MemberLeft
			verify(c.getDefaultSpace(), times(2)).emit(argument1.capture(), argument2.capture(), argument3.capture());
			assertNull(argument1.getValue());
			Event evt = argument2.getValue();
			assertNotNull(evt);
			assertTrue(evt instanceof MemberLeft);
			assertEquals(this.agent.getID(), ((MemberLeft) evt).agentID);
			assertNotNull(argument3.getValue());
			//
			ArgumentCaptor<Event> argument4 = ArgumentCaptor.forClass(Event.class);
			ArgumentCaptor<Scope<Address>> argument5 = ArgumentCaptor.forClass(Scope.class);
			++nb;
			// Nb times includes the joins and the leaves
			verify(this.behaviorCapacity, times(nb)).wake(argument4.capture(), argument5.capture());
			evt = argument4.getValue();
			assertNotNull(evt);
			assertTrue(evt instanceof ContextLeft);
			assertEquals(c.getID(), ((ContextLeft) evt).holonContextID);
			assertNotNull(argument5.getValue());
		}
		assertTrue(remaining.isEmpty());
	}

	@Test
	public void install() throws Exception {
		assertNull(this.defaultSpace.getAddress(this.agent.getID()));
		this.reflect.invoke(this.skill, "install");
		ArgumentCaptor<EventListener> argument = ArgumentCaptor.forClass(EventListener.class);
		verify(this.defaultSpace, times(1)).register(argument.capture());
		assertSame(this.eventListener, argument.getValue());
	}

	@Test
	public void uninstall_Pre() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.reflect.invoke(this.skill, "uninstall", UninstallationStage.PRE_DESTROY_EVENT);
		ArgumentCaptor<EventListener> argument = ArgumentCaptor.forClass(EventListener.class);
		verify(this.defaultSpace, never()).unregister(argument.capture());
	}

	@Test
	public void uninstall_Post() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.reflect.invoke(this.skill, "uninstall", UninstallationStage.POST_DESTROY_EVENT);
		ArgumentCaptor<EventListener> argument = ArgumentCaptor.forClass(EventListener.class);
		verify(this.defaultSpace, times(1)).unregister(argument.capture());
		assertSame(this.eventListener, argument.getValue());
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventSpace_null_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(null, (Space) null);
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventSpace_notNull_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(mock(Event.class), (Space) null);
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventSpace_null_notNull() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(null, mock(Space.class));
	}

	@Test
	public void isInSpaceEventSpace_inside() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		SpaceID spaceID = mock(SpaceID.class);
		when(spaceID.getID()).thenReturn(id);
		Space space = mock(Space.class);
		when(space.getSpaceID()).thenReturn(spaceID);
		Event event = mock(Event.class);
		Address address = mock(Address.class);
		when(address.getSpaceID()).thenReturn(spaceID);
		when(event.getSource()).thenReturn(address);
		//
		assertTrue(this.skill.isInSpace(event, space));
	}

	@Test
	public void isInSpaceEventSpace_outside() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		SpaceID spaceID = mock(SpaceID.class);
		when(spaceID.getID()).thenReturn(id);
		Space space = mock(Space.class);
		when(space.getSpaceID()).thenReturn(spaceID);
		Event event = mock(Event.class);
		SpaceID spaceID2 = mock(SpaceID.class);
		when(spaceID2.getID()).thenReturn(UUID.randomUUID());
		Address address = mock(Address.class);
		when(address.getSpaceID()).thenReturn(spaceID2);
		when(event.getSource()).thenReturn(address);
		//
		assertFalse(this.skill.isInSpace(event, space));
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventSpaceID_null_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(null, (SpaceID) null);
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventSpaceID_notNull_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(mock(Event.class), (SpaceID) null);
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventSpaceID_null_notNull() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(null, mock(SpaceID.class));
	}

	@Test
	public void isInSpaceEventSpaceID_inside() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		SpaceID spaceID = mock(SpaceID.class);
		when(spaceID.getID()).thenReturn(id);
		Event event = mock(Event.class);
		Address address = mock(Address.class);
		when(address.getSpaceID()).thenReturn(spaceID);
		when(event.getSource()).thenReturn(address);
		//
		assertTrue(this.skill.isInSpace(event, spaceID));
	}

	@Test
	public void isInSpaceEventSpaceID_outside() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		SpaceID spaceID = mock(SpaceID.class);
		when(spaceID.getID()).thenReturn(id);
		Event event = mock(Event.class);
		SpaceID spaceID2 = mock(SpaceID.class);
		when(spaceID2.getID()).thenReturn(UUID.randomUUID());
		Address address = mock(Address.class);
		when(address.getSpaceID()).thenReturn(spaceID2);
		when(event.getSource()).thenReturn(address);
		//
		assertFalse(this.skill.isInSpace(event, spaceID));
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventUUID_null_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(null, (UUID) null);
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventUUID_notNull_null() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(mock(Event.class), (UUID) null);
	}

	@Test(expected = NullPointerException.class)
	public void isInSpaceEventUUID_null_notNull() throws Exception {
		this.reflect.invoke(this.skill, "install");
		this.skill.isInSpace(null, UUID.randomUUID());
	}

	@Test
	public void isInSpaceEventUUID_inside() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		SpaceID spaceID = mock(SpaceID.class);
		when(spaceID.getID()).thenReturn(id);
		Event event = mock(Event.class);
		Address address = mock(Address.class);
		when(address.getSpaceID()).thenReturn(spaceID);
		when(event.getSource()).thenReturn(address);
		//
		assertTrue(this.skill.isInSpace(event, id));
	}

	@Test
	public void isInSpaceEventUUID_outside() throws Exception {
		this.reflect.invoke(this.skill, "install");
		UUID id = UUID.randomUUID();
		Event event = mock(Event.class);
		SpaceID spaceID2 = mock(SpaceID.class);
		when(spaceID2.getID()).thenReturn(UUID.randomUUID());
		Address address = mock(Address.class);
		when(address.getSpaceID()).thenReturn(spaceID2);
		when(event.getSource()).thenReturn(address);
		//
		assertFalse(this.skill.isInSpace(event, id));
	}

	@Test
	public void emitEventScope() throws Exception {
		final UUID contextId = UUID.randomUUID();
		OpenEventSpace space = mock(OpenEventSpace.class);
		when(space.getSpaceID()).thenReturn(new SpaceID(contextId, UUID.randomUUID(), EventSpaceSpecification.class));
		
		this.reflect.invoke(this.skill, "install");
		Event event = mock(Event.class);
		Scope<Address> scope = Scopes.allParticipants();
		this.skill.emit(space, event, scope);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope> argument3 = ArgumentCaptor.forClass(Scope.class);
		verify(space, times(1)).emit(argument1.capture(), argument2.capture(), argument3.capture());
		assertEquals(this.agent.getID(), argument1.getValue());
		assertSame(event, argument2.getValue());
		assertSame(scope, argument3.getValue());
	}

	@Test
	public void emitEvent() throws Exception {
		final UUID contextId = UUID.randomUUID();
		OpenEventSpace space = mock(OpenEventSpace.class);
		when(space.getSpaceID()).thenReturn(new SpaceID(contextId, UUID.randomUUID(), EventSpaceSpecification.class));

		this.reflect.invoke(this.skill, "install");
		Event event = mock(Event.class);
		this.skill.emit(space, event);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument2 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope> argument3 = ArgumentCaptor.forClass(Scope.class);
		verify(space, times(1)).emit(argument1.capture(), argument2.capture(), argument3.capture());
		assertEquals(this.agent.getID(), argument1.getValue());
		assertSame(event, argument2.getValue());
		assertNull(argument3.getValue());
	}

	public static class TestAgent extends Agent {

		private final ExternalContextAccessSkillTest test;

		public TestAgent(ExternalContextAccessSkillTest test) {
			super(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null);
			this.test = test;
		}

		@Override
		protected ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
			if (Behaviors.class.equals(capacity))
				return new ClearableReference(this.test.behaviorCapacity);
			return new ClearableReference(this.test.busCapacity);
		}

	}
}
