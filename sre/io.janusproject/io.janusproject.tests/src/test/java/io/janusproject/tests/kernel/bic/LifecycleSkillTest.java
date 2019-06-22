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

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.UUID;

import org.hamcrest.core.IsInstanceOf;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import io.janusproject.kernel.bic.AsynchronousAgentKillingEvent;
import io.janusproject.kernel.bic.DefaultContextInteractionsSkill;
import io.janusproject.kernel.bic.InternalEventBusCapacity;
import io.janusproject.kernel.bic.InternalEventBusSkill;
import io.janusproject.kernel.bic.LifecycleSkill;
import io.janusproject.services.executor.EarlyExitException;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.ClearableReference;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class LifecycleSkillTest extends AbstractJanusTest {

	@Nullable
	private UUID agentID;

	@Mock
	private SpawnService spawnService;

	@Mock
	private InternalEventBusSkill eventBus;

	@Mock
	private AgentContext context;

	@Mock
	private DefaultContextInteractionsSkill defaultContextInteractions;

	private LifecycleSkill skill;

	@Before
	public void setUp() throws Exception {
		this.agentID = UUID.randomUUID();
		Mockito.doReturn(this.context).when(this.defaultContextInteractions).getDefaultContext();
		Agent agent = new Agent(Mockito.mock(BuiltinCapacitiesProvider.class), UUID.randomUUID(), null) {
			@Override
			protected ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity) {
				if (InternalEventBusCapacity.class.equals(capacity)) {
					return new ClearableReference<>(LifecycleSkillTest.this.eventBus);
				}
				if (DefaultContextInteractions.class.equals(capacity)) {
					return new ClearableReference<>(LifecycleSkillTest.this.defaultContextInteractions);
				}
				return null;
			}

			@Override
			public UUID getID() {
				return LifecycleSkillTest.this.agentID;
			}
		};
		this.skill = this.reflect.newInstance(LifecycleSkill.class, agent);
		this.reflect.set(this.skill, "spawnService", this.spawnService);
	}

	@Test
	public void spawn_1() throws Exception {
		Class type = Agent.class;
		this.skill.spawn(type, 1, "String"); //$NON-NLS-1$
		ArgumentCaptor<Integer> argument0 = ArgumentCaptor.forClass(Integer.class);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<AgentContext> argument2 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<UUID> argument3 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Class> argument4 = ArgumentCaptor.forClass(Class.class);
		ArgumentCaptor<Object> argument5 = ArgumentCaptor.forClass(Object.class);
		verify(this.spawnService, times(1)).spawn(argument0.capture(), argument1.capture(), argument2.capture(),
				argument3.capture(), argument4.capture(), argument5.capture());
		assertEquals(1, argument0.getValue().intValue());
		assertSame(this.agentID, argument1.getValue());
		assertSame(this.context, argument2.getValue());
		assertNull(argument3.getValue());
		assertEquals(Agent.class, argument4.getValue());
		assertArrayEquals(new Object[] { 1, "String" }, argument5.getAllValues().toArray()); //$NON-NLS-1$
	}

	@Test
	public void spawnInContext_1() {
		Class type = Agent.class;
		this.skill.spawnInContext(type, this.context, 1, "String"); //$NON-NLS-1$
		ArgumentCaptor<Integer> argument0 = ArgumentCaptor.forClass(Integer.class);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<AgentContext> argument2 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<UUID> argument3 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Class> argument4 = ArgumentCaptor.forClass(Class.class);
		ArgumentCaptor<Object> argument5 = ArgumentCaptor.forClass(Object.class);
		verify(this.spawnService, times(1)).spawn(argument0.capture(), argument1.capture(), argument2.capture(),
				argument3.capture(), argument4.capture(), argument5.capture());
		assertEquals(1, argument0.getValue().intValue());
		assertEquals(this.agentID, argument1.getValue());
		assertSame(this.context, argument2.getValue());
		assertNull(argument3.getValue());
		assertEquals(Agent.class, argument4.getValue());
		assertArrayEquals(new Object[] { 1, "String" }, argument5.getAllValues().toArray()); //$NON-NLS-1$
	}

	@Test
	public void spawnInContextWithID_1() {
		Class type = Agent.class;
		// Class<? extends Agent> agentClass, UUID agentID, AgentContext context, Object... 
		this.skill.spawnInContextWithID(type, this.agentID, this.context, 1, "String"); //$NON-NLS-1$
		ArgumentCaptor<Integer> argument0 = ArgumentCaptor.forClass(Integer.class);
		ArgumentCaptor<UUID> argument1 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<AgentContext> argument2 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<UUID> argument3 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Class> argument4 = ArgumentCaptor.forClass(Class.class);
		ArgumentCaptor<Object> argument5 = ArgumentCaptor.forClass(Object.class);
		verify(this.spawnService, times(1)).spawn(argument0.capture(), argument1.capture(), argument2.capture(),
				argument3.capture(), argument4.capture(), argument5.capture());
		assertEquals(1, argument0.getValue().intValue());
		assertSame(this.agentID, argument1.getValue());
		assertSame(this.context, argument2.getValue());
		assertSame(this.agentID, argument3.getValue());
		assertEquals(Agent.class, argument4.getValue());
		assertArrayEquals(new Object[] { 1, "String" }, argument5.getAllValues().toArray()); //$NON-NLS-1$
	}

	@Test
	public void killMe() throws Exception {
		try {
			this.skill.killMe();
			fail("killMe() must never return!"); //$NON-NLS-1$
		} catch (EarlyExitException exception) {
			// Expected exception
		} catch (Exception e) {
			throw e;
		}
		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		verify(this.eventBus, times(1)).selfEvent(argument.capture());
		assertThat(argument.getValue(), IsInstanceOf.instanceOf(AsynchronousAgentKillingEvent.class));
	}

}
