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
package io.janusproject.tests.kernel.services.jdk.spawn;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.internal.verification.Times;

import io.janusproject.kernel.services.jdk.spawn.StandardSpawnService;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.spawn.KernelAgentSpawnListener;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.services.spawn.SpawnServiceListener;
import io.janusproject.tests.testutils.AbstractDependentServiceTest;
import io.janusproject.tests.testutils.AvoidServiceStartForTest;
import io.janusproject.tests.testutils.StartServiceForTest;

import io.sarl.core.AgentKilled;
import io.sarl.core.AgentSpawned;
import io.sarl.core.ExternalContextAccess;
import io.sarl.core.InnerContextAccess;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.tests.api.ManualMocking;
import io.sarl.tests.api.Nullable;
import io.sarl.util.Collections3;
import io.sarl.util.OpenEventSpace;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@StartServiceForTest(startAfterSetUp = true)
@ManualMocking
public class StandardSpawnServiceTest extends AbstractDependentServiceTest<StandardSpawnService> {

	@Nullable
	private BuiltinCapacitiesProvider builtinCapacitiesProvider;

	@Nullable
	private UUID agentId;

	@Nullable
	private OpenEventSpace innerSpace;

	@Mock
	private AgentContext innerContext;

	@Mock
	private AgentContext agentContext;

	@Mock
	private EventSpace defaultSpace;

	@Mock
	private KernelAgentSpawnListener kernelListener;

	@Mock
	private SpawnServiceListener serviceListener;

	private Injector injector;

	/**
	 */
	public StandardSpawnServiceTest() {
		super(SpawnService.class);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public StandardSpawnService newService() {
		this.builtinCapacitiesProvider = Mockito.mock(BuiltinCapacitiesProvider.class);
		if (this.injector == null) {
			this.injector = Guice.createInjector(new TestModule(this.builtinCapacitiesProvider));
		}
		return this.injector.getInstance(StandardSpawnService.class);
	}

	@Before
	public void setUp() throws Exception {
		UUID parentID = UUID.randomUUID();
		this.agentId = UUID.randomUUID();
		MockitoAnnotations.initMocks(this);
		this.innerSpace = Mockito.mock(OpenEventSpace.class);
		Mockito.when(this.innerSpace.getParticipants()).thenReturn(Collections3.synchronizedSingleton(this.agentId));
		Mockito.when(this.innerContext.getDefaultSpace()).thenReturn(this.innerSpace);
		Mockito.when(this.agentContext.getDefaultSpace()).thenReturn(this.defaultSpace);
		Mockito.when(this.agentContext.getID()).thenReturn(parentID);
		Mockito.when(this.defaultSpace.getAddress(ArgumentMatchers.any(UUID.class))).thenReturn(Mockito.mock(Address.class));

		Map<Class<? extends Capacity>, Skill> bic = new HashMap<>();

		InnerContextSkillMock innerContextSkill = Mockito.mock(InnerContextSkillMock.class);
		bic.put(InnerContextAccess.class, innerContextSkill);
		Mockito.when(innerContextSkill.getInnerContext()).thenReturn(this.innerContext);

		ExternalContextSkillMock externalContextSkill = Mockito.mock(ExternalContextSkillMock.class);
		bic.put(ExternalContextAccess.class, externalContextSkill);
		Mockito.when(externalContextSkill.getAllContexts())
				.thenReturn(Collections3.synchronizedCollection(Collections.singleton(this.agentContext), this));

		Mockito.when(this.builtinCapacitiesProvider.getBuiltinCapacities(Mockito.any())).thenReturn(bic);

		this.service.addKernelAgentSpawnListener(this.kernelListener);
		this.service.addSpawnServiceListener(this.serviceListener);
	}

	@Override
	public void getServiceDependencies() {
		assertContains(this.service.getServiceDependencies(), ContextSpaceService.class);
	}

	@Override
	public void getServiceWeakDependencies() {
		assertContains(this.service.getServiceWeakDependencies());
	}

	@Test
	public void getAgents() {
		SynchronizedSet<UUID> agents = this.service.getAgents();
		assertNotNull(agents);
		assertTrue(agents.isEmpty());
	}

	@Test
	public void spawn_notNull_1() throws Exception {
		UUID aId = UUID.fromString(this.agentId.toString());
		List<UUID> agentIds = this.service.spawn(1, this.agentContext, aId, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		//
		assertNotNull(agentIds);
		assertEquals(1, agentIds.size());
		assertEquals(aId, agentIds.get(0));
		Set<UUID> agents = this.service.getAgents();
		assertEquals(1, agents.size());
		assertTrue(agents.contains(agentIds.get(0)));
		Agent spawnedAgent = (Agent) this.reflect.invoke(this.service, "getAgent", agentIds.get(0));
		assertNotNull(spawnedAgent);
		assertEquals(agentIds.get(0), spawnedAgent.getID());
		assertEquals(this.agentContext.getID(), spawnedAgent.getParentID());
		//
		ArgumentCaptor<AgentContext> argument1 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<List<Agent>> argument2 = ArgumentCaptor.forClass(List.class);
		ArgumentCaptor<Object[]> argument3 = ArgumentCaptor.forClass(Object[].class);
		Mockito.verify(this.serviceListener, new Times(1)).agentSpawned(argument1.capture(), argument2.capture(),
				argument3.capture());
		assertSame(this.agentContext, argument1.getValue());
		List<Agent> ags = argument2.getValue();
		assertNotNull(ags);
		assertEquals(1, ags.size());
		assertSame(agentIds.get(0), ags.get(0).getID());
		assertEquals("a", argument3.getValue()[0]); //$NON-NLS-1$
		assertEquals("b", argument3.getValue()[1]); //$NON-NLS-1$
		//
		ArgumentCaptor<Event> argument4 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.defaultSpace, new Times(1)).emit(argument4.capture());
		assertTrue(argument4.getValue() instanceof AgentSpawned);
		assertSame(agentIds.get(0), ((AgentSpawned) argument4.getValue()).agentID);
		assertEquals(ags.get(0).getClass().getName(), ((AgentSpawned) argument4.getValue()).agentType);
	}

	@Test
	public void spawn_null_1() throws Exception {
		List<UUID> agentIds = this.service.spawn(1, this.agentContext, null, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		//
		assertNotNull(agentIds);
		assertEquals(1, agentIds.size());
		Set<UUID> agents = this.service.getAgents();
		assertEquals(1, agents.size());
		assertTrue(agents.contains(agentIds.get(0)));
		Agent spawnedAgent = (Agent) this.reflect.invoke(this.service, "getAgent", agentIds.get(0));
		assertNotNull(spawnedAgent);
		assertEquals(agentIds.get(0), spawnedAgent.getID());
		assertEquals(this.agentContext.getID(), spawnedAgent.getParentID());
		//
		ArgumentCaptor<AgentContext> argument1 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<List<Agent>> argument2 = ArgumentCaptor.forClass(List.class);
		ArgumentCaptor<Object[]> argument3 = ArgumentCaptor.forClass(Object[].class);
		Mockito.verify(this.serviceListener, new Times(1)).agentSpawned(argument1.capture(), argument2.capture(),
				argument3.capture());
		assertSame(this.agentContext, argument1.getValue());
		List<Agent> ags = argument2.getValue();
		assertNotNull(ags);
		assertEquals(1, ags.size());
		assertSame(agentIds.get(0), ags.get(0).getID());
		assertEquals("a", argument3.getValue()[0]); //$NON-NLS-1$
		assertEquals("b", argument3.getValue()[1]); //$NON-NLS-1$
		//
		ArgumentCaptor<Event> argument4 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.defaultSpace, new Times(1)).emit(argument4.capture());
		assertTrue(argument4.getValue() instanceof AgentSpawned);
		assertSame(agentIds.get(0), ((AgentSpawned) argument4.getValue()).agentID);
		assertEquals(ags.getClass().getName(), ((AgentSpawned) argument4.getValue()).agentType);
	}

	@AvoidServiceStartForTest
	@Test
	public void canKillAgent_oneagentinsideinnercontext() throws Exception {
		Set<UUID> agIds = new HashSet<>();
		Mockito.when(this.defaultSpace.getParticipants()).thenReturn(Collections3.synchronizedSet(agIds, agIds));
		this.service.startAsync().awaitRunning();
		List<UUID> agentIds = this.service.spawn(1, this.agentContext, this.agentId, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		agIds.add(agentIds.get(0));
		Agent ag = (Agent) this.reflect.invoke(this.service, "getAgent", agentIds.get(0));
		assertNotNull(ag);
		// Only the super agent is inside the inner context => super agent could be killed
		assertTrue(this.service.canKillAgent(ag));
	}

	@AvoidServiceStartForTest
	@Test
	public void canKillAgent_twoagentsinsideinnercontext() throws Exception {
		Mockito.when(this.innerSpace.getParticipants())
				.thenReturn(Collections3.synchronizedSet(new HashSet<>(Arrays.asList(this.agentId, UUID.randomUUID())), this));
		Set<UUID> agIds = new HashSet<>();
		Mockito.when(this.defaultSpace.getParticipants()).thenReturn(Collections3.synchronizedSet(agIds, agIds));
		this.service.startAsync().awaitRunning();
		List<UUID> agentIds = this.service.spawn(1, this.agentContext, this.agentId, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		agIds.add(agentIds.get(0));
		Agent ag = (Agent) this.reflect.invoke(this.service, "getAgent", agentIds.get(0));
		assertNotNull(ag);
		// One agent other than the super agent is inside the inner context => super agent could not be killed
		assertFalse(this.service.canKillAgent(ag));
	}

	@Test
	public void killAgent() throws Exception {
		List<UUID> agentIds = this.service.spawn(1, this.agentContext, this.agentId, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		Agent ag = (Agent) this.reflect.invoke(this.service, "getAgent", agentIds.get(0));
		assertNotNull(ag);
		//
		this.service.killAgent(agentIds.get(0));
		//
		Set<UUID> agents = this.service.getAgents();
		assertTrue(agents.isEmpty());
		//
		ArgumentCaptor<Agent> argument4 = ArgumentCaptor.forClass(Agent.class);
		Mockito.verify(this.serviceListener, new Times(1)).agentDestroy(argument4.capture());
		assertSame(ag, argument4.getValue());
		//
		ArgumentCaptor<Event> argument5 = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.defaultSpace, new Times(2)).emit(argument5.capture());
		assertTrue(argument5.getValue() instanceof AgentKilled);
		assertEquals(agentIds, ((AgentKilled) argument5.getValue()).agentID);
		//
		Mockito.verify(this.kernelListener, new Times(1)).kernelAgentDestroy();
	}

	@AvoidServiceStartForTest
	@Test
	public void doStart() throws Exception {
		try {
			this.reflect.invoke(this.service, "doStart");
			fail("Expecting IllegalStateException"); //$NON-NLS-1$
		} catch (InvocationTargetException exception) {
			final Throwable ex = exception.getCause();
			if (!(ex instanceof IllegalStateException)) {
				fail("Expecting IllegalStateException"); //$NON-NLS-1$
			}
		}
		Mockito.verify(this.kernelListener, new Times(1)).kernelAgentSpawn();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class TestModule extends AbstractModule {

		private final BuiltinCapacitiesProvider builtinCapacitiesProvider;

		TestModule(BuiltinCapacitiesProvider builtinCapacitiesProvider) {
			this.builtinCapacitiesProvider = builtinCapacitiesProvider;
		}

		@Override
		protected void configure() {
			bind(BuiltinCapacitiesProvider.class).toInstance(this.builtinCapacitiesProvider);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static abstract class InnerContextSkillMock extends Skill implements InnerContextAccess {
		//
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static abstract class ExternalContextSkillMock extends Skill implements ExternalContextAccess {
		//
	}

}
