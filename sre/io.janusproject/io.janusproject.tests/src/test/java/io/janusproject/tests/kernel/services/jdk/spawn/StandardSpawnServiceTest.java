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

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.locks.ReadWriteLock;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Provider;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.internal.verification.Times;

import io.janusproject.kernel.services.jdk.spawn.StandardSpawnService;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.logging.LogService;
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
import io.sarl.core.OpenEventSpace;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.sarlspecification.SarlSpecificationChecker;
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
@StartServiceForTest(createAfterSetUp = false, startAfterSetUp = true)
@ManualMocking
public class StandardSpawnServiceTest extends AbstractDependentServiceTest<StandardSpawnService> {

	@Nullable
	private UUID agentId;

	@Nullable
	private UUID parentID;

	@Nullable
	private OpenEventSpace innerSpace;

	@Nullable
	private BuiltinCapacitiesProvider builtinCapacitiesProvider;

	@Nullable
	private Injector injector;

	@Nullable
	private Injector subInjector;

	@Nullable
	private SarlSpecificationChecker sarlSpecificationChecker;

	@Nullable
	private LogService logService;

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

	/**
	 */
	public StandardSpawnServiceTest() {
		super(SpawnService.class);
	}

	@Override
	public StandardSpawnService newService() {
		if (this.builtinCapacitiesProvider == null) {
			this.builtinCapacitiesProvider = mock(BuiltinCapacitiesProvider.class);
		}
		if (this.sarlSpecificationChecker == null) {
			this.sarlSpecificationChecker = mock(SarlSpecificationChecker.class);
			when(this.sarlSpecificationChecker.isValidSarlElement(ArgumentMatchers.any())).thenReturn(true);
		}
		if (this.subInjector == null) {
			this.subInjector = mock(Injector.class);
			when(this.subInjector.getInstance(ArgumentMatchers.any(Class.class))).then((it) -> {
				if (Agent.class.equals(it.getArgument(0))) {
					Agent agent = new Agent(this.builtinCapacitiesProvider, this.parentID,
							this.agentId);
					return spy(agent);
				}
				return null;
			});
		}
		if (this.injector == null) {
			this.injector = mock(Injector.class);
			when(this.injector.getProvider(any(Class.class))).thenAnswer((it) -> {
				Provider<?> provider = null;
				if (ReadWriteLock.class.equals(it.getArgument(0))) {
					provider = () -> NoReadWriteLock.SINGLETON;
				}
				return provider;
			});
			when(this.injector.createChildInjector(ArgumentMatchers.any(Module.class))).thenReturn(this.subInjector);
		}
		return new StandardSpawnService(this.injector, this.sarlSpecificationChecker);
	}

	@Before
	public void setUp() throws Exception {
		this.parentID = UUID.randomUUID();
		this.agentId = UUID.randomUUID();
		this.innerSpace = mock(OpenEventSpace.class);
		when(this.innerSpace.getParticipants()).thenReturn(Collections3.synchronizedSingleton(this.agentId));
		when(this.innerContext.getDefaultSpace()).thenReturn(this.innerSpace);
		when(this.agentContext.getDefaultSpace()).thenReturn(this.defaultSpace);
		when(this.agentContext.getID()).thenReturn(this.parentID);
		when(this.defaultSpace.getAddress(ArgumentMatchers.any(UUID.class))).thenReturn(mock(Address.class));
		SpaceID spaceID = mock(SpaceID.class);
		when(this.defaultSpace.getSpaceID()).thenReturn(spaceID);

		Map<Class<? extends Capacity>, Skill> bic = new HashMap<>();

		InnerContextSkillMock innerContextSkill = mock(InnerContextSkillMock.class);
		bic.put(InnerContextAccess.class, innerContextSkill);
		when(innerContextSkill.getInnerContext()).thenReturn(this.innerContext);

		ExternalContextSkillMock externalContextSkill = mock(ExternalContextSkillMock.class);
		bic.put(ExternalContextAccess.class, externalContextSkill);
		when(externalContextSkill.getAllContexts())
				.thenReturn(Collections3.synchronizedCollection(Collections.singleton(this.agentContext), NoReadWriteLock.SINGLETON));

		when(this.builtinCapacitiesProvider.getBuiltinCapacities(any())).thenReturn(bic);

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
	public void spawn_1agent() throws Exception {
		List<UUID> agentIds = this.service.spawn(1, this.parentID, this.agentContext,
				null, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
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
		ArgumentCaptor<UUID> argument0 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<AgentContext> argument1 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<List<Agent>> argument2 = ArgumentCaptor.forClass(List.class);
		ArgumentCaptor<Object[]> argument3 = ArgumentCaptor.forClass(Object[].class);
		verify(this.serviceListener, new Times(1)).agentSpawned(
				argument0.capture(),
				argument1.capture(), argument2.capture(),
				argument3.capture());
		assertEquals(this.parentID, argument0.getValue());
		assertSame(this.agentContext, argument1.getValue());
		List<Agent> ags = argument2.getValue();
		assertNotNull(ags);
		assertEquals(1, ags.size());
		assertSame(agentIds.get(0), ags.get(0).getID());
		assertEquals("a", argument3.getValue()[0]); //$NON-NLS-1$
		assertEquals("b", argument3.getValue()[1]); //$NON-NLS-1$
		//
		ArgumentCaptor<UUID> argument4 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument5 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope<Address>> argument6 = ArgumentCaptor.forClass(Scope.class);
		verify(this.defaultSpace, new Times(1)).emit(argument4.capture(), argument5.capture(), argument6.capture());
		assertNull(argument4.getValue());
		assertTrue(argument5.getValue() instanceof AgentSpawned);
		assertContainsCollection(((AgentSpawned) argument5.getValue()).agentIdentifiers, agentIds);
		assertNotNull(argument6.getValue());
	}

	@AvoidServiceStartForTest
	@Test
	public void canKillAgent_oneagentinsideinnercontext() throws Exception {
		Set<UUID> agIds = new HashSet<>();
		when(this.defaultSpace.getParticipants()).thenReturn(Collections3.synchronizedSet(agIds, NoReadWriteLock.SINGLETON));
		this.service.startAsync().awaitRunning();
		List<UUID> agentIds = this.service.spawn(1, this.parentID, this.agentContext, this.agentId, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		agIds.add(agentIds.get(0));
		Agent ag = (Agent) this.reflect.invoke(this.service, "getAgent", agentIds.get(0));
		assertNotNull(ag);
		// Only the super agent is inside the inner context => super agent could be killed
		assertTrue(this.service.canKillAgent(ag));
	}

	@AvoidServiceStartForTest
	@Test
	public void canKillAgent_twoagentsinsideinnercontext() throws Exception {
		when(this.innerSpace.getParticipants())
				.thenReturn(Collections3.synchronizedSet(new HashSet<>(Arrays.asList(this.agentId, UUID.randomUUID())), NoReadWriteLock.SINGLETON));
		Set<UUID> agIds = new HashSet<>();
		when(this.defaultSpace.getParticipants()).thenReturn(Collections3.synchronizedSet(agIds, NoReadWriteLock.SINGLETON));
		this.service.startAsync().awaitRunning();
		List<UUID> agentIds = this.service.spawn(1, this.parentID, this.agentContext, this.agentId, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		agIds.add(agentIds.get(0));
		Agent ag = (Agent) this.reflect.invoke(this.service, "getAgent", agentIds.get(0));
		assertNotNull(ag);
		// One agent other than the super agent is inside the inner context => super agent could not be killed
		assertFalse(this.service.canKillAgent(ag));
	}

	@Test
	public void killAgent() throws Exception {
		List<UUID> agentIds = this.service.spawn(1, this.parentID, this.agentContext, this.agentId, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		Agent ag = (Agent) this.reflect.invoke(this.service, "getAgent", agentIds.get(0));
		assertNotNull(ag);
		//
		this.service.killAgent(agentIds.get(0));
		//
		Set<UUID> agents = this.service.getAgents();
		assertTrue(agents.isEmpty());
		//
		ArgumentCaptor<Agent> argument4 = ArgumentCaptor.forClass(Agent.class);
		verify(this.serviceListener, new Times(1)).agentDestroy(argument4.capture());
		assertSame(ag, argument4.getValue());
		//
		ArgumentCaptor<UUID> argument5 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Event> argument6 = ArgumentCaptor.forClass(Event.class);
		ArgumentCaptor<Scope<Address>> argument7 = ArgumentCaptor.forClass(Scope.class);
		verify(this.defaultSpace, new Times(2)).emit(argument5.capture(), argument6.capture(), argument7.capture());
		assertNull(argument5.getValue());
		assertTrue(argument6.getValue() instanceof AgentKilled);
		assertEquals(agentIds.get(0), ((AgentKilled) argument6.getValue()).agentID);
		assertNotNull(argument7.getValue());
		//
		verify(this.kernelListener, new Times(1)).kernelAgentDestroy();
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
		verify(this.kernelListener, new Times(1)).kernelAgentSpawn();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class TestModule extends AbstractModule {

		private final BuiltinCapacitiesProvider builtinCapacitiesProvider;

		private final LogService logService;

		TestModule(BuiltinCapacitiesProvider builtinCapacitiesProvider, LogService logService) {
			this.builtinCapacitiesProvider = builtinCapacitiesProvider;
			this.logService = logService;
		}

		@Override
		protected void configure() {
			bind(BuiltinCapacitiesProvider.class).toInstance(this.builtinCapacitiesProvider);
			bind(LogService.class).toInstance(this.logService);
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
