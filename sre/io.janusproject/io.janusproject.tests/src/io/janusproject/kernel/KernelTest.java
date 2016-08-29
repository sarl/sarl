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
package io.janusproject.kernel;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.Thread.UncaughtExceptionHandler;
import java.lang.reflect.Method;
import java.util.UUID;

import org.eclipse.jdt.annotation.Nullable;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Matchers;

import com.google.common.collect.ImmutableMultimap;
import com.google.common.util.concurrent.Service;
import com.google.common.util.concurrent.Service.State;

import io.janusproject.services.IServiceManager;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.testutils.AbstractJanusTest;
import io.janusproject.util.TwoStepConstruction;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import javassist.Modifier;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class KernelTest extends AbstractJanusTest {

	@Nullable
	private ImmutableMultimap<State, Service> services;

	@Nullable
	private SpawnService spawnService;

	@Nullable
	private ExecutorService executorService;

	@Nullable
	private LogService loggingService;

	@Nullable
	private ContextSpaceService contextService;

	@Nullable
	private IServiceManager serviceManager;

	@Nullable
	private UncaughtExceptionHandler exceptionHandler;

	@Nullable
	private AgentContext agentContext;

	@Nullable
	private Kernel kernel;

	@Nullable
	private UUID uuid;

	@Before
	public void setUp() {
		this.uuid = UUID.randomUUID();
		this.spawnService = mock(SpawnService.class);
		this.executorService = mock(ExecutorService.class);
		this.loggingService = mock(LogService.class);
		this.contextService = mock(ContextSpaceService.class);
		this.services = ImmutableMultimap.<State, Service> of(State.RUNNING, this.spawnService, State.RUNNING,
				this.executorService, State.RUNNING, this.contextService);
		this.agentContext = mock(AgentContext.class);
		this.exceptionHandler = mock(UncaughtExceptionHandler.class);
		this.serviceManager = mock(IServiceManager.class);
		//
		when(this.spawnService.isRunning()).thenReturn(true);
		when(this.spawnService.state()).thenReturn(State.RUNNING);
		when(this.spawnService.spawn(Matchers.any(AgentContext.class), Matchers.any(UUID.class), Matchers.any(Class.class),
				Matchers.anyVararg())).thenReturn(this.uuid);
		when(this.executorService.isRunning()).thenReturn(true);
		when(this.executorService.state()).thenReturn(State.RUNNING);
		when(this.contextService.isRunning()).thenReturn(true);
		when(this.contextService.state()).thenReturn(State.RUNNING);
		when(this.serviceManager.servicesByState()).thenReturn(this.services);
		//
		this.kernel = new Kernel(this.serviceManager, this.spawnService, this.loggingService, this.exceptionHandler);
		this.kernel = spy(this.kernel);
	}

	@Test
	public void getService() {
		assertSame(this.spawnService, this.kernel.getService(SpawnService.class));
		assertSame(this.executorService, this.kernel.getService(ExecutorService.class));
		assertSame(this.contextService, this.kernel.getService(ContextSpaceService.class));
	}

	@Test
	public void spawn() {
		this.kernel.setJanusContext(this.agentContext);
		//
		UUID id = this.kernel.spawn(Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		assertSame(this.uuid, id);
		ArgumentCaptor<AgentContext> argument1 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<UUID> argument2 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Class> argument3 = ArgumentCaptor.forClass(Class.class);
		ArgumentCaptor<String> argument4 = ArgumentCaptor.forClass(String.class);
		verify(this.spawnService).spawn(argument1.capture(), argument2.capture(), argument3.capture(), argument4.capture());
		assertSame(this.agentContext, argument1.getValue());
		assertNull(argument2.getValue());
		assertEquals(Agent.class, argument3.getValue());
		assertArrayEquals(new String[] { "a", "b" }, argument4.getAllValues().toArray()); //$NON-NLS-1$//$NON-NLS-2$
	}

	@Test
	public void spawnWithAgentId() {
		this.kernel.setJanusContext(this.agentContext);
		//
		UUID aId = UUID.fromString(this.uuid.toString());
		UUID id = this.kernel.spawn(aId, Agent.class, "a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		assertSame(this.uuid, id);
		assertEquals(aId, id);
		ArgumentCaptor<AgentContext> argument1 = ArgumentCaptor.forClass(AgentContext.class);
		ArgumentCaptor<UUID> argument2 = ArgumentCaptor.forClass(UUID.class);
		ArgumentCaptor<Class> argument3 = ArgumentCaptor.forClass(Class.class);
		ArgumentCaptor<String> argument4 = ArgumentCaptor.forClass(String.class);
		verify(this.spawnService).spawn(argument1.capture(), argument2.capture(), argument3.capture(), argument4.capture());
		assertSame(this.agentContext, argument1.getValue());
		assertSame(aId, argument2.getValue());
		assertEquals(Agent.class, argument3.getValue());
		assertArrayEquals(new String[] { "a", "b" }, argument4.getAllValues().toArray()); //$NON-NLS-1$//$NON-NLS-2$
	}

	@Test
	public void twoStepConstruction() throws Exception {
		TwoStepConstruction annotation = Kernel.class.getAnnotation(TwoStepConstruction.class);
		assertNotNull(annotation);
		for (String name : annotation.names()) {
			for (Method method : Kernel.class.getMethods()) {
				if (name.equals(method.getName())) {
					assertTrue(Modifier.isPackage(method.getModifiers()) || Modifier.isPublic(method.getModifiers()));
					break;
				}
			}
		}
	}

}
