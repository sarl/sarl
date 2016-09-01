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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.UUID;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import io.janusproject.kernel.bic.SchedulesSkill;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.logging.LogService;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.verification.Times;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.sarl.core.AgentTask;
import io.sarl.lang.core.Agent;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SchedulesSkillTest extends AbstractJanusTest {

	@Nullable
	private UUID agentId;

	@Mock
	private ExecutorService executorService;

	@Mock
	private Agent agent;

	@Mock
	private LogService logger;

	@InjectMocks
	private SchedulesSkill skill;

	@Before
	public void setUp() throws Exception {
		this.agentId = UUID.randomUUID();
		Mockito.when(this.agent.getID()).thenReturn(this.agentId);
		Mockito.when(this.executorService.schedule(ArgumentMatchers.any(Runnable.class), ArgumentMatchers.any(long.class),
				ArgumentMatchers.any(TimeUnit.class))).thenAnswer(new Answer<ScheduledFuture>() {
					@Override
					public ScheduledFuture answer(InvocationOnMock invocation) throws Throwable {
						ScheduledFuture f = Mockito.mock(ScheduledFuture.class);
						Mockito.when(f.isDone()).thenReturn(false);
						Mockito.when(f.isCancelled()).thenReturn(false);
						Mockito.when(f.cancel(ArgumentMatchers.anyBoolean())).thenReturn(true);
						return f;
					}
				});
		Mockito.when(this.executorService.scheduleAtFixedRate(ArgumentMatchers.any(Runnable.class), ArgumentMatchers.any(long.class),
				ArgumentMatchers.any(long.class), ArgumentMatchers.any(TimeUnit.class))).thenAnswer(new Answer<ScheduledFuture>() {
					@Override
					public ScheduledFuture answer(InvocationOnMock invocation) throws Throwable {
						ScheduledFuture f = Mockito.mock(ScheduledFuture.class);
						Mockito.when(f.isDone()).thenReturn(false);
						Mockito.when(f.isCancelled()).thenReturn(false);
						Mockito.when(f.cancel(ArgumentMatchers.anyBoolean())).thenReturn(true);
						return f;
					}
				});
	}

	@Test
	public void task() {
		AgentTask task = this.skill.task("thename"); //$NON-NLS-1$
		assertNotNull(task);
		assertNotNull(task.getGuard());
		assertNull(task.getProcedure());
		assertEquals("thename", task.getName()); //$NON-NLS-1$
		//
		AgentTask task2 = this.skill.task("thename"); //$NON-NLS-1$
		assertSame(task, task2);
		//
		AgentTask task3 = this.skill.task("thename2"); //$NON-NLS-1$
		assertNotSame(task, task3);
	}

	@Test
	public void inLongProcedure1() {
		Procedure1 procedure = Mockito.mock(Procedure1.class);
		AgentTask task = this.skill.in(5, procedure);
		assertNotNull(task);
		assertSame(procedure, task.getProcedure());
		ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
		ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<TimeUnit> argument3 = ArgumentCaptor.forClass(TimeUnit.class);
		Mockito.verify(this.executorService, new Times(1)).schedule(argument1.capture(), argument2.capture(),
				argument3.capture());
		assertNotNull(argument1.getValue());
		assertEquals(new Long(5), argument2.getValue());
		assertSame(TimeUnit.MILLISECONDS, argument3.getValue());
	}

	@Test
	public void inAgentTaskLongProcedure1() {
		AgentTask task = Mockito.mock(AgentTask.class);
		Mockito.when(task.getName()).thenReturn("thetask"); //$NON-NLS-1$
		Procedure1 procedure = Mockito.mock(Procedure1.class);
		AgentTask t = this.skill.in(task, 5, procedure);
		assertSame(task, t);
		ArgumentCaptor<Procedure1> argument0 = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(task, new Times(1)).setProcedure(argument0.capture());
		assertSame(procedure, argument0.getValue());
		ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
		ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<TimeUnit> argument3 = ArgumentCaptor.forClass(TimeUnit.class);
		Mockito.verify(this.executorService, new Times(1)).schedule(argument1.capture(), argument2.capture(),
				argument3.capture());
		assertNotNull(argument1.getValue());
		assertEquals(new Long(5), argument2.getValue());
		assertSame(TimeUnit.MILLISECONDS, argument3.getValue());
	}

	@Test
	public void everyLongProcedure1() {
		Procedure1 procedure = Mockito.mock(Procedure1.class);
		AgentTask task = this.skill.every(5, procedure);
		assertNotNull(task);
		assertSame(procedure, task.getProcedure());
		ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
		ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<Long> argument3 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<TimeUnit> argument4 = ArgumentCaptor.forClass(TimeUnit.class);
		Mockito.verify(this.executorService, new Times(1)).scheduleAtFixedRate(argument1.capture(), argument2.capture(),
				argument3.capture(), argument4.capture());
		assertNotNull(argument1.getValue());
		assertEquals(new Long(0), argument2.getValue());
		assertEquals(new Long(5), argument3.getValue());
		assertSame(TimeUnit.MILLISECONDS, argument4.getValue());
	}

	@Test
	public void everyAgentTaskLongProcedure1() {
		AgentTask task = Mockito.mock(AgentTask.class);
		Mockito.when(task.getName()).thenReturn("thetask"); //$NON-NLS-1$
		Procedure1 procedure = Mockito.mock(Procedure1.class);
		AgentTask t = this.skill.every(task, 5, procedure);
		assertSame(task, t);
		ArgumentCaptor<Procedure1> argument0 = ArgumentCaptor.forClass(Procedure1.class);
		Mockito.verify(task, new Times(1)).setProcedure(argument0.capture());
		assertSame(procedure, argument0.getValue());
		ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
		ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<Long> argument3 = ArgumentCaptor.forClass(Long.class);
		ArgumentCaptor<TimeUnit> argument4 = ArgumentCaptor.forClass(TimeUnit.class);
		Mockito.verify(this.executorService, new Times(1)).scheduleAtFixedRate(argument1.capture(), argument2.capture(),
				argument3.capture(), argument4.capture());
		assertNotNull(argument1.getValue());
		assertEquals(new Long(0), argument2.getValue());
		assertEquals(new Long(5), argument3.getValue());
		assertSame(TimeUnit.MILLISECONDS, argument4.getValue());
	}

	@Test
	public void uninstall() throws Exception {
		Procedure1 procedure1 = Mockito.mock(Procedure1.class);
		this.skill.every(5, procedure1);
		Procedure1 procedure2 = Mockito.mock(Procedure1.class);
		this.skill.in(5, procedure2);
		Collection<ScheduledFuture<?>> futures = (Collection<ScheduledFuture<?>>) this.reflect.invoke(this.skill, "getActiveFutures");
		assertEquals(2, futures.size());
		//
		this.reflect.invoke(this.skill, "uninstall");
		//
		Collection<String> activeTasks = (Collection<String>) this.reflect.invoke(this.skill, "getActiveTasks");
		assertTrue(activeTasks.isEmpty());
		for (ScheduledFuture<?> f : futures) {
			Mockito.verify(f, new Times(1)).cancel(ArgumentMatchers.anyBoolean());
		}
	}

	@Test
	public void cancelAgentTask() throws Exception {
		Procedure1 procedure1 = Mockito.mock(Procedure1.class);
		AgentTask t1 = this.skill.every(5, procedure1);
		Procedure1 procedure2 = Mockito.mock(Procedure1.class);
		AgentTask t2 = this.skill.in(5, procedure2);
		Collection<ScheduledFuture<?>> futures = (Collection<ScheduledFuture<?>>) this.reflect.invoke(this.skill, "getActiveFutures");
		assertEquals(2, futures.size());
		//
		this.skill.cancel(t2);
		this.skill.cancel(t1);
		//
		Collection<String> activeTasks = (Collection<String>) this.reflect.invoke(this.skill, "getActiveTasks");
		assertTrue(activeTasks.isEmpty());
		for (ScheduledFuture<?> f : futures) {
			Mockito.verify(f, new Times(1)).cancel(ArgumentMatchers.anyBoolean());
		}
	}

	@Test
	public void cancelAgentTaskBoolean_true() throws Exception {
		Procedure1 procedure1 = Mockito.mock(Procedure1.class);
		AgentTask t1 = this.skill.every(5, procedure1);
		Procedure1 procedure2 = Mockito.mock(Procedure1.class);
		AgentTask t2 = this.skill.in(5, procedure2);
		Collection<ScheduledFuture<?>> futures = (Collection<ScheduledFuture<?>>) this.reflect.invoke(this.skill, "getActiveFutures");
		assertEquals(2, futures.size());
		//
		this.skill.cancel(t2, true);
		this.skill.cancel(t1, true);
		//
		Collection<String> activeTasks = (Collection<String>) this.reflect.invoke(this.skill, "getActiveTasks");
		assertTrue(activeTasks.isEmpty());
		for (ScheduledFuture<?> f : futures) {
			Mockito.verify(f, new Times(1)).cancel(ArgumentMatchers.anyBoolean());
		}
	}

	@Test
	public void cancelAgentTaskBoolean_false() throws Exception {
		Procedure1 procedure1 = Mockito.mock(Procedure1.class);
		AgentTask t1 = this.skill.every(5, procedure1);
		Procedure1 procedure2 = Mockito.mock(Procedure1.class);
		AgentTask t2 = this.skill.in(5, procedure2);
		Collection<ScheduledFuture<?>> futures = (Collection<ScheduledFuture<?>>) this.reflect.invoke(this.skill, "getActiveFutures");
		assertEquals(2, futures.size());
		//
		this.skill.cancel(t2, false);
		this.skill.cancel(t1, false);
		//
		Collection<String> activeTasks = (Collection<String>) this.reflect.invoke(this.skill, "getActiveTasks");
		assertTrue(activeTasks.isEmpty());
		for (ScheduledFuture<?> f : futures) {
			Mockito.verify(f, new Times(1)).cancel(ArgumentMatchers.anyBoolean());
		}
	}

}
