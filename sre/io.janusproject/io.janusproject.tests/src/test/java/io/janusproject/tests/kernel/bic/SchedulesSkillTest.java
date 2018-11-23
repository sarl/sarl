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

import java.util.Collection;
import java.util.UUID;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.verification.Times;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.janusproject.kernel.bic.SchedulesSkill;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.logging.LogService;
import io.janusproject.tests.testutils.AbstractJanusRunTest;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.AgentTask;
import io.sarl.core.Behaviors;
import io.sarl.core.Destroy;
import io.sarl.core.Initialize;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.core.Time;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.SREutils;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.Skill.UninstallationStage;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SchedulesSkillTest.StaticTests.class,
	SchedulesSkillTest.RuntimeTests.class,
})
@SuppressWarnings("all")
public class SchedulesSkillTest {

	public static class StaticTests extends AbstractJanusTest {

		@Nullable
		private UUID agentId;

		@InjectMocks
		private SchedulesSkill skill;

		@Mock
		private ExecutorService executorService;

		@Mock
		private LogService logger;

		private Agent agent;

		private Skill timeSkill;

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		private static class TimeSkillMock extends Skill implements Time {
			
			public TimeSkillMock(Agent agent) {
				super(agent);
			}

			@Override
			public double getTime(TimeUnit timeUnit) {
				return 0;
			}

			@Override
			public double getOSTimeFactor() {
				return 1;
			}

			@Override
			public double toOSTime(double timeValue) {
				return timeValue;
			}

			@Override
			public double fromOSTime(double timeValue) {
				return timeValue;
			}

			@Override
			public double toOSDuration(double timeDuration) {
				return timeDuration;
			}

			@Override
			public double fromOSDuration(double timeDuration) {
				return timeDuration;
			}
			
		}
		
		@Before
		public void setUp() throws Exception {
			this.agentId = UUID.randomUUID();
			this.agent = new Agent(UUID.randomUUID(), this.agentId);
			this.agent = spy(this.agent);
			this.timeSkill = new TimeSkillMock(this.agent);
			this.timeSkill = spy(this.timeSkill);
			SREutils.createSkillMapping(this.agent, Time.class, this.timeSkill);
			this.reflect.invoke(this.skill, "setOwner", this.agent);
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
			assertSame(AgentTask.TRUE_GUARD, task.getGuard());
			assertNull(task.getProcedure());
			assertEquals("thename", task.getName()); //$NON-NLS-1$
			//
			AgentTask task2 = this.skill.task("thename"); //$NON-NLS-1$
			assertSame(task, task2);
			//
			AgentTask task3 = this.skill.task("thename2"); //$NON-NLS-1$
			assertNotSame(task, task3);
			//
			AgentTask task4 = this.skill.task(null);
			assertNotNull(task4);
			assertNotNull(task4.getGuard());
			assertSame(AgentTask.TRUE_GUARD, task4.getGuard());
			assertNull(task4.getProcedure());
			assertFalse(Strings.isEmpty(task4.getName()));
			//
			AgentTask task5 = this.skill.task("");
			assertNotNull(task5);
			assertNotNull(task5.getGuard());
			assertSame(AgentTask.TRUE_GUARD, task5.getGuard());
			assertNull(task5.getProcedure());
			assertFalse(Strings.isEmpty(task5.getName()));
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
		public void atLongProcedure1_inTheFuture() {
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask task = this.skill.at(5, procedure);
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
		public void atLongProcedure1_now() {
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask task = this.skill.at(0, procedure);
			assertNull(task);
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
			ArgumentCaptor<TimeUnit> argument3 = ArgumentCaptor.forClass(TimeUnit.class);
			Mockito.verify(this.executorService, never()).schedule(argument1.capture(), argument2.capture(),
					argument3.capture());
		}

		@Test
		public void atLongProcedure1_inThePast() {
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask task = this.skill.at(-5, procedure);
			assertNull(task);
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
			ArgumentCaptor<TimeUnit> argument3 = ArgumentCaptor.forClass(TimeUnit.class);
			Mockito.verify(this.executorService, never()).schedule(argument1.capture(), argument2.capture(),
					argument3.capture());
		}

		@Test
		public void atAgentTaskLongProcedure1_inTheFuture() {
			AgentTask task = Mockito.mock(AgentTask.class);
			Mockito.when(task.getName()).thenReturn("thetask"); //$NON-NLS-1$
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask t = this.skill.at(task, 5, procedure);
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
		public void atAgentTaskLongProcedure1_now() {
			AgentTask task = Mockito.mock(AgentTask.class);
			Mockito.when(task.getName()).thenReturn("thetask"); //$NON-NLS-1$
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask t = this.skill.at(task, 0, procedure);
			assertSame(task, t);
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
			ArgumentCaptor<TimeUnit> argument3 = ArgumentCaptor.forClass(TimeUnit.class);
			Mockito.verify(this.executorService, never()).schedule(argument1.capture(), argument2.capture(),
					argument3.capture());
		}

		@Test
		public void atAgentTaskLongProcedure1_inThePast() {
			AgentTask task = Mockito.mock(AgentTask.class);
			Mockito.when(task.getName()).thenReturn("thetask"); //$NON-NLS-1$
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask t = this.skill.at(task, -5, procedure);
			assertSame(task, t);
			assertSame(task, t);
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
			ArgumentCaptor<TimeUnit> argument3 = ArgumentCaptor.forClass(TimeUnit.class);
			Mockito.verify(this.executorService, never()).schedule(argument1.capture(), argument2.capture(),
					argument3.capture());
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
			this.reflect.invoke(this.skill, "uninstall", UninstallationStage.PRE_DESTROY_EVENT);
			//
			Collection<String> activeTasks = (Collection<String>) this.reflect.invoke(this.skill, "getActiveTasks");
			assertTrue(activeTasks.isEmpty());
			//
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

		@Test
		public void atFixedDelayLongProcedure1() {
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask task = this.skill.atFixedDelay(500, procedure);
			assertNotNull(task);
			assertSame(procedure, task.getProcedure());
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
			ArgumentCaptor<Long> argument3 = ArgumentCaptor.forClass(Long.class);
			ArgumentCaptor<TimeUnit> argument4 = ArgumentCaptor.forClass(TimeUnit.class);
			Mockito.verify(this.executorService, new Times(1)).scheduleWithFixedDelay(argument1.capture(), argument2.capture(),
					argument3.capture(), argument4.capture());
			assertNotNull(argument1.getValue());
			assertEquals(new Long(0), argument2.getValue());
			assertEquals(new Long(500), argument3.getValue());
			assertSame(TimeUnit.MILLISECONDS, argument4.getValue());
		}

		@Test
		public void atFixedDelayAgentTaskLongProcedure1() {
			AgentTask task = Mockito.mock(AgentTask.class);
			Mockito.when(task.getName()).thenReturn("thetask"); //$NON-NLS-1$
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask t = this.skill.atFixedDelay(task, 500, procedure);
			assertSame(task, t);
			ArgumentCaptor<Procedure1> argument0 = ArgumentCaptor.forClass(Procedure1.class);
			Mockito.verify(task, new Times(1)).setProcedure(argument0.capture());
			assertSame(procedure, argument0.getValue());
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			ArgumentCaptor<Long> argument2 = ArgumentCaptor.forClass(Long.class);
			ArgumentCaptor<Long> argument3 = ArgumentCaptor.forClass(Long.class);
			ArgumentCaptor<TimeUnit> argument4 = ArgumentCaptor.forClass(TimeUnit.class);
			Mockito.verify(this.executorService, new Times(1)).scheduleWithFixedDelay(argument1.capture(), argument2.capture(),
					argument3.capture(), argument4.capture());
			assertNotNull(argument1.getValue());
			assertEquals(new Long(0), argument2.getValue());
			assertEquals(new Long(500), argument3.getValue());
			assertSame(TimeUnit.MILLISECONDS, argument4.getValue());
		}

		@Test
		public void atFixedDelayLongProcedure1_zeroDelay() {
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask task = this.skill.atFixedDelay(0, procedure);
			assertNotNull(task);
			assertSame(procedure, task.getProcedure());
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, new Times(1)).submit(argument1.capture());
			Runnable runnable = argument1.getValue();
			assertNotNull(runnable);
			assertEquals("io.janusproject.kernel.bic.SchedulesSkill$AgentInfiniteLoopTask", runnable.getClass().getName());
		}

		@Test
		public void atFixedDelayAgentTaskLongProcedure1_zeroDelay() {
			AgentTask task = Mockito.mock(AgentTask.class);
			Mockito.when(task.getName()).thenReturn("thetask"); //$NON-NLS-1$
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask t = this.skill.atFixedDelay(task, 0, procedure);
			assertSame(task, t);
			ArgumentCaptor<Procedure1> argument0 = ArgumentCaptor.forClass(Procedure1.class);
			Mockito.verify(task, new Times(1)).setProcedure(argument0.capture());
			assertSame(procedure, argument0.getValue());
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, new Times(1)).submit(argument1.capture());
			Runnable runnable = argument1.getValue();
			assertNotNull(runnable);
			assertEquals("io.janusproject.kernel.bic.SchedulesSkill$AgentInfiniteLoopTask", runnable.getClass().getName());
		}

		@Test
		public void executeProcedure1() {
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask task = this.skill.execute(procedure);
			assertNotNull(task);
			assertSame(procedure, task.getProcedure());
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, new Times(1)).submit(argument1.capture());
			assertNotNull(argument1.getValue());
		}

		@Test
		public void executeAgentTaskLongProcedure1() {
			AgentTask task = Mockito.mock(AgentTask.class);
			Mockito.when(task.getName()).thenReturn("thetask"); //$NON-NLS-1$
			Procedure1 procedure = Mockito.mock(Procedure1.class);
			AgentTask t = this.skill.execute(task, procedure);
			assertSame(task, t);
			ArgumentCaptor<Procedure1> argument0 = ArgumentCaptor.forClass(Procedure1.class);
			Mockito.verify(task, new Times(1)).setProcedure(argument0.capture());
			assertSame(procedure, argument0.getValue());
			ArgumentCaptor<Runnable> argument1 = ArgumentCaptor.forClass(Runnable.class);
			Mockito.verify(this.executorService, new Times(1)).submit(argument1.capture());
			assertNotNull(argument1.getValue());
		}

		@Test
		public void uninstall_Pre() throws Exception {
			Procedure1 procedure1 = Mockito.mock(Procedure1.class);
			this.skill.every(5, procedure1);
			Procedure1 procedure2 = Mockito.mock(Procedure1.class);
			this.skill.in(5, procedure2);
			Collection<ScheduledFuture<?>> futures = (Collection<ScheduledFuture<?>>) this.reflect.invoke(this.skill, "getActiveFutures");
			assertEquals(2, futures.size());
			//
			this.reflect.invoke(this.skill, "uninstall", UninstallationStage.PRE_DESTROY_EVENT);
			//
			Collection<String> activeTasks = (Collection<String>) this.reflect.invoke(this.skill, "getActiveTasks");
			assertTrue(activeTasks.isEmpty());
			for (ScheduledFuture<?> f : futures) {
				Mockito.verify(f, new Times(1)).cancel(ArgumentMatchers.anyBoolean());
			}
		}

		@Test
		public void uninstall_Post() throws Exception {
			Procedure1 procedure1 = Mockito.mock(Procedure1.class);
			this.skill.every(5, procedure1);
			Procedure1 procedure2 = Mockito.mock(Procedure1.class);
			this.skill.in(5, procedure2);
			Collection<ScheduledFuture<?>> futures = (Collection<ScheduledFuture<?>>) this.reflect.invoke(this.skill, "getActiveFutures");
			assertEquals(2, futures.size());
			//
			this.reflect.invoke(this.skill, "uninstall", UninstallationStage.POST_DESTROY_EVENT);
			//
			Collection<String> activeTasks = (Collection<String>) this.reflect.invoke(this.skill, "getActiveTasks");
			assertTrue(activeTasks.isEmpty());
			for (ScheduledFuture<?> f : futures) {
				Mockito.verify(f, new Times(1)).cancel(ArgumentMatchers.anyBoolean());
			}
		}

	}

	public static class RuntimeTests extends AbstractJanusRunTest {

		@Test
		public void in() throws Exception {
			runJanus(SchedulesRunTestAgent0.class, false);
			assertEquals(1, getNumberOfResults());
			assertEquals(Boolean.TRUE, getResult(Boolean.class, 0));
		}

		@Test
		public void every() throws Exception {
			runJanus(SchedulesRunTestAgent1.class, false);
			assertEquals(2, getNumberOfResults());
			assertEquals(0, getResult(Integer.class, 0).intValue());
			assertEquals(1, getResult(Integer.class, 1).intValue());
		}

		@Test
		public void atFixedDelay() throws Exception {
			runJanus(SchedulesRunTestAgent2.class, false);
			assertEquals(2, getNumberOfResults());
			assertEquals(0, getResult(Integer.class, 0).intValue());
			assertEquals(1, getResult(Integer.class, 1).intValue());
		}

		@Test
		public void atFixedDelay_zeroDelay() throws Exception {
			runJanus(SchedulesRunTestAgent3.class, false);
			assertEquals(2, getNumberOfResults());
			assertEquals(0, getResult(Integer.class, 0).intValue());
			assertEquals(1, getResult(Integer.class, 1).intValue());
		}

		@Test
		public void execute() throws Exception {
			runJanus(SchedulesRunTestAgent4.class, false);
			assertEquals(1, getNumberOfResults());
			assertEquals(Boolean.TRUE, getResult(Boolean.class, 0));
		}

		@Test
		public void cancel_infiniteLoop() throws Exception {
			runJanus(SchedulesRunTestAgent5.class, false);
			assertEquals(2, getNumberOfResults());
			assertEquals(0, getResult(Integer.class, 0).intValue());
			assertEquals(1, getResult(Integer.class, 1).intValue());
		}

		@Test
		public void cancel_everyTask() throws Exception {
			runJanus(SchedulesRunTestAgent6.class, false);
			assertEquals(2, getNumberOfResults());
			assertEquals(0, getResult(Integer.class, 0).intValue());
			assertEquals(1, getResult(Integer.class, 1).intValue());
		}

		@Test
		@Ignore
		public void autodestroyTaskLaunchedFromBehavior() throws Exception {
			runJanus(InitiatorTestingAgent.class);
			Collection<String> collection = getResult(Collection.class, 0);
			assertNotNull(collection);
			assertEquals("Task list: " + collection.toArray(), 1, collection.size());
			String task = collection.iterator().next();
			assertEquals("XXX", task);
		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class InfiniteTask implements Procedure1<Agent> {

			@Override
			public void apply(Agent it) {
				while (true) {
					try {
						Thread.sleep(500);
					} catch (InterruptedException e) {
						//
					}
				}
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class SchedulesRunTestAgent0 extends TestingAgent {

			public SchedulesRunTestAgent0(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				getSkill(Schedules.class).in(500, (it) -> {
					addResult(Boolean.TRUE);
					forceKillMe();
				});
				return false;
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class SchedulesRunTestAgent1 extends TestingAgent {

			private final AtomicInteger index = new AtomicInteger();

			public SchedulesRunTestAgent1(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				getSkill(Schedules.class).every(500, (it) -> {
					addResult(this.index.getAndIncrement());
					if (this.index.get() >= 2) {
						forceKillMe();
					}
				});
				return false;
			}

		}

		/**
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class SchedulesRunTestAgent2 extends TestingAgent {

			private final AtomicInteger index = new AtomicInteger();

			public SchedulesRunTestAgent2(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				getSkill(Schedules.class).atFixedDelay(500, (it) -> {
					addResult(this.index.getAndIncrement());
					if (this.index.get() >= 2) {
						forceKillMe();
					}
				});
				return false;
			}
		}

				/**
				 * @author $Author: sgalland$
				 * @version $FullVersion$
				 * @mavengroupid $GroupId$
				 * @mavenartifactid $ArtifactId$
				 */
				@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
				public static class SchedulesRunTestAgent3 extends TestingAgent {

					private final AtomicInteger index = new AtomicInteger();

					public SchedulesRunTestAgent3(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
						super(provider, parentID, agentID);
					}

					@Override
					protected boolean runAgentTest() {
						getSkill(Schedules.class).atFixedDelay(0, (it) -> {
							addResult(this.index.getAndIncrement());
							if (this.index.get() >= 2) {
								forceKillMe();
							}
						});
						return false;
					}

				}

				/**
				 * @author $Author: sgalland$
				 * @version $FullVersion$
				 * @mavengroupid $GroupId$
				 * @mavenartifactid $ArtifactId$
				 */
				@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
				public static class SchedulesRunTestAgent4 extends TestingAgent {

					public SchedulesRunTestAgent4(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
						super(provider, parentID, agentID);
					}

					@Override
					protected boolean runAgentTest() {
						getSkill(Schedules.class).execute((it) -> {
							addResult(Boolean.TRUE);
							forceKillMe();
						});
						return false;
					}

				}

				/**
				 * @author $Author: sgalland$
				 * @version $FullVersion$
				 * @mavengroupid $GroupId$
				 * @mavenartifactid $ArtifactId$
				 */
				@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
				public static class SchedulesRunTestAgent5 extends TestingAgent {

					private final AtomicInteger index = new AtomicInteger();

					public SchedulesRunTestAgent5(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
						super(provider, parentID, agentID);
					}

					@Override
					protected boolean runAgentTest() {
						final AgentTask task = getSkill(Schedules.class).task(null);
						getSkill(Schedules.class).atFixedDelay(task, 0, (it) -> {
							if (this.index.get() >= 2) {
								throw new IllegalStateException();
							}
							addResult(this.index.getAndIncrement());
							if (this.index.get() >= 2) {
								getSkill(Schedules.class).cancel(task);
							}
							SchedulesSkill skill = (SchedulesSkill) getSkill(Schedules.class);
							if (skill.getActiveTasks().isEmpty()) {
								forceKillMe();
							}
						});
						return false;
					}

				}

				/**
				 * @author $Author: sgalland$
				 * @version $FullVersion$
				 * @mavengroupid $GroupId$
				 * @mavenartifactid $ArtifactId$
				 */
				@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
				public static class SchedulesRunTestAgent6 extends TestingAgent {

					private final AtomicInteger index = new AtomicInteger();

					public SchedulesRunTestAgent6(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
						super(provider, parentID, agentID);
					}

					@Override
					protected boolean runAgentTest() {
						final AgentTask task = getSkill(Schedules.class).task(null);
						getSkill(Schedules.class).every(task, 500, (it) -> {
							if (this.index.get() >= 2) {
								throw new IllegalStateException();
							}
							addResult(this.index.getAndIncrement());
							if (this.index.get() >= 2) {
								getSkill(Schedules.class).cancel(task);
							}
							SchedulesSkill skill = (SchedulesSkill) getSkill(Schedules.class);
							if (skill.getActiveTasks().isEmpty()) {
								forceKillMe();
							}
						});
						return false;
					}

				}

				@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
				public static class InitiatorTestingAgent extends TestingAgent {

					protected final ReflectExtensions ext = new ReflectExtensions();

					public InitiatorTestingAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
						super(provider, parentID, agentID);
					}

					@PerceptGuardEvaluator
					private void onDestroyGuard(Destroy event, Collection<Runnable> handlers) {
						handlers.add(() -> onDestroy(event, event));
					}

					private void onDestroy(Destroy it, Destroy occurrence) {
						try {
							addResult(this.ext.invoke(getSkill(Schedules.class), "getActiveTasks"));
						} catch (Exception e) {
							addResult(e);
						}
					}

					@Override
					protected boolean runAgentTest() {
						BehaviorTest beh = new BehaviorTest(this);
						getSkill(Behaviors.class).registerBehavior(beh);
						Schedules schedules = getSkill(Schedules.class);
						AgentTask xxx = schedules.task("XXX");
						schedules.in(xxx, 1000, (it) -> {
							getSkill(Behaviors.class).unregisterBehavior(beh);
							try {
								addResult(this.ext.invoke(schedules, "getActiveTasks"));
							} catch (Exception e) {
								addResult(e);
							}
							getSkill(Lifecycle.class).killMe();
						});
						return false;
					}

				}

				@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
				public static class BehaviorTest extends Behavior {

					public BehaviorTest(Agent agent) {
						super(agent);
					}

					@PerceptGuardEvaluator
					private void onInitializeGuard(Initialize event, Collection<Runnable> handlers) {
						handlers.add(() -> onInitialize(event, event));
					}

					private void onInitialize(Initialize it, Initialize occurrence) {
						getSkill(Schedules.class).in(0, new InfiniteTask());
					}

				}

			}

		}
