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

package io.janusproject.tests.bugs;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.janusproject.kernel.Kernel;
import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.AgentKilled;
import io.sarl.core.AgentTask;
import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Event;
import io.sarl.util.Scopes;

/**
 * Unit test for the issue #546: Not enough AgentKilled occurrences after killMe() calls.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/546
 */
@RunWith(Suite.class)
@SuiteClasses({
	Bug546.NoRandomWaiting.class,
	Bug546.RandomWaiting.class,
	Bug546.HugeEventSetTest.class,
})
@SuppressWarnings("all")
public class Bug546 {

	private static final boolean LOG = false;
	
	private static final int NB_AGENTS = 300;

	private static final int NB_EVENTS = 500;

	private static final int BASE_DELAY = 500;

	private static final int MAX_EXTRA_DELAY = 2000;

	private static final int PRESENTATION_DELAY = 1000;

	public static abstract class AbstractBugS546Test extends AbstractJanusRunTest {

		@Test
		public void waitAgentKilled() throws Exception {
			startJanusWithDefaultProcess(KillWaiterAgent.class, false, true, getDefaultJanusModule());
			List<UUID> ids = new ArrayList<>(NB_AGENTS);
			for (int i = 0; i < NB_AGENTS; ++i) {
				UUID id = spawn(this.janusKernel);
				ids.add(id);
			}
			try {
				waitForTheKernel(EXTRA_TIMEOUT);
			} catch (TimeoutException e) {
				if (LOG) {
					System.err.println(getResults());
				}
				throw e;
			}
			assertEquals(NB_AGENTS, getNumberOfResults());
			assertContains(getResults(), ids.toArray());
		}

		protected abstract UUID spawn(Kernel kernel);

		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static abstract class AbstractKillableAgent extends TestingAgent {

			private AgentTask task;

			public AbstractKillableAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				if (LOG) {
					System.out.println("K-" + getID() + ": starting");
				}
				final Schedules s = getSkill(Schedules.class);
				this.task = s.task(null);
				s.every(this.task, PRESENTATION_DELAY, (it) -> {
					if (LOG) {
						System.out.println("K-" + getID() + ": send presentation");
					}
					getSkill(DefaultContextInteractions.class).emit(new PresentationEvent());
				});
				return false;
			}

			@PerceptGuardEvaluator
			private void shootGuardEvaluator(ShootEvent occurrence, Collection<Runnable> ___SARLlocal_runnableCollection) {
				assert occurrence != null;
				assert ___SARLlocal_runnableCollection != null;
				___SARLlocal_runnableCollection.add(() -> shootBehaviorUnitPrivate(occurrence));
			}

			private void shootBehaviorUnitPrivate(ShootEvent occurrence) {
				Schedules s = getSkill(Schedules.class);
				s.cancel(this.task);
				if (LOG) {
					System.out.println("K-" + getID() + ": cancel presentation task");
					System.out.println("K-" + getID() + ": run shoot behavior");
				}
				shootBehaviorUnit(occurrence);
			}

			protected abstract void shootBehaviorUnit(ShootEvent occurrence);

		}

		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class KillWaiterAgent extends TestingAgent {

			private final Set<UUID> presentedAgents = new TreeSet<>();
			private final Set<UUID> shootedAgents = new TreeSet<>();
			private final AtomicInteger feedbacks = new AtomicInteger();
			private final AtomicInteger guards = new AtomicInteger();

			public KillWaiterAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				final Schedules s = getSkill(Schedules.class);
				final AgentTask task = s.task(null);
				s.every(task, 500, (it) -> {
					final int nb;
					synchronized (this.presentedAgents) {
						nb = this.presentedAgents.size();
					}
					if (LOG) {
						System.out.println("W: receiving " + nb + " agent presentation(s)");
					}
					if (nb >= NB_AGENTS && !s.isCanceled(task)) {
						s.cancel(task);
						if (LOG) {
							System.out.println("W: waiting task canceled");
							System.out.println("W: shoot the guys");
						}
						getSkill(DefaultContextInteractions.class).emit(new ShootEvent());
					}
				});
				if (LOG) {
					s.every(1000, (it) -> {
						final int nb1;
						synchronized (this.presentedAgents) {
							nb1 = this.presentedAgents.size();
						}
						final int nb2;
						synchronized (this.shootedAgents) {
							nb2 = this.shootedAgents.size();
						}
						final int nb3 = getSkill(DefaultContextInteractions.class).getDefaultSpace().getParticipants().size();
						System.out.println("INFO: present=" + nb1 + "; killed=" + nb2 + "; inSpace=" + nb3);
						System.out.flush();
					});
				}
				return false;
			}

			@PerceptGuardEvaluator
			private void initializeGuardEvaluator(PresentationEvent occurrence, Collection<Runnable> ___SARLlocal_runnableCollection) {
				assert occurrence != null;
				assert ___SARLlocal_runnableCollection != null;
				___SARLlocal_runnableCollection.add(() -> initializeBehaviorUnit(occurrence));
			}

			private void initializeBehaviorUnit(PresentationEvent occurrence) {
				final boolean b;
				synchronized (this.presentedAgents) {
					b = this.presentedAgents.add(occurrence.getSource().getUUID());
				}
				if (!b) {
					if (LOG) {
						System.out.println("W: again K-" + occurrence.getSource().getUUID());
					}
				} else if (LOG) {
					System.out.println("W: hello K-" + occurrence.getSource().getUUID());
				}

			}

			@PerceptGuardEvaluator
			private void agentKilledGuardEvaluator(AgentKilled occurrence, Collection<Runnable> ___SARLlocal_runnableCollection) {
				assert occurrence != null;
				assert ___SARLlocal_runnableCollection != null;
				if (LOG) {
					System.out.println("W: guard evaluation #" + guards.incrementAndGet()) ;
				}
				if (!isFromMe(occurrence)) {
					___SARLlocal_runnableCollection.add(() -> agentKilledBehaviorUnit(occurrence));
				}
			}

			private void agentKilledBehaviorUnit(AgentKilled occurrence) {
				if (LOG) {
					System.out.println("W: feedback #" + feedbacks.incrementAndGet()) ;
				}
				final boolean b;
				final int size1;
				synchronized (this.presentedAgents) {
					b = this.presentedAgents.remove(occurrence.getSource().getUUID());
					size1 = this.presentedAgents.size();
				}
				if (b) {
					if (LOG) {
						System.out.println("W: agent K-" + occurrence.getSource().getUUID() + " killed") ;
						System.out.println("W: remaining " + size1 + " agent(s)") ;
					}
					final int size2;
					synchronized (this.shootedAgents) {
						this.shootedAgents.add(occurrence.getSource().getUUID());
						size2 = this.shootedAgents.size();
					}
					if (LOG) {
						System.out.println("W: living agents = " + size1) ;
						System.out.println("W: killed agents = " + size2) ;
					}
					if (size2 >= NB_AGENTS) {
						if (LOG) {
							System.out.println("W: Stop unit test");
						}
						synchronized (this.shootedAgents) {
							addResults(this.shootedAgents);
						}
						getSkill(Lifecycle.class).killMe();
					}
				} else if (LOG) {
					System.out.println("W: IGNORE AGENT KILLED FOR " + occurrence.getSource().getUUID());
				}
			}
	
		}

	}

	public static class PresentationEvent extends Event {
	}

	public static class ShootEvent extends Event {
	}

	public static class NoRandomWaiting extends AbstractBugS546Test {

		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class NotRandomKillableAgent extends AbstractKillableAgent {

			public NotRandomKillableAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			protected void shootBehaviorUnit(ShootEvent occurrence) {
				if (LOG) {
					System.out.println("K-" + getID() + ": bye bye");

				}
				getSkill(Lifecycle.class).killMe();
			}

		}

		@Override
		protected UUID spawn(Kernel kernel) {
			return kernel.spawn(NotRandomKillableAgent.class, getAgentInitializationParameters());
		}

	}

	public static class RandomWaiting extends AbstractBugS546Test {

		private static final Random RANDOM = new Random();

		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class RandomKillableAgent extends AbstractKillableAgent {

			public RandomKillableAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			protected void shootBehaviorUnit(ShootEvent occurrence) {
				long delay = RANDOM.nextInt(MAX_EXTRA_DELAY) + BASE_DELAY;
				if (LOG) {
					System.out.println("I'M SHOOT -> commit suicide in " + delay + " ms: " + getID());
				}
				getSkill(Schedules.class).in(delay, (it) -> {
					if (LOG) {
						System.out.println("Commit suicide now: " + getID());
					}
					getSkill(Lifecycle.class).killMe();
				});
			}

		}

		@Override
		protected UUID spawn(Kernel kernel) {
			return kernel.spawn(RandomKillableAgent.class, getAgentInitializationParameters());
		}

	}

	@Ignore
	public static class HugeEventSetTest extends AbstractJanusRunTest {

		@Test
		public void sendEvents() throws Exception {
			startJanusWithDefaultProcess(ReceivingAgent.class, false, true, getDefaultJanusModule());
			this.janusKernel.spawn(SendingAgent.class);
			try {
				waitForTheKernel(EXTRA_TIMEOUT);
			} catch (TimeoutException e) {
				if (LOG) {
					System.err.println(getResults());
				}
				throw e;
			}
			assertEquals(NB_EVENTS, getNumberOfResults());
			for (Object result : getResults()) {
				assertTrue(result instanceof HugeEventSetEvent);
			}
		}

		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class SendingAgent extends TestingAgent {

			private final AtomicBoolean treated = new AtomicBoolean(false);

			public SendingAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				return false;
			}

			@PerceptGuardEvaluator
			private void guardEvaluator1(PresentationEvent occurrence, Collection<Runnable> ___SARLlocal_runnableCollection) {
				assert occurrence != null;
				assert ___SARLlocal_runnableCollection != null;
				if (treated.getAndSet(true)) {
					___SARLlocal_runnableCollection.add(() -> eventHandler1(occurrence));
				}
			}

			private void eventHandler1(PresentationEvent occurrence) {
				assert occurrence != null;
				getSkill(Schedules.class).in(1000, (agent) -> {
					DefaultContextInteractions skill = getSkill(DefaultContextInteractions.class);
					for (int i = 0; i < NB_EVENTS; ++i) {
						skill.emit(new HugeEventSetEvent(), Scopes.notAddresses(skill.getDefaultAddress()));
					}
				});
			}

		}
	
		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class ReceivingAgent extends TestingAgent {

			private AgentTask task;

			public ReceivingAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				final Schedules schedules = getSkill(Schedules.class);
				this.task = schedules.task(null);
				schedules.every(this.task, PRESENTATION_DELAY, (agent) -> {
					DefaultContextInteractions ctx = getSkill(DefaultContextInteractions.class);
					ctx.emit(new PresentationEvent(), Scopes.notAddresses(ctx.getDefaultAddress()));
				});
				return false;
			}

			@PerceptGuardEvaluator
			private void guardEvaluator(HugeEventSetEvent occurrence, Collection<Runnable> ___SARLlocal_runnableCollection) {
				assert occurrence != null;
				assert ___SARLlocal_runnableCollection != null;
				AgentTask tsk = this.task;
				if (tsk != null) {
					this.task = null;
					getSkill(Schedules.class).cancel(tsk);
				}
				___SARLlocal_runnableCollection.add(() -> eventHandler(occurrence));
			}

			private void eventHandler(HugeEventSetEvent occurrence) {
				assert occurrence != null;
				addResult(occurrence);
			}

		}

		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static final class HugeEventSetEvent extends Event {

		}

	}

}
