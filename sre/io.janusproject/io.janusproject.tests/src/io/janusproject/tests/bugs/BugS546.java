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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeoutException;

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
	BugS546.NoRandomWaiting.class,
	BugS546.RandomWaiting.class,
})
@SuppressWarnings("all")
public class BugS546 {

	private static final boolean LOG = false;
	
	private static final int NB_AGENTS = 100;

	private static final int TIMEOUT = 240;

	private static final int BASE_DELAY = 500;

	private static final int MAX_EXTRA_DELAY = 2000;

	public static abstract class AbstractBugS546Test extends AbstractJanusRunTest {

		@Test
		public void waitAgentKilled() throws Exception {
			Kernel kernel = setupTheJanusKernel(KillWaiterAgent.class, false, true);
			List<UUID> ids = new ArrayList<>(NB_AGENTS);
			for (int i = 0; i < NB_AGENTS; ++i) {
				UUID id = spawn(kernel);
				ids.add(id);
			}
			try {
				waitForTheKernel(TIMEOUT);
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
					System.out.println("KILLABLE AGENT: " + getID());
				}
				final Schedules s = getSkill(Schedules.class);
				this.task = s.task(null);
				s.every(this.task, 1000, (it) -> {
					if (LOG) {
						System.out.println("SEND PRESENTATION: " + getID());
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
				if (LOG) {
					System.out.println("SHOOT -> cancel task: " + getID());
				}
				Schedules s = getSkill(Schedules.class);
				s.cancel(this.task);
				if (LOG) {
					System.out.println("SHOOT -> react: " + getID());
				}
				shootBehaviorUnit(occurrence);
			}

			protected abstract void shootBehaviorUnit(ShootEvent occurrence);

		}

		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class KillWaiterAgent extends TestingAgent {

			private final Set<UUID> agents = Collections.synchronizedSet(new HashSet<>());
			private final Set<UUID> killedAgents = Collections.synchronizedSet(new HashSet<>());

			public KillWaiterAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			@Override
			protected boolean runAgentTest() {
				final Schedules s = getSkill(Schedules.class);
				final AgentTask task = s.task(null);
				s.every(task, 500, (it) -> {
					int nb = this.agents.size();
					if (LOG) {
						System.out.println("CHECK FOR KILLABLE AGENTS: " + nb);
					}
					if (nb >= NB_AGENTS) {
						s.cancel(task);
						getSkill(DefaultContextInteractions.class).emit(new ShootEvent());
					}
				});
				return false;
			}

			@PerceptGuardEvaluator
			private void initializeGuardEvaluator(PresentationEvent occurrence, Collection<Runnable> ___SARLlocal_runnableCollection) {
				assert occurrence != null;
				assert ___SARLlocal_runnableCollection != null;
				___SARLlocal_runnableCollection.add(() -> initializeBehaviorUnit(occurrence));
			}

			private void initializeBehaviorUnit(PresentationEvent occurrence) {
				if (LOG) {
					System.out.println("AGENT READY: " + occurrence.getSource().getUUID());
				}
				this.agents.add(occurrence.getSource().getUUID());
			}

			@PerceptGuardEvaluator
			private void agentKilledGuardEvaluator(AgentKilled occurrence, Collection<Runnable> ___SARLlocal_runnableCollection) {
				assert occurrence != null;
				assert ___SARLlocal_runnableCollection != null;
				if (!isFromMe(occurrence)) {
					___SARLlocal_runnableCollection.add(() -> agentKilledBehaviorUnit(occurrence));
				}
			}

			private void agentKilledBehaviorUnit(AgentKilled occurrence) {
				if (this.agents.contains(occurrence.getSource().getUUID())) {
					if (LOG) {
						System.out.println("AGENT KILLED: " + occurrence.getSource().getUUID());
					}
					addResult(occurrence.getSource().getUUID());
					if (getResults().size() >= NB_AGENTS) {
						if (LOG) {
							System.out.println("STOP SYSTEM");
						}
						getSkill(Lifecycle.class).killMe();
					}
				} else if (LOG) {
					System.out.println("IGNORE AGENT KILLED: " + occurrence.getSource().getUUID());
				}

			}
	
		}

		public static class PresentationEvent extends Event {
		}

		public static class ShootEvent extends Event {
		}

	}

	public static class NoRandomWaiting extends AbstractBugS546Test {

		@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
		public static class NotRandomKillableAgent extends AbstractKillableAgent {

			public NotRandomKillableAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
				super(provider, parentID, agentID);
			}

			protected void shootBehaviorUnit(ShootEvent occurrence) {
				if (LOG) {
					System.out.println("I'M SHOOT -> commit suicide: " + getID());
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

}
