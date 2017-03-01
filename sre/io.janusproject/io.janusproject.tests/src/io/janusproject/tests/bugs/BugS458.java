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

import java.util.Collection;
import java.util.UUID;

import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.Behaviors;
import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Destroy;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Event;

/**
 * Unit test for the issue #458: Thread deadlock problem in agent's destroy function
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/458
 */
@SuppressWarnings("all")
public class BugS458 extends AbstractJanusRunTest {

	@Test
	public void ExceptionInInit() throws Exception {
		runJanus(KillMeInBehaviorAgent.class, false, true, STANDARD_TIMEOUT);
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class FakeEvent extends Event {

		public FakeEvent() {
			super();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class KillMeBehavior extends Behavior {

		public KillMeBehavior(Agent agent) {
			super(agent);
		}

		@PerceptGuardEvaluator
		private void $guardEvaluator$FakeEvent(final FakeEvent occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {
			assert occurrence != null;
			assert ___SARLlocal_runnableCollection != null;
			___SARLlocal_runnableCollection.add(() -> $behaviorUnit$FakeEvent$0(occurrence));
		}

		private void $behaviorUnit$FakeEvent$0(final FakeEvent occurrence) {
			getSkill(Lifecycle.class).killMe();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class KillMeInBehaviorAgent extends TestingAgent {

		private KillMeBehavior behavior;
		
		public KillMeInBehaviorAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
			super(provider, parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			this.behavior = new KillMeBehavior(this);
			getSkill(Behaviors.class).registerBehavior(this.behavior);
			getSkill(Schedules.class).in(500, (agent) -> {
				getSkill(DefaultContextInteractions.class).emit(new FakeEvent());
			});
			return false;
		}

		@PerceptGuardEvaluator
		private void $guardEvaluator$Destroy(final Destroy occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {
			assert occurrence != null;
			assert ___SARLlocal_runnableCollection != null;
			___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Destroy$0(occurrence));
		}

		private void $behaviorUnit$Destroy$0(final Destroy occurrence) {
			getSkill(Behaviors.class).unregisterBehavior(this.behavior);
		}

	}

}
