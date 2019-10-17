/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
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
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.sarl.SarlPackage;

/** Tests for issue #942: Check if the source of an event received in Behavior is correct.
 *
 * <p>See: https://github.com/sarl/sarl/issues/942
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/942"
 */
@SuppressWarnings("all")
public class Bug942 extends AbstractJanusRunTest {

	@Test
	public void doTest() throws Exception {
		startJanusWithDefaultProcess(AgentBug942.class, false, true, getDefaultJanusModule());

		Thread.sleep(1000);

		final UUID sender = UUID.randomUUID();
		final EventSpace space = getTestingKernel().getJanusContext().getDefaultSpace();
		
		space.emit(sender, new EventBug942());

		waitForTheKernel(STANDARD_TIMEOUT);

		assertEquals(2, getNumberOfResults());
		final Data d1 = getResult(Data.class, 0);
		final Data d2 = getResult(Data.class, 1);

		assertNotNull(d1);
		assertNotNull(d2);

		if ("ag".equals(d1.name)) {
			assertEquals("beh", d2.name);
		} else {
			assertEquals("beh", d1.name);
			assertEquals("ag", d2.name);
		}

		assertEquals(sender, d1.address.getUUID());
		assertEquals(sender, d2.address.getUUID());

		assertEquals(space.getSpaceID(), d1.address.getSpaceID());
		assertEquals(space.getSpaceID(), d2.address.getSpaceID());

		forgetTheKernel();
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_EVENT)
	static class EventBug942 extends Event {
	}

	static class Data {
		public final String name;
		public final Address address;
		public Data(String name, Address address) {
			this.name = name;
			this.address = address;
		}
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class AgentBug942 extends TestingAgent {

		public AgentBug942(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			final BehaviorBug942 beh = new BehaviorBug942(this);
			getSkill(Behaviors.class).registerBehavior(beh);
			return false;
		}

		@PerceptGuardEvaluator
		private void triggerEventBug942(EventBug942 occurrence,  Collection<Runnable> handlers) {
			handlers.add(() -> onEventBug942(occurrence));
		}

		private void onEventBug942(EventBug942 occurrence) {
			addResult(new Data("ag", occurrence.getSource()));
			getSkill(Schedules.class).in(1000, it -> getSkill(Lifecycle.class).killMe());
		}

		void addResultFrombehavior(Object data) {
			addResult(data);
		}

	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_BEHAVIOR)
	static class BehaviorBug942 extends Behavior {

		public BehaviorBug942(Agent owner) {
			super(owner);
		}

		@PerceptGuardEvaluator
		private void triggerEventBug942(EventBug942 occurrence,  Collection<Runnable> handlers) {
			handlers.add(() -> onEventBug942(occurrence));
		}

		private void onEventBug942(EventBug942 occurrence) {
			AgentBug942 ag = (AgentBug942) getOwner();
			ag.addResultFrombehavior(new Data("beh", occurrence.getSource()));
		}

	}

}
