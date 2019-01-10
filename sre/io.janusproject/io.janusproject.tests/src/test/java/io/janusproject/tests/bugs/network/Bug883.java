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
package io.janusproject.tests.bugs.network;

import java.util.Collection;
import java.util.UUID;

import org.junit.Ignore;
import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Logging;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Event;
import io.sarl.lang.sarl.SarlPackage;

/** Tests for issue #883: Agent cannot receive event over network.
 *
 * <p>See: https://github.com/sarl/sarl/issues/883
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/883"
 */
@SuppressWarnings("all")
public class Bug883 extends AbstractJanusRunTest {

	@Test
	@Ignore
	public void runReceiver() throws Exception {
		runJanus(ReceiverAgent.class, true, false, NO_TIMEOUT);
	}

	@Test
	@Ignore
	public void runSender() throws Exception {
		runJanus(SenderAgent.class, true, false, NO_TIMEOUT);
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_EVENT)
	static class TestEvent extends Event {
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class SenderAgent extends TestingAgent {

		public SenderAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			getSkill(Schedules.class).in(500, (agent) -> {
				getSkill(DefaultContextInteractions.class).emit(new TestEvent());
			});
			return false;
		}

		@PerceptGuardEvaluator
		private void guardTestEvent(TestEvent occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onTestEvent(occurrence));
		}

		private void onTestEvent(TestEvent occurrence) {
			getSkill(Logging.class).info("Sender received: " + occurrence);
			addResult("received");
			forceKillMe();
		}

	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class ReceiverAgent extends TestingAgent {

		public ReceiverAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			return false;
		}

		@PerceptGuardEvaluator
		private void guardTestEvent(TestEvent occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onTestEvent(occurrence));
		}

		private void onTestEvent(TestEvent occurrence) {
			getSkill(Logging.class).info("Receiver received: " + occurrence);
			addResult("received");
			forceKillMe();
		}

	}

}
