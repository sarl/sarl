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
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Rule;
import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.AgentKilled;
import io.sarl.core.AgentTask;
import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.InnerContextAccess;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Event;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.Repeat;
import io.sarl.tests.api.RepeatRule;

/** Tests for issue #900: Run-time inconstancy between Schedules and DefaultContextInteractions.
 *
 * <p>See: https://github.com/sarl/sarl/issues/900
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/900"
 */
@SuppressWarnings("all")
public class Bug900 extends AbstractJanusRunTest {

	@Test
	public void run_01() throws Exception {
		startJanusWithDefaultProcess(PongAgent.class, false, true, getDefaultJanusModule());
		Thread.sleep(1000);
		this.janusKernel.spawn(PingAgent.class, getAgentInitializationParameters());
		waitForTheKernel(NO_TIMEOUT);
		assertEquals(1, getNumberOfResults(getBootAgent()));
		assertEquals("RECEIVE 159", getResult(getBootAgent(), String.class, 0));
		forgetTheKernel();
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_EVENT)
	static class Ping extends Event {
		public final int index;
		public Ping(int index) {
			this.index = index;
		}
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class PingAgent extends TestingAgent {

		private final AtomicInteger index = new AtomicInteger(159); 
		
		public PingAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			AgentTask task = getSkill(Schedules.class).task("hello");
			getSkill(Schedules.class).every(task, 1000, (it) -> {
				if (getSkill(DefaultContextInteractions.class).getDefaultSpace().getParticipants().size() > 1) {
					getSkill(DefaultContextInteractions.class).emit(new Ping(index.getAndIncrement()));
					getSkill(Schedules.class).cancel(task);
				}
			});
			getSkill(Schedules.class).in(3000, (it) -> {
				forceKillMe();
			});
			return false;
		}

	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class PongAgent extends TestingAgent {

		public PongAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			return false;
		}
		
		@PerceptGuardEvaluator
		private void guardPing(Ping occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onPing(occurrence));
		}

		private void onPing(Ping occurrence) {
			addResult("RECEIVE " + occurrence.index);
			forceKillMe();
		}
		
	}

}
