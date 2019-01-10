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

import static org.junit.Assert.assertFalse;

import java.util.Collection;
import java.util.UUID;

import org.junit.Rule;
import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.AgentKilled;
import io.sarl.core.InnerContextAccess;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.Repeat;
import io.sarl.tests.api.RepeatRule;

/** Tests for issue #872: Invalid synchronization of the member agent collection.
 *
 * <p>See: https://github.com/sarl/sarl/issues/872
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/872"
 */
@SuppressWarnings("all")
public class Bug872 extends AbstractJanusRunTest {

	@Rule
	public RepeatRule repeatRule = new RepeatRule();
	
	@Test
	@Repeat(5)
	public void run_01() throws Exception {
		runJanus(TopAgent.class, false, true, NO_TIMEOUT);
		assertFalse(getResult(getBootAgent(), Boolean.class, 0));
		forgetTheKernel();
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class TopAgent extends TestingAgent {

		public TopAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			getSkill(Lifecycle.class).spawnInContext(
					InnerAgent.class,
					getSkill(InnerContextAccess.class).getInnerContext(),
					getAgentInitializationParameters());
			return false;
		}

		@PerceptGuardEvaluator
		private void guardAgentKilled(AgentKilled occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onAgentKilled(occurrence));
		}

		private void onAgentKilled(AgentKilled occurrence) {
			boolean has = getSkill(InnerContextAccess.class).hasMemberAgent();
			addResult(has);
			forceKillMe();
		}
		
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class InnerAgent extends TestingAgent {

		public InnerAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			getSkill(Schedules.class).in(1000, it -> forceKillMe());
			return false;
		}
		
	}

}
