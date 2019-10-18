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

import java.util.UUID;

import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Lifecycle;
import io.sarl.core.OpenEventSpace;
import io.sarl.core.OpenEventSpaceSpecification;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Space;
import io.sarl.lang.sarl.SarlPackage;

/** Tests for issue #934: Agents communication enabled through space without being registered to it.
 *
 * <p>See: https://github.com/sarl/sarl/issues/934
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/934"
 */
@SuppressWarnings("all")
public class Bug934 extends AbstractJanusRunTest {

	@Test
	public void withSpec() throws Exception {
		runJanus(WithSpecAgent.class, false, true, SHORT_TIMEOUT);

		UUID id = getBootAgent();
		assertEquals(5, getNumberOfResults(id));
		
		EventSpace defaultSpace = getResult(id, EventSpace.class, 0);
		assertNotNull(defaultSpace);
		
		UUID spaceId1 = getResult(id, UUID.class, 1);
		assertNotEquals(defaultSpace.getSpaceID().getID(), spaceId1);
		
		Space otherSpace1 = getResult(id, Space.class, 2);
		assertNotNull(otherSpace1);
		assertEquals(spaceId1, otherSpace1.getSpaceID().getID());
		assertNotSame(defaultSpace, otherSpace1);
		assertNotEquals(defaultSpace.getSpaceID(), otherSpace1.getSpaceID());
		assertNotEquals(defaultSpace.getSpaceID().getID(), otherSpace1.getSpaceID().getID());
		
		UUID spaceId2 = getResult(id, UUID.class, 3);
		assertNotEquals(defaultSpace.getSpaceID().getID(), spaceId2);
		assertNotEquals(spaceId1, spaceId2);
		
		Space otherSpace2 = getResult(id, Space.class, 4);
		assertNotNull(otherSpace2);
		assertEquals(spaceId1, otherSpace2.getSpaceID().getID());
		assertNotSame(defaultSpace, otherSpace2);
		assertNotEquals(defaultSpace.getSpaceID(), otherSpace2.getSpaceID());
		assertNotEquals(defaultSpace.getSpaceID().getID(), otherSpace2.getSpaceID().getID());
		assertSame(otherSpace1, otherSpace2);

		forgetTheKernel();
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class WithSpecAgent extends TestingAgent {

		public WithSpecAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			AgentContext ctx = getSkill(DefaultContextInteractions.class).getDefaultContext();
			EventSpace defaultSpace = ctx.getDefaultSpace();
			addResult(defaultSpace);
			UUID id1 = UUID.randomUUID();
			addResult(id1);
			OpenEventSpace spc1 = ctx.getOrCreateSpaceWithSpec(OpenEventSpaceSpecification.class, id1);
			addResult(spc1);
			UUID id2 = UUID.randomUUID();
			addResult(id2);
			OpenEventSpace spc2 = ctx.getOrCreateSpaceWithSpec(OpenEventSpaceSpecification.class, id2);
			addResult(spc2);
			getSkill(Schedules.class).in(100, it -> getSkill(Lifecycle.class).killMe());
			return false;
		}

	}

	@Test
	public void withId() throws Exception {
		runJanus(WithIdAgent.class, false, true, SHORT_TIMEOUT);

		UUID id = getBootAgent();
		assertEquals(5, getNumberOfResults(id));
		
		EventSpace defaultSpace = getResult(id, EventSpace.class, 0);
		assertNotNull(defaultSpace);
		
		UUID spaceId1 = getResult(id, UUID.class, 1);
		assertNotEquals(defaultSpace.getSpaceID().getID(), spaceId1);
		
		Space otherSpace1 = getResult(id, Space.class, 2);
		assertNotNull(otherSpace1);
		assertEquals(spaceId1, otherSpace1.getSpaceID().getID());
		assertNotSame(defaultSpace, otherSpace1);
		assertNotEquals(defaultSpace.getSpaceID(), otherSpace1.getSpaceID());
		assertNotEquals(defaultSpace.getSpaceID().getID(), otherSpace1.getSpaceID().getID());
		
		UUID spaceId2 = getResult(id, UUID.class, 3);
		assertNotEquals(defaultSpace.getSpaceID().getID(), spaceId2);
		assertNotEquals(spaceId1, spaceId2);
		
		Space otherSpace2 = getResult(id, Space.class, 4);
		assertNotNull(otherSpace2);
		assertEquals(spaceId2, otherSpace2.getSpaceID().getID());
		assertNotSame(defaultSpace, otherSpace2);
		assertNotEquals(defaultSpace.getSpaceID(), otherSpace2.getSpaceID());
		assertNotEquals(defaultSpace.getSpaceID().getID(), otherSpace2.getSpaceID().getID());
		assertNotSame(otherSpace1, otherSpace2);
		assertNotEquals(otherSpace1.getSpaceID(), otherSpace2.getSpaceID());
		assertNotEquals(otherSpace1.getSpaceID().getID(), otherSpace2.getSpaceID().getID());
		
		forgetTheKernel();
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	@SarlElementType(SarlPackage.SARL_AGENT)
	static class WithIdAgent extends TestingAgent {

		public WithIdAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			AgentContext ctx = getSkill(DefaultContextInteractions.class).getDefaultContext();
			EventSpace defaultSpace = ctx.getDefaultSpace();
			addResult(defaultSpace);
			UUID id1 = UUID.randomUUID();
			addResult(id1);
			OpenEventSpace spc1 = ctx.getOrCreateSpaceWithID(OpenEventSpaceSpecification.class, id1);
			addResult(spc1);
			UUID id2 = UUID.randomUUID();
			addResult(id2);
			OpenEventSpace spc2 = ctx.getOrCreateSpaceWithID(OpenEventSpaceSpecification.class, id2);
			addResult(spc2);
			getSkill(Schedules.class).in(100, it -> getSkill(Lifecycle.class).killMe());
			return false;
		}

	}

}
