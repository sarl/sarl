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

import io.sarl.core.AgentKilled;
import io.sarl.core.InnerContextAccess;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSpecification;

/**
 * Unit test for the issue #794: Spawn function first parameter strange behavior.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/794
 */
@SuppressWarnings("all")
public class Bug794 extends AbstractJanusRunTest {

	@Test
	public void spawn1() throws Exception {
		runJanus(SpawnerAgent1.class, false, true, STANDARD_TIMEOUT);
		assertEquals(1, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent1 extends AbstractSpawnerAgent {

		public SpawnerAgent1(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 1;
		}

	}

	@Test
	public void spawn2() throws Exception {
		runJanus(SpawnerAgent2.class, false, true, STANDARD_TIMEOUT);
		assertEquals(2, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent2 extends AbstractSpawnerAgent {

		public SpawnerAgent2(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 2;
		}

	}

	@Test
	public void spawn3() throws Exception {
		runJanus(SpawnerAgent3.class, false, true, STANDARD_TIMEOUT);
		assertEquals(3, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent3 extends AbstractSpawnerAgent {

		public SpawnerAgent3(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 3;
		}

	}

	@Test
	public void spawn4() throws Exception {
		runJanus(SpawnerAgent4.class, false, true, STANDARD_TIMEOUT);
		assertEquals(4, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent4 extends AbstractSpawnerAgent {

		public SpawnerAgent4(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 4;
		}

	}

	@Test
	public void spawn5() throws Exception {
		runJanus(SpawnerAgent5.class, false, true, STANDARD_TIMEOUT);
		assertEquals(5, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent5 extends AbstractSpawnerAgent {

		public SpawnerAgent5(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 5;
		}

	}

	@Test
	public void spawn6() throws Exception {
		runJanus(SpawnerAgent6.class, false, true, STANDARD_TIMEOUT);
		assertEquals(6, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent6 extends AbstractSpawnerAgent {

		public SpawnerAgent6(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 6;
		}

	}

	@Test
	public void spawn7() throws Exception {
		runJanus(SpawnerAgent7.class, false, true, STANDARD_TIMEOUT);
		assertEquals(7, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent7 extends AbstractSpawnerAgent {

		public SpawnerAgent7(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 7;
		}

	}

	@Test
	public void spawn8() throws Exception {
		runJanus(SpawnerAgent8.class, false, true, STANDARD_TIMEOUT);
		assertEquals(8, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent8 extends AbstractSpawnerAgent {

		public SpawnerAgent8(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 8;
		}

	}

	@Test
	public void spawn9() throws Exception {
		runJanus(SpawnerAgent9.class, false, true, STANDARD_TIMEOUT);
		assertEquals(9, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent9 extends AbstractSpawnerAgent {

		public SpawnerAgent9(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 9;
		}

	}

	@Test
	public void spawn10() throws Exception {
		runJanus(SpawnerAgent10.class, false, true, STANDARD_TIMEOUT);
		assertEquals(10, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent10 extends AbstractSpawnerAgent {

		public SpawnerAgent10(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 10;
		}

	}

	@Test
	public void spawn200() throws Exception {
		runJanus(SpawnerAgent200.class, false, true, STANDARD_TIMEOUT);
		assertEquals(200, getResults().size());
		assertAllDifferents(getResults());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnerAgent200 extends AbstractSpawnerAgent {

		public SpawnerAgent200(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected int getAgentsToSpawn() {
			return 200;
		}

	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static abstract class AbstractSpawnerAgent extends TestingAgent {

		public AbstractSpawnerAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		protected abstract int getAgentsToSpawn();
		
		@Override
		protected boolean runAgentTest() {
			spawnChildren();
			return true;
		}

		private void spawnChildren() {
			Iterable<UUID> ids = getSkill(Lifecycle.class).spawnInContext(getAgentsToSpawn(), SpawnedAgent.class,
					getSkill(InnerContextAccess.class).getInnerContext(),
					getAgentInitializationParameters());

			for (final UUID id : ids) {
				addResult(id);
			}
		}

		@PerceptGuardEvaluator
		private void guardOnAgentKilled(final AgentKilled event, Collection<Runnable> handlers) {
			handlers.add(() -> onAgentKilled(event));
		}

		private void onAgentKilled(AgentKilled event) {
			final int children = getSkill(InnerContextAccess.class).getMemberAgentCount();
			if (children <= 1) {
				forceKillMe();
			}
		}

	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpawnedAgent extends TestingAgent {

		public SpawnedAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			getSkill(Schedules.class).in(1000, (it) -> forceKillMe());
			return false;
		}

	}

}
