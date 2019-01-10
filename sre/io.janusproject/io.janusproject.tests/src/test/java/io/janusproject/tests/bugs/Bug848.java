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

import static org.junit.Assert.*;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.arakhne.afc.util.Triplet;
import org.eclipse.xtext.util.Triple;
import org.eclipse.xtext.xbase.lib.CollectionExtensions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.AgentKilled;
import io.sarl.core.AgentSpawned;
import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Event;

/** Tests for issue #848: Event AgentSpawned being received by spawned agent.
 *
 * <p>See: https://github.com/sarl/sarl/issues/848
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/848"
 */
@SuppressWarnings("all")
public class Bug848 extends AbstractJanusRunTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class TEvent extends Event {
	}
	
	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class TAgent1a extends TestingAgent {

		public TAgent1a(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			final UUID child = getSkill(Lifecycle.class).spawn(TAgent1b.class, getAgentInitializationParameters());
			addResult(child);
			return false;
		}

		private void onAgentSpawned(AgentSpawned occurrence) {
			Triplet<Boolean, UUID, Collection<UUID>> pair = new Triplet<>(isFromMe(occurrence), occurrence.getSource().getUUID(),
					occurrence.agentIdentifiers);
			addResult(pair);
			forceKillMe();
		}

		@PerceptGuardEvaluator
		private void guard$AgentSpawned(AgentSpawned occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onAgentSpawned(occurrence));
		}

		private void onTEvent(TEvent occurrence) {
			forceKillMe();
		}

		@PerceptGuardEvaluator
		private void guard$TEvent(TEvent occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onTEvent(occurrence));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class TAgent1b extends TestingAgent {

		public TAgent1b(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			return false;
		}

		private void onAgentSpawned(AgentSpawned occurrence) {
			Triplet<Boolean, UUID, Collection<UUID>> pair = new Triplet<>(isFromMe(occurrence), occurrence.getSource().getUUID(),
					occurrence.agentIdentifiers);
			addResult(pair);
			stop();
		}

		private void stop() {
			getSkill(Schedules.class).in(1000, (it) -> {
				getSkill(DefaultContextInteractions.class).emit(new TEvent());
				forceKillMe();
			});
		}

		@PerceptGuardEvaluator
		private void guard$AgentSpawned(AgentSpawned occurrence, Collection<Runnable> handlers) {
			if (!occurrence.agentType.equals(TAgent1a.class.getName())) {
				handlers.add(() -> onAgentSpawned(occurrence));
			} else {
				handlers.add(() -> stop());
			}
		}

	}

	@Test
	public void agentSpawnedReceiving_01() throws Exception {
		runJanus(TAgent1a.class, false);

		UUID bootAgent = getBootAgent();
		assertEquals(2, getNumberOfResults(bootAgent));
		final UUID childAgent = getResult(bootAgent, UUID.class, 0);
		assertNotNull(childAgent);
		final Triplet<Boolean, UUID, Collection<UUID>> spawnEvents1 = getResult(bootAgent, Triplet.class, 1);
		assertNotNull(spawnEvents1);
		assertTrue(spawnEvents1.getA().booleanValue());
		assertEquals(bootAgent, spawnEvents1.getB());
		assertEquals(1, spawnEvents1.getC().size());
		assertEquals(childAgent, spawnEvents1.getC().iterator().next());

		assertEquals(0, getNumberOfResults(childAgent));
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class TAgent2a extends TestingAgent {

		public TAgent2a(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			final UUID child = getSkill(Lifecycle.class).spawn(TAgent2b.class, getAgentInitializationParameters());
			addResult(child);
			return false;
		}

		private void onAgentKilled(AgentKilled occurrence) {
			Pair<Boolean, UUID> pair = Pair.of(isFromMe(occurrence), occurrence.getSource().getUUID());
			addResult(pair);
			forceKillMe();
		}

		@PerceptGuardEvaluator
		private void guard$AgentKilled(AgentKilled occurrence, Collection<Runnable> handlers) {
			handlers.add(() -> onAgentKilled(occurrence));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class TAgent2b extends TestingAgent {

		public TAgent2b(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			stop();
			return false;
		}

		private void onAgentKilled(AgentKilled occurrence) {
			Pair<Boolean, UUID> pair = Pair.of(isFromMe(occurrence), occurrence.getSource().getUUID());
			addResult(pair);
			stop();
		}

		private void stop() {
			getSkill(Schedules.class).in(1000, (it) -> {
				forceKillMe();
			});
		}

		@PerceptGuardEvaluator
		private void guard$AgentKilled(AgentKilled occurrence, Collection<Runnable> handlers) {
			if (!occurrence.agentType.equals(TAgent2a.class.getName())) {
				handlers.add(() -> onAgentKilled(occurrence));
			} else {
				handlers.add(() -> stop());
			}
		}

	}

	@Test
	public void agentKilledReceiving_01() throws Exception {
		runJanus(TAgent2a.class, false);

		UUID bootAgent = getBootAgent();
		assertEquals(2, getNumberOfResults(bootAgent));
		final UUID childAgent = getResult(bootAgent, UUID.class, 0);
		assertNotNull(childAgent);
		final Pair<Boolean, UUID> killEvents1 = getResult(bootAgent, Pair.class, 1);
		assertNotNull(killEvents1);
		assertFalse(killEvents1.getKey().booleanValue());
		assertEquals(childAgent, killEvents1.getValue());

		assertEquals(0, getNumberOfResults(childAgent));
	}

}
