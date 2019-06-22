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

package io.janusproject.tests;

import java.util.UUID;

import com.google.inject.Module;
import org.junit.Test;

import io.janusproject.Bootstrap;
import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.bootstrap.SRE;
import io.sarl.bootstrap.SREBootstrap;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ShutdownTest extends AbstractJanusRunTest {

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	protected static class Agent1Mock extends TestingAgent {

		public Agent1Mock(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			getSkill(Schedules.class).in(1000, it -> forceKillMe());
			return false;
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	protected static class Agent2Mock extends TestingAgent {

		public Agent2Mock(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			return false;
		}

	}

	@Test
	public void shutdownWithoutAgent() throws Exception {
		final Module injectionModule = prepareJanus(false, true, getDefaultJanusModule());
		startJanusKernel(injectionModule);

		SREBootstrap bootstrap = getTestingBootstrap();

		final TimeOutHandler handler = startTimeOut();
		try {
			bootstrap.shutdown(true);
		} finally {
			handler.stop();
		}
		assertFalse(getTestingKernel().isRunning());
		assertEquals(0, getTestingKernel().getAgentCount());
	}

	@Test
	public void shutdownWithAgent1() throws Exception {
		startJanusWithDefaultProcess(Agent2Mock.class, false, true, getDefaultJanusModule());

		SREBootstrap bootstrap = getTestingBootstrap();

		final TimeOutHandler handler = startTimeOut();
		try {
			bootstrap.shutdown(true);
		} finally {
			handler.stop();
		}
		assertFalse(getTestingKernel().isRunning());
		assertEquals(0, getTestingKernel().getAgentCount());
	}

	@Test
	public void shutdownWithAgent2() throws Exception {
		startJanusWithDefaultProcess(Agent2Mock.class, false, true, getDefaultJanusModule());

		SREBootstrap bootstrap = getTestingBootstrap();

		final TimeOutHandler handler = startTimeOut();
		try {
			bootstrap.shutdown(true);
		} finally {
			handler.stop();
		}
		assertFalse(getTestingKernel().isRunning());
		assertEquals(0, getTestingKernel().getAgentCount());
	}

}
