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

package io.janusproject.bugs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;

import java.net.InetAddress;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

import io.janusproject.Boot;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.testutils.AbstractJanusRunTest;
import io.sarl.core.Lifecycle;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.BuiltinCapacitiesProvider;

/**
 * Unit test for the issue #84: Problem with calling killMe in Initialize behavior of an agent.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/janus-project/janusproject/issues/84
 */
@SuppressWarnings("all")
public class Bug84 extends AbstractJanusRunTest {

	@Before
	public void setUp() {
		Boot.setOffline(false);
	}

	@Test
	public void killMeInInit() throws Exception {
		InetAddress adr = NetworkUtil.getPrimaryAddress();
		assumeTrue("Cannot unit test the Bug84 when the network is down.", adr != null && !adr.isLoopbackAddress());
		//
		runJanus(KilledInInitAgent.class, true);
		assertEquals(-1, indexOfResult(Throwable.class));
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class KilledInInitAgent extends TestingAgent {

		public KilledInInitAgent(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
			super(provider, parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			getSkill(Lifecycle.class).killMe();
			return false;
		}

	}

}
