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

import static org.junit.Assert.*;

import java.util.Collection;
import java.util.List;
import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Test;

import io.janusproject.services.executor.EarlyExitException;
import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.Destroy;
import io.sarl.core.Initialize;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.UnimplementedCapacityException;
import io.sarl.lang.util.ClearableReference;

/**
 * Unit test for the issue #609: on Destroy is called two times with killMe.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/609
 */
@SuppressWarnings("all")
public class Bug609 extends AbstractJanusRunTest {

	@Test
	public void killMeInInit() throws Exception {
		runJanus(TAgent1.class, true);
		assertEquals(1, getResults().size());
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class TAgent1 extends TestingAgent {

		public TAgent1(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
			super(provider, parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			forceKillMe();
			return false;
		}

		@PerceptGuardEvaluator
		private void $guardEvaluator$Destroy(final Destroy occurrence, final Collection<Runnable> ___SARLlocal_runnableCollection) {
			assert occurrence != null;
			assert ___SARLlocal_runnableCollection != null;
			___SARLlocal_runnableCollection.add(() -> $behaviorUnit$Destroy$0(occurrence));
		}

		private void $behaviorUnit$Destroy$0(final Destroy occurrence) {
			addResult(occurrence);
		}

	}

}
