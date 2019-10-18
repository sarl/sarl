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

import java.util.UUID;

import javax.inject.Inject;

import com.google.inject.name.Named;
import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.OpenEventSpace;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.DefaultSpace;
import io.sarl.util.concurrent.Collections3;

/**
 * Unit test for the issue #66: Injection of the default space in a Space implementation.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/66
 */
@SuppressWarnings("all")
public class Bug66 extends AbstractJanusRunTest {

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public interface MySpace extends Space {
		OpenEventSpace getTheOtherSpace();
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class MySpaceImpl implements MySpace {

		private final SpaceID id;

		private final OpenEventSpace otherSpace;
		
		public MySpaceImpl(SpaceID id, OpenEventSpace otherSpace) {
			this.id = id;
			this.otherSpace = otherSpace;
		}
		
		public OpenEventSpace getTheOtherSpace() {
			return this.otherSpace;
		}

		@Override
		public SpaceID getID() {
			return getSpaceID();
		}

		@Override
		public SpaceID getSpaceID() {
			return this.id;
		}

		@Override
		public SynchronizedSet<UUID> getParticipants() {
			return Collections3.emptySynchronizedSet();
		}

	}

	@Test
	public void injectionWithNamedAnnotation() throws Exception {
		runJanus(SpaceCreatorWithNamedAnnotationAgent.class, false);
		assertEquals(2, getNumberOfResults());
		final EventSpace dftSpc = getResult(EventSpace.class, 0);
		assertNotNull(dftSpc);
		final MySpace createdSpc = getResult(MySpace.class, 1);
		assertSame(dftSpc, createdSpc.getTheOtherSpace());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class MySpaceSpecificationWithNamedAnnotation implements SpaceSpecification<MySpace> {

		@Inject
		@Named("defaultSpace")
		private OpenEventSpace dftSpc;
		
		@Override
		public MySpace create(SpaceID id, Object... params) {
			return new MySpaceImpl(id, this.dftSpc);
		}
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpaceCreatorWithNamedAnnotationAgent extends TestingAgent {

		public SpaceCreatorWithNamedAnnotationAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			final AgentContext ctx = getSkill(DefaultContextInteractions.class).getDefaultContext();
			final EventSpace dftSpc = ctx.getDefaultSpace();
			final MySpace space = ctx.getOrCreateSpaceWithSpec(MySpaceSpecificationWithNamedAnnotation.class, UUID.randomUUID());
			addResult(dftSpc);
			addResult(space);
			return true;
		}

	}

	@Test
	public void injectionWithDefaultSpaceAnnotation() throws Exception {
		runJanus(SpaceCreatorWithDefaultSpaceAnnotationAgent.class, false);
		assertEquals(2, getNumberOfResults());
		final EventSpace dftSpc = getResult(EventSpace.class, 0);
		assertNotNull(dftSpc);
		final MySpace createdSpc = getResult(MySpace.class, 1);
		assertSame(dftSpc, createdSpc.getTheOtherSpace());
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class MySpaceSpecificationWithDefaultSpaceAnnotation implements SpaceSpecification<MySpace> {

		@Inject
		@DefaultSpace
		private OpenEventSpace dftSpc;
		
		@Override
		public MySpace create(SpaceID id, Object... params) {
			return new MySpaceImpl(id, this.dftSpc);
		}
	}

	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class SpaceCreatorWithDefaultSpaceAnnotationAgent extends TestingAgent {

		public SpaceCreatorWithDefaultSpaceAnnotationAgent(UUID parentID, UUID agentID) {
			super(parentID, agentID);
		}

		@Override
		protected boolean runAgentTest() {
			final AgentContext ctx = getSkill(DefaultContextInteractions.class).getDefaultContext();
			final EventSpace dftSpc = ctx.getDefaultSpace();
			final MySpace space = ctx.getOrCreateSpaceWithSpec(MySpaceSpecificationWithDefaultSpaceAnnotation.class, UUID.randomUUID());
			addResult(dftSpc);
			addResult(space);
			return true;
		}

	}

}
