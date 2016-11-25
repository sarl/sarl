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
import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure0;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.junit.Test;

import io.janusproject.tests.testutils.AbstractJanusRunTest;

import io.sarl.core.Behaviors;
import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Destroy;
import io.sarl.core.Initialize;
import io.sarl.core.Lifecycle;
import io.sarl.core.Schedules;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.ImportedCapacityFeature;
import io.sarl.lang.annotation.PerceptGuardEvaluator;
import io.sarl.lang.annotation.SarlSpecification;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.ClearableReference;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.UnimplementedCapacityException;

/**
 * Unit test for the issue #543: Incomplete reset of the hidden buffers to skills.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/543
 */
@SuppressWarnings("all")
public class BugS543 extends AbstractJanusRunTest {

	@Test
	public void setSkill() throws Exception {
		runJanus(TAgent1.class, false);
		assertEquals(2, getNumberOfResults());
		assertEquals("1", getResult(String.class, 0));
		assertEquals("2", getResult(String.class, 1));
	}

	@Test
	public void clearSkill() throws Exception {
		runJanus(TAgent2.class, false);
		assertEquals(2, getNumberOfResults());
		assertEquals("1", getResult(String.class, 0));
		UnimplementedCapacityException ex = getResult(UnimplementedCapacityException.class, 1);
		assertNotNull(ex);
		assertEquals(C1.class, ex.getUnimplementedCapacity());
	}

	@Test
	public void doubleHiddenGetters() throws Exception {
		runJanus(TAgent3.class, false);
		assertEquals(1, getNumberOfResults());
		assertEquals(Boolean.TRUE, getResult(Boolean.class, 0));
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static interface C1 extends Capacity {
		void fct();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class S1 extends Skill implements C1 {
		private final Procedure1<String> proc;
		public S1(Procedure1<String> fct) {
			this.proc = fct;
		}
		@Override
		public void fct() {
			this.proc.apply("1");
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
	public static class S2 extends Skill implements C1 {
		private final Procedure1<String> proc;
		public S2(Procedure1<String> fct) {
			this.proc = fct;
		}
		@Override
		public void fct() {
			this.proc.apply("2");
		}
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

		private transient ClearableReference<Skill> $CAPACITY_USE$C1;

		private C1 $CAPACITY_USE$C1$CALLER() {
			if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {
				this.$CAPACITY_USE$C1 = $getSkill(C1.class);
			}
			return $castSkill(C1.class, this.$CAPACITY_USE$C1);
		}

		@Override
		protected boolean runAgentTest() {
			setSkill(new S1((it) -> addResult(it)), C1.class);
			//
			C1 _$CAPACITY_USE$C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);
			_$CAPACITY_USE$C1$CALLER.fct();
			//
			setSkill(new S2((it) -> addResult(it)), C1.class);
			//
			C1 _$CAPACITY_USE$C1$CALLER1 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);
			_$CAPACITY_USE$C1$CALLER1.fct();
			//
			getSkill(Lifecycle.class).killMe();
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
	public static class TAgent2 extends TestingAgent {

		public TAgent2(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
			super(provider, parentID, agentID);
		}

		private transient ClearableReference<Skill> $CAPACITY_USE$C1;

		private C1 $CAPACITY_USE$C1$CALLER() {
			if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {
				this.$CAPACITY_USE$C1 = $getSkill(C1.class);
			}
			return $castSkill(C1.class, this.$CAPACITY_USE$C1);
		}

		@Override
		protected boolean runAgentTest() {
			setSkill(new S1((it) -> addResult(it)), C1.class);
			//
			C1 _$CAPACITY_USE$C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);
			_$CAPACITY_USE$C1$CALLER.fct();
			//
			clearSkill(C1.class);
			//
			C1 _$CAPACITY_USE$C1$CALLER1 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);
			//
			getSkill(Lifecycle.class).killMe();
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
	public static class TAgent3 extends TestingAgent {

		public TAgent3(BuiltinCapacitiesProvider provider, UUID parentID, UUID agentID) {
			super(provider, parentID, agentID);
		}

		private transient ClearableReference<Skill> $CAPACITY_USE$C1;

		private C1 $CAPACITY_USE$C1$CALLER() {
			if (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) {
				this.$CAPACITY_USE$C1 = $getSkill(C1.class);
			}
			return $castSkill(C1.class, this.$CAPACITY_USE$C1);
		}

		@Override
		protected boolean runAgentTest() {
			setSkill(new S1((it) -> addResult(it)), C1.class);
			//
			C1 _$CAPACITY_USE$C1$CALLER = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);
			C1 _$CAPACITY_USE$C1$CALLER1 = this.$castSkill(C1.class, (this.$CAPACITY_USE$C1 == null || this.$CAPACITY_USE$C1.get() == null) ? (this.$CAPACITY_USE$C1 = $getSkill(C1.class)) : this.$CAPACITY_USE$C1);
			addResult(Boolean.valueOf(_$CAPACITY_USE$C1$CALLER == _$CAPACITY_USE$C1$CALLER1));
			//
			getSkill(Lifecycle.class).killMe();
			return false;
		}

	}

}
