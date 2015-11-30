/*
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.generator;

import static org.junit.Assert.assertEquals;

import java.util.Collections;

import com.google.inject.Inject;
import org.eclipse.jdt.annotation.NonNullByDefault;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.serializer.ISerializer;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XbaseFactory;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.ecoregenerator.helper.ECoreGeneratorHelper;
import io.sarl.lang.ecoregenerator.helper.SARLHiddenTokenSequencer;
import io.sarl.lang.ecoregenerator.helper.SarlEcoreCode;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.tests.api.AbstractSarlTest;

/** This class tests the functions of {@link SARLHiddenTokenSequencer}.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	SARLHiddenTokenSequencerTest.AgentComments.class,
	SARLHiddenTokenSequencerTest.EventComments.class,
	SARLHiddenTokenSequencerTest.BehaviorComments.class,
	SARLHiddenTokenSequencerTest.CapacityComments.class,
	SARLHiddenTokenSequencerTest.SkillComments.class,
	SARLHiddenTokenSequencerTest.ValueComments.class,
	SARLHiddenTokenSequencerTest.VariableComments.class,
	SARLHiddenTokenSequencerTest.ActionComments.class,
	SARLHiddenTokenSequencerTest.ActionBlockComments.class,
})
@SuppressWarnings("all")
public class SARLHiddenTokenSequencerTest {
	
	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static abstract class AbstractTokenSequencerTest extends AbstractSarlTest {
		
		@Inject
		protected ISerializer serializer;

		@Inject
		protected ECoreGeneratorHelper generator;

		@NonNullByDefault
		protected SarlEcoreCode code;

		@Before
		public void setUp() {
			XtextResource resource = new XtextResource();
			XtextResourceSet resourceSet = new XtextResourceSet();
			resourceSet.setClasspathURIContext(getClass());
			resourceSet.getResources().add(resource);
			this.code = generator.createScript(resource, "io.sarl.lang.tests");
		}

		protected void assertSerialize(String expected) {
			String text = serializer.serialize(this.code.getSarlScript());
			assertEquals(expected, text);
		}
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static abstract class AbstractMemberTokenSequencerTest extends AbstractTokenSequencerTest {
		
		@NonNullByDefault
		protected SarlAgent container;

		@Before
		public void setUp() {
			super.setUp();
			this.container = generator.createAgent(this.code, "Foo", null);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class AgentComments extends AbstractTokenSequencerTest {

		@NonNullByDefault
		private SarlAgent agent;
		
		@Before
		public void setUp() {
			super.setUp();
			this.agent = generator.createAgent(this.code, "Foo", null);
		}
		
		@Test
		public void prefixComment() {
			generator.attachComment(this.code, this.agent, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\n/* my comment\n*/\nagent Foo {\n}");
		}

		@Test
		public void postfixComment() {
			generator.attachPostComment(code, this.agent, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\nagent Foo {\n}\n\n/* my comment\n*/");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class EventComments extends AbstractTokenSequencerTest {

		@NonNullByDefault
		private SarlEvent event;
		
		@Before
		public void setUp() {
			super.setUp();
			this.event = generator.createEvent(this.code, "Foo", null);
		}
		
		@Test
		public void prefixComment() {
			generator.attachComment(this.code, this.event, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\n/* my comment\n*/\nevent Foo");
		}

		@Test
		public void postfixComment() {
			generator.attachPostComment(code, this.event, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\nevent Foo\n\n/* my comment\n*/");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class BehaviorComments extends AbstractTokenSequencerTest {

		@NonNullByDefault
		private SarlBehavior behavior;
		
		@Before
		public void setUp() {
			super.setUp();
			this.behavior = generator.createBehavior(this.code, "Foo", null);
		}
		
		@Test
		public void prefixComment() {
			generator.attachComment(this.code, this.behavior, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\n/* my comment\n*/\nbehavior Foo {\n}");
		}

		@Test
		public void postfixComment() {
			generator.attachPostComment(code, this.behavior, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\nbehavior Foo {\n}\n\n/* my comment\n*/");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class CapacityComments extends AbstractTokenSequencerTest {

		@NonNullByDefault
		private SarlCapacity capacity;
		
		@Before
		public void setUp() {
			super.setUp();
			this.capacity = generator.createCapacity(this.code, "Foo", null);
		}
		
		@Test
		public void prefixComment() {
			generator.attachComment(this.code, this.capacity, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\n/* my comment\n*/\ncapacity Foo {\n}");
		}

		@Test
		public void postfixComment() {
			generator.attachPostComment(code, this.capacity, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\ncapacity Foo {\n}\n\n/* my comment\n*/");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class SkillComments extends AbstractTokenSequencerTest {

		@NonNullByDefault
		private SarlSkill skill;
		
		@Before
		public void setUp() {
			super.setUp();
			this.skill = generator.createSkill(this.code, "Foo", null, Collections.<String>emptyList());
		}
		
		@Test
		public void prefixComment() {
			generator.attachComment(this.code, this.skill, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\n/* my comment\n*/\nskill Foo {\n}");
		}

		@Test
		public void postfixComment() {
			generator.attachPostComment(code, this.skill, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\nskill Foo {\n}\n\n/* my comment\n*/");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ValueComments extends AbstractMemberTokenSequencerTest {

		@NonNullByDefault
		private SarlField field;
		
		@Before
		public void setUp() {
			super.setUp();
			this.field = generator.createValue(this.code, this.container, "foo", "int");
		}
		
		@Test
		public void prefixComment() {
			generator.attachComment(this.code, this.field, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\nagent Foo {\n\n/* my comment\n*/\nval foo : int\n}");
		}

		@Test
		public void postfixComment() {
			generator.attachPostComment(code, this.field, "/* my comment\n*/");
			assertSerialize(
					"package io.sarl.lang.tests\n\nagent Foo {\n	val foo : int\n	\n/* my comment\n*/\n}");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class VariableComments extends AbstractMemberTokenSequencerTest {

		@NonNullByDefault
		private SarlField field;
		
		@Before
		public void setUp() {
			super.setUp();
			this.field = generator.createVariable(this.code, this.container, "foo", "int");
		}
		
		@Test
		public void prefixComment() {
			generator.attachComment(this.code, this.field, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\nagent Foo {\n\n/* my comment\n*/\nvar foo : int\n}");
		}

		@Test
		public void postfixComment() {
			generator.attachPostComment(code, this.field, "/* my comment\n*/");
			assertSerialize(
					"package io.sarl.lang.tests\n\nagent Foo {\n	var foo : int\n	\n/* my comment\n*/\n}");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ActionComments extends AbstractMemberTokenSequencerTest {

		@NonNullByDefault
		private SarlAction action;
		
		@Before
		public void setUp() {
			super.setUp();
			this.action = generator.createAction(this.code, this.container, "foo", "int", null);
		}
		
		@Test
		public void prefixComment() {
			generator.attachComment(this.code, this.action, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\nagent Foo {\n\n/* my comment\n*/\ndef foo : int {\n// TODO Auto-generated action.\n		0\n	}\n}");
		}

		@Test
		public void postfixComment() {
			generator.attachPostComment(code, this.action, "/* my comment\n*/");
			assertSerialize(
				"package io.sarl.lang.tests\n\nagent Foo {\n\n	def foo : int {\n	// TODO Auto-generated action.\n		0\n	}\n	\n/* my comment\n*/\n}");
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ActionBlockComments extends AbstractMemberTokenSequencerTest {

		@NonNullByDefault
		private XBlockExpression block;
		
		@Before
		public void setUp() {
			super.setUp();
			this.block = XbaseFactory.eINSTANCE.createXBlockExpression();
			generator.createAction(this.code, this.container, "foo", null, this.block);
		}

		@Test
		public void blockComment() {
			generator.attachInnerComment(code, this.block, "// my comment\n");
			assertSerialize(
				"package io.sarl.lang.tests\n\nagent Foo {\n\n	def foo {\n	// my comment\n	}\n}");
		}

	}

}
