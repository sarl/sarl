/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.tests.bugs.to00999;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Enable static modifier for functions in skill.
 *
 * <p>https://github.com/sarl/sarl/issues/967
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/967"
 */
@DisplayName("Bug #967")
@SuppressWarnings("all")
@Tag("core")
public class Bug967Test {

	/** In agent.
	 * 
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("In agent")
	public class InAgent extends AbstractSarlTest {
		
		private final String SARL_CODE_MODIFIABLE_FIELD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"agent MyAgent {",
				"  static var field : int",
				"  def getField : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Modifiable field parsing")
		@Tag("sarlValidation")
		public void parsingModifiableField() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_MODIFIABLE_FIELD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
					"Potential problem of data sharing outside the control of the agent")
				.assertNoIssues();
		}

		@Test
		@DisplayName("Modifiable field compiling")
		@Tag("compileToJava")
		public void compileModifiableField() throws Exception {
			getCompileHelper().compile(SARL_CODE_MODIFIABLE_FIELD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyAgent");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyAgent extends Agent {",
						"  private static int field;",
						"  ",
						"  @Pure",
						"  protected int getField() {",
						"    return MyAgent.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyAgent(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_UNMODIFIABLE_FIELD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"agent MyAgent {",
				"  static val field : int = 34",
				"  def getField : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Unmodifiable field parsing")
		@Tag("sarlValidation")
		public void parsingUnmodifiableField() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_UNMODIFIABLE_FIELD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@DisplayName("Unmodifiable field compiling")
		@Tag("compileToJava")
		public void compileUnmodifiableField() throws Exception {
			getCompileHelper().compile(SARL_CODE_UNMODIFIABLE_FIELD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyAgent");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyAgent extends Agent {",
						"  private static final int field = 34;",
						"  ",
						"  @Pure",
						"  protected int getField() {",
						"    return MyAgent.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyAgent(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_METHOD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"agent MyAgent {",
				"  static def getGlobalValue : int {",
				"    return 34",
				"  }",
				"}");

		@Test
		@DisplayName("Method parsing")
		@Tag("sarlValidation")
		public void parsingMethod() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_METHOD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@DisplayName("Method compiling")
		@Tag("compileToJava")
		public void compileMethod() throws Exception {
			getCompileHelper().compile(SARL_CODE_METHOD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyAgent");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyAgent extends Agent {",
						"  @Pure",
						"  protected static int getGlobalValue() {",
						"    return 34;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyAgent(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""), actual);
			});
		}

	}

	/** In behavior.
	 * 
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("In behavior")
	public class InBehavior extends AbstractSarlTest {
		
		private final String SARL_CODE_MODIFIABLE_FIELD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"behavior MyBehavior {",
				"  static var field : int",
				"  def getField : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Modifiable field parsing")
		@Tag("sarlValidation")
		public void parsingModifiableField() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_MODIFIABLE_FIELD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
					"Potential problem of data sharing outside the control of the agent")
				.assertNoIssues();
		}

		@Test
		@DisplayName("Modifiable field compiling")
		@Tag("compileToJava")
		public void compileModifiableField() throws Exception {
			getCompileHelper().compile(SARL_CODE_MODIFIABLE_FIELD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyBehavior");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyBehavior extends Behavior {",
						"  private static int field;",
						"  ",
						"  @Pure",
						"  public int getField() {",
						"    return MyBehavior.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyBehavior(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_UNMODIFIABLE_FIELD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"behavior MyBehavior {",
				"  static val field : int = 34",
				"  def getField : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Unmodifiable field parsing")
		@Tag("sarlValidation")
		public void parsingUnmodifiableField() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_UNMODIFIABLE_FIELD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@DisplayName("Unmodifiable field compiling")
		@Tag("compileToJava")
		public void compileUnmodifiableField() throws Exception {
			getCompileHelper().compile(SARL_CODE_UNMODIFIABLE_FIELD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyBehavior");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyBehavior extends Behavior {",
						"  private static final int field = 34;",
						"  ",
						"  @Pure",
						"  public int getField() {",
						"    return MyBehavior.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyBehavior(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_METHOD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"behavior MyBehavior {",
				"  static def getGlobalValue : int {",
				"    return 34",
				"  }",
				"}");

		@Test
		@DisplayName("Method parsing")
		@Tag("sarlValidation")
		public void parsingMethod() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_METHOD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@DisplayName("Method compiling")
		@Tag("compileToJava")
		public void compileMethod() throws Exception {
			getCompileHelper().compile(SARL_CODE_METHOD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyBehavior");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyBehavior extends Behavior {",
						"  @Pure",
						"  public static int getGlobalValue() {",
						"    return 34;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyBehavior(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""), actual);
			});
		}

	}

	/** In skill.
	 * 
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("In skill")
	public class InSkill extends AbstractSarlTest {
		
		private final String SARL_CODE_MODIFIABLE_FIELD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"capacity MyCapacity {",
				"  def myfct : int",
				"}",
				"skill MySkill implements MyCapacity {",
				"  static var field : int",
				"  def myfct : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Modifiable field parsing")
		@Tag("sarlValidation")
		public void parsingModifiableField() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_MODIFIABLE_FIELD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
					"Potential problem of data sharing outside the control of the agent")
				.assertNoIssues();
		}

		@Test
		@DisplayName("Modifiable field compiling")
		@Tag("compileToJava")
		public void compileModifiableField() throws Exception {
			getCompileHelper().compile(SARL_CODE_MODIFIABLE_FIELD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MySkill");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Skill;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
						"@SuppressWarnings(\"all\")",
						"public class MySkill extends Skill implements MyCapacity {",
						"  private static int field;",
						"  ",
						"  public int myfct() {",
						"    return MySkill.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MySkill() {",
						"    super();",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MySkill(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_UNMODIFIABLE_FIELD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"capacity MyCapacity {",
				"  def myfct : int",
				"}",
				"skill MySkill implements MyCapacity {",
				"  static val field : int = 34",
				"  def myfct : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Unmodifiable field parsing")
		@Tag("sarlValidation")
		public void parsingUnmodifiableField() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_UNMODIFIABLE_FIELD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@DisplayName("Unmodifiable field compiling")
		@Tag("compileToJava")
		public void compileUnmodifiableField() throws Exception {
			getCompileHelper().compile(SARL_CODE_UNMODIFIABLE_FIELD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MySkill");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Skill;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
						"@SuppressWarnings(\"all\")",
						"public class MySkill extends Skill implements MyCapacity {",
						"  private static final int field = 34;",
						"  ",
						"  public int myfct() {",
						"    return MySkill.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MySkill() {",
						"    super();",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MySkill(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_METHOD = multilineString(
				"package io.sarl.lang.tests.bug967",
				"capacity MyCapacity {",
				"  def myfct : int",
				"}",
				"skill MySkill implements MyCapacity {",
				"  static def getGlobalValue : int {",
				"    return 34",
				"  }",
				"  def myfct : int {getGlobalValue}",
				"}");

		@Test
		@DisplayName("Method parsing")
		@Tag("sarlValidation")
		public void parsingMethod() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_METHOD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@DisplayName("Method compiling")
		@Tag("compileToJava")
		public void compileMethod() throws Exception {
			getCompileHelper().compile(SARL_CODE_METHOD, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MySkill");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Skill;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
						"@SuppressWarnings(\"all\")",
						"public class MySkill extends Skill implements MyCapacity {",
						"  @Pure",
						"  public static int getGlobalValue() {",
						"    return 34;",
						"  }",
						"  ",
						"  public int myfct() {",
						"    return MySkill.getGlobalValue();",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MySkill() {",
						"    super();",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MySkill(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""), actual);
			});
		}

	}

	/** Check field cases.
	 * 
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Nested
	@DisplayName("Field cases")
	public class FieldCase extends AbstractSarlTest {

		private final String SARL_CODE_MODIFIABLE_FIELD_NO_INIT = multilineString(
				"package io.sarl.lang.tests.bug967",
				"agent MyAgent {",
				"  static var field : int",
				"  def myfct : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Modifiable field without init parsing")
		@Tag("sarlValidation")
		public void parsingModifiableField() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_MODIFIABLE_FIELD_NO_INIT);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
					"Potential problem of data sharing outside the control of the agent")
				.assertNoIssues();
		}

		@Test
		@DisplayName("Modifiable field without init compiling")
		@Tag("compileToJava")
		public void compileModifiableField() throws Exception {
			getCompileHelper().compile(SARL_CODE_MODIFIABLE_FIELD_NO_INIT, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyAgent");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyAgent extends Agent {",
						"  private static int field;",
						"  ",
						"  @Pure",
						"  protected int myfct() {",
						"    return MyAgent.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyAgent(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_MODIFIABLE_FIELD_CONSTANT_INIT = multilineString(
				"package io.sarl.lang.tests.bug967",
				"agent MyAgent {",
				"  static var field : int = 34",
				"  def myfct : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Modifiable field with constant init parsing")
		@Tag("sarlValidation")
		public void parsingModifiableFieldConstantInit() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_MODIFIABLE_FIELD_CONSTANT_INIT);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
					"Potential problem of data sharing outside the control of the agent")
				.assertNoIssues();
		}

		@Test
		@DisplayName("Modifiable field with constant init compiling")
		@Tag("compileToJava")
		public void compileModifiableFieldConstantInit() throws Exception {
			getCompileHelper().compile(SARL_CODE_MODIFIABLE_FIELD_CONSTANT_INIT, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyAgent");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyAgent extends Agent {",
						"  private static int field = 34;",
						"  ",
						"  @Pure",
						"  protected int myfct() {",
						"    return MyAgent.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyAgent(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_MODIFIABLE_FIELD_NO_CONSTANT_INIT = multilineString(
				"package io.sarl.lang.tests.bug967",
				"import java.util.ArrayList",
				"agent MyAgent {",
				"  static var field : ArrayList<Object> = new ArrayList",
				"  def myfct : ArrayList<Object> {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Modifiable field with no constant init parsing")
		@Tag("sarlValidation")
		public void parsingModifiableFieldNoConstantInit() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_MODIFIABLE_FIELD_NO_CONSTANT_INIT);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
					"Potential problem of data sharing outside the control of the agent")
				.assertNoIssues();
		}

		@Test
		@DisplayName("Modifiable field with no constant init compiling")
		@Tag("compileToJava")
		public void compileModifiableFieldNoConstantInit() throws Exception {
			getCompileHelper().compile(SARL_CODE_MODIFIABLE_FIELD_NO_CONSTANT_INIT, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyAgent");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.ArrayList;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyAgent extends Agent {",
						"  private static ArrayList<Object> field = new ArrayList<Object>();",
						"  ",
						"  @Pure",
						"  protected ArrayList<Object> myfct() {",
						"    return MyAgent.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyAgent(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_UNMODIFIABLE_FIELD_NO_INIT = multilineString(
				"package io.sarl.lang.tests.bug967",
				"behavior MyBehavior {",
				"  static val field : int",
				"  static new {",
				"    field = 34",
				"  }",
				"  def myfct : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Unmodifiable field without init parsing")
		@Tag("sarlValidation")
		public void parsingUnodifiableField() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_UNMODIFIABLE_FIELD_NO_INIT);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
					"Potential problem of data sharing outside the control of the agent")
				.assertNoIssues();
		}

		@Test
		@DisplayName("Unmodifiable field without init compiling")
		@Tag("compileToJava")
		public void compileUnodifiableField() throws Exception {
			getCompileHelper().compile(SARL_CODE_UNMODIFIABLE_FIELD_NO_INIT, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyBehavior");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyBehavior extends Behavior {",
						"  private static final int field;",
						"  ",
						"  static {",
						"    MyBehavior.field = 34;",
						"  }",
						"  ",
						"  @Pure",
						"  public int myfct() {",
						"    return MyBehavior.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyBehavior(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_UNMODIFIABLE_FIELD_CONSTANT_INIT = multilineString(
				"package io.sarl.lang.tests.bug967",
				"agent MyAgent {",
				"  static val field : int = 34",
				"  def myfct : int {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Unmodifiable field with constant init parsing")
		@Tag("sarlValidation")
		public void parsingUnmodifiableFieldConstantInit() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_UNMODIFIABLE_FIELD_CONSTANT_INIT);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@DisplayName("Unmodifiable field with constant init compiling")
		@Tag("compileToJava")
		public void compileUnmodifiableFieldConstantInit() throws Exception {
			getCompileHelper().compile(SARL_CODE_UNMODIFIABLE_FIELD_CONSTANT_INIT, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyAgent");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyAgent extends Agent {",
						"  private static final int field = 34;",
						"  ",
						"  @Pure",
						"  protected int myfct() {",
						"    return MyAgent.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyAgent(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""), actual);
			});
		}

		private final String SARL_CODE_UNMODIFIABLE_FIELD_NO_CONSTANT_INIT = multilineString(
				"package io.sarl.lang.tests.bug967",
				"import java.util.ArrayList",
				"agent MyAgent {",
				"  static val field : ArrayList<Object> = new ArrayList",
				"  def myfct : ArrayList<Object> {",
				"    return field",
				"  }",
				"}");

		@Test
		@DisplayName("Unmodifiable field with no constant init parsing")
		@Tag("sarlValidation")
		public void parsingUnmodifiableFieldNoConstantInit() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_UNMODIFIABLE_FIELD_NO_CONSTANT_INIT);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL,
					"Potential problem of data sharing outside the control of the agent")
				.assertNoIssues();
		}

		@Test
		@DisplayName("Unmodifiable field with no constant init compiling")
		@Tag("compileToJava")
		public void compileUnmodifiableFieldNoConstantInit() throws Exception {
			getCompileHelper().compile(SARL_CODE_UNMODIFIABLE_FIELD_NO_CONSTANT_INIT, (it) -> {
				final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug967.MyAgent");
				assertEquals(multilineString(
						"package io.sarl.lang.tests.bug967;",
						"",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.ArrayList;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class MyAgent extends Agent {",
						"  private static final ArrayList<Object> field = new ArrayList<Object>();",
						"  ",
						"  @Pure",
						"  protected ArrayList<Object> myfct() {",
						"    return MyAgent.field;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public MyAgent(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public MyAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""), actual);
			});
		}

	}

}
