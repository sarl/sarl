/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.api.core.tests.bugs;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid function call resolution.
 *
 * <p>https://github.com/sarl/sarl/issues/1124
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/11124"
 */
@DisplayName("Bug #1124")
@SuppressWarnings("all")
@Tag("core")
public class Bug1124Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1124",
			"import io.sarl.api.core.DefaultContextInteractions",
			"event Evt {",
			"   var symptoms : String",
			"}",
			"capacity DoSometing {",
			"   def setSymptoms(s : String)",
			"}",
			"skill DoSometingSkill implements DoSometing {",
			"   uses DefaultContextInteractions",
			"   def setSymptoms(s : String) {",
			"   }",
			"   def doIt : void {",
			"      var evt = new Evt",
			"      evt.symptoms = \"abc\"",
			"      emit(evt)",
			"   }",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1124;",
			"",
			"import io.sarl.api.core.DefaultContextInteractions;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.lang.core.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class DoSometingSkill extends Skill implements DoSometing {",
			"  public void setSymptoms(final String s) {",
			"  }",
			"",
			"  public void doIt() {",
			"    Evt evt = new Evt();",
			"    evt.symptoms = \"abc\";",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(evt);",
			"  }",
			"",
			"  @Extension",
			"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);",
			"    }",
			"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS);",
			"  }",
			"",
			"  @SyntheticMember",
			"  public DoSometingSkill() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public DoSometingSkill(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing expected code")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling expected code")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1124.DoSometingSkill");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1124",
			"import io.sarl.api.core.DefaultContextInteractions",
			"import java.util.Optional",
			"event Evt {",
			"   var symptoms : String",
			"}",
			"capacity DoSometing {",
			"   def setSymptoms(s : String)",
			"}",
			"skill DoSometingSkill implements DoSometing {",
			"   uses DefaultContextInteractions",
			"   val symptoms : Optional<String> = Optional.empty",
			"   def setSymptoms(s : String) {",
			"   }",
			"   def doIt : void {",
			"      var evt = new Evt",
			"      evt.symptoms = this.symptoms.get",
			"      emit(evt)",
			"   }",
			"}");

	private static final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1124;",
			"",
			"import io.sarl.api.core.DefaultContextInteractions;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.lang.core.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Optional;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class DoSometingSkill extends Skill implements DoSometing {",
			"  private final Optional<String> symptoms = Optional.<String>empty();",
			"",
			"  public void setSymptoms(final String s) {",
			"  }",
			"",
			"  public void doIt() {",
			"    Evt evt = new Evt();",
			"    evt.symptoms = this.symptoms.get();",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(evt);",
			"  }",
			"",
			"  @Extension",
			"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);",
			"    }",
			"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public DoSometingSkill() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public DoSometingSkill(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing err code")
	@Tag("sarlParsing")
	public void parsing02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling err code")
	@Tag("compileToJava")
	public void compiling02() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1124.DoSometingSkill");
			assertEquals(JAVA_CODE_02, actual);
		});
	}

	private static final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1124",
			"import io.sarl.api.core.DefaultContextInteractions",
			"import java.util.Optional",
			"import foo.bug1124.Evt",
			"capacity DoSometing {",
			"   def setSymptoms(s : String)",
			"}",
			"skill DoSometingSkill implements DoSometing {",
			"   uses DefaultContextInteractions",
			"   val symptoms : Optional<String> = Optional.empty",
			"   def setSymptoms(s : String) {",
			"   }",
			"   def doIt : void {",
			"      var evt = new Evt",
			"      evt.symptoms = this.symptoms.get",
			"      emit(evt)",
			"   }",
			"}");

	private static final String JAVA_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1124;",
			"",
			"import foo.bug1124.Evt;",
			"import io.sarl.api.core.DefaultContextInteractions;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.lang.core.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Optional;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class DoSometingSkill extends Skill implements DoSometing {",
			"  private final Optional<String> symptoms = Optional.<String>empty();",
			"",
			"  public void setSymptoms(final String s) {",
			"  }",
			"",
			"  public void doIt() {",
			"    Evt evt = new Evt();",
			"    evt.symptoms = this.symptoms.get();",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(evt);",
			"  }",
			"",
			"  @Extension",
			"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);",
			"    }",
			"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public DoSometingSkill() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public DoSometingSkill(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing err code 2")
	@Tag("sarlParsing")
	public void parsing03() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling err code 2")
	@Tag("compileToJava")
	public void compiling03() throws Exception {
		getCompileHelper().compile(SARL_CODE_03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1124.DoSometingSkill");
			assertEquals(JAVA_CODE_03, actual);
		});
	}

	private static final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1124",
			"import io.sarl.api.core.DefaultContextInteractions",
			"import java.util.Optional",
			"import foo.bug1124.Evt",
			"import foo.bug1124.EvtCapacity",
			"skill DoSometingSkill implements EvtCapacity {",
			"   uses DefaultContextInteractions",
			"   val symptoms : Optional<String> = Optional.empty",
			"   def setSymptoms(s : String) {",
			"   }",
			"   def doIt : void {",
			"      var evt = new Evt",
			"      evt.symptoms = this.symptoms.get",
			"      emit(evt)",
			"   }",
			"}");

	private static final String JAVA_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1124;",
			"",
			"import foo.bug1124.Evt;",
			"import foo.bug1124.EvtCapacity;",
			"import io.sarl.api.core.DefaultContextInteractions;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.Skill;",
			"import io.sarl.lang.core.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.Optional;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class DoSometingSkill extends Skill implements EvtCapacity {",
			"  private final Optional<String> symptoms = Optional.<String>empty();",
			"",
			"  public void setSymptoms(final String s) {",
			"  }",
			"",
			"  public void doIt() {",
			"    Evt evt = new Evt();",
			"    evt.symptoms = this.symptoms.get();",
			"    DefaultContextInteractions _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER = this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER.emit(evt);",
			"  }",
			"",
			"  @Extension",
			"  @ImportedCapacityFeature(DefaultContextInteractions.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS;",
			"",
			"  @SyntheticMember",
			"  @Pure",
			"  private DefaultContextInteractions $CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS == null || this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS = $getSkill(DefaultContextInteractions.class);",
			"    }",
			"    return $castSkill(DefaultContextInteractions.class, this.$CAPACITY_USE$IO_SARL_API_CORE_DEFAULTCONTEXTINTERACTIONS);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"",
			"  @SyntheticMember",
			"  public DoSometingSkill() {",
			"    super();",
			"  }",
			"",
			"  @SyntheticMember",
			"  public DoSometingSkill(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing err code 3")
	@Tag("sarlParsing")
	public void parsing04() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling err code 3")
	@Tag("compileToJava")
	public void compiling04() throws Exception {
		getCompileHelper().compile(SARL_CODE_04, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1124.DoSometingSkill");
			assertEquals(JAVA_CODE_04, actual);
		});
	}

}
