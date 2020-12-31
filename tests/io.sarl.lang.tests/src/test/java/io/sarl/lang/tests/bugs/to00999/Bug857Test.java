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

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid missed return value after killMe.
 *
 * <p>https://github.com/sarl/sarl/issues/857
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/857"
 */
@DisplayName("Bug #857")
@SuppressWarnings("all")
@Tag("core")
public class Bug857Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug857",
			"import io.sarl.core.Lifecycle",
			"agent SomeAgent {",
			"  uses Lifecycle",
			"  def runAgentTest : boolean {",
			"       killMe",
			"       return false",
			"  }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"package io.sarl.lang.tests.bug857;",
			"",
			"import io.sarl.core.Lifecycle;",
			"import io.sarl.lang.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  protected boolean runAgentTest() {",
			"    throw new Error(\"Unresolved compilation problems:\"",
			"      + \"\\nUnreachable expression.\");",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(Lifecycle.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  private Lifecycle $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = $getSkill(Lifecycle.class);",
			"    }",
			"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXReturnExpression(),
				IssueCodes.UNREACHABLE_CODE,
				"Unreachable expression");
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug857.SomeAgent");
			assertEquals(EXPECTED01, actual);
		});
	}

	private static final String SNIPSET02 = multilineString(
			"package io.sarl.lang.tests.bug857",
			"import io.sarl.core.Lifecycle",
			"agent SomeAgent {",
			"  uses Lifecycle",
			"  def runAgentTest : boolean {",
			"       killMe",
			"  }",
			"}");

	private static final String EXPECTED02 = multilineString(
			"package io.sarl.lang.tests.bug857;",
			"",
			"import io.sarl.core.Lifecycle;",
			"import io.sarl.lang.annotation.ImportedCapacityFeature;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AtomicSkillReference;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Extension;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeAgent extends Agent {",
			"  protected boolean runAgentTest() {",
			"    Lifecycle _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER();",
			"    _$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER.killMe();",
			"    return false;",
			"  }",
			"  ",
			"  @Extension",
			"  @ImportedCapacityFeature(Lifecycle.class)",
			"  @SyntheticMember",
			"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE;",
			"  ",
			"  @SyntheticMember",
			"  @Pure",
			"  private Lifecycle $CAPACITY_USE$IO_SARL_CORE_LIFECYCLE$CALLER() {",
			"    if (this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE == null || this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE.get() == null) {",
			"      this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE = $getSkill(Lifecycle.class);",
			"    }",
			"    return $castSkill(Lifecycle.class, this.$CAPACITY_USE$IO_SARL_CORE_LIFECYCLE);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public SomeAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug857.SomeAgent");
			assertEquals(EXPECTED02, actual);
		});
	}

}

