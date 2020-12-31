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
package io.sarl.lang.tests.general.compilation.general;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: @PrivateAPIAccess")
@Tag("core")
@Tag("compileToJava")
public class PrivateAPIAccessTest extends AbstractSarlTest {

	@Test
	public void privateFunctionPrivateCaller_01() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject",
				"",
				"class Accessor {",
				"  @PrivateAPI(isCallerOnly=true)",
				"  def doSomething(a : PrivateAPIObject) {",
				"    a.function",
				"  }",
				"}",
				"");
		final String expectedAccessor = multilineString(
				"import foo.PrivateAPIObject;",
				"import io.sarl.lang.annotation.PrivateAPI;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class Accessor {",
				"  @PrivateAPI(isCallerOnly = true)",
				"  public void doSomething(final PrivateAPIObject a) {",
				"    a.function();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public Accessor() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedAccessor, r.getGeneratedCode("Accessor"));
		});
	}

	@Test
	public void privateFunctionPrivateCaller_02() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject",
				"",
				"@PrivateAPI(isCallerOnly=true)",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject) {",
				"    a.function",
				"  }",
				"}",
				"");
		final String expectedAccessor = multilineString(
				"import foo.PrivateAPIObject;",
				"import io.sarl.lang.annotation.PrivateAPI;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@PrivateAPI(isCallerOnly = true)",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class Accessor {",
				"  public void doSomething(final PrivateAPIObject a) {",
				"    a.function();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public Accessor() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedAccessor, r.getGeneratedCode("Accessor"));
		});
	}

	@Test
	public void privateFunctionPrivateCaller_03() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"",
				"class Accessor {",
				"  @PrivateAPI(isCallerOnly=true)",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				"");
		final String expectedAccessor = multilineString(
				"import foo.PrivateAPIObject2;",
				"import io.sarl.lang.annotation.PrivateAPI;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class Accessor {",
				"  @PrivateAPI(isCallerOnly = true)",
				"  public void doSomething(final PrivateAPIObject2 a) {",
				"    a.function();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public Accessor() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedAccessor, r.getGeneratedCode("Accessor"));
		});
	}

	@Test
	public void privateFunctionPrivateCaller_04() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"",
				"@PrivateAPI(isCallerOnly=true)",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				"");
		final String expectedAccessor = multilineString(
				"import foo.PrivateAPIObject2;",
				"import io.sarl.lang.annotation.PrivateAPI;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@PrivateAPI(isCallerOnly = true)",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class Accessor {",
				"  public void doSomething(final PrivateAPIObject2 a) {",
				"    a.function();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public Accessor() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedAccessor, r.getGeneratedCode("Accessor"));
		});
	}

	@Test
	public void ambigousPrivateAPI_02() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"import static extension foo.PrivateAPIObject3.*",
				"",
				"@PrivateAPI",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				"");
		final String expectedAccessor = multilineString(
				"import foo.PrivateAPIObject2;",
				"import io.sarl.lang.annotation.PrivateAPI;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@PrivateAPI",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class Accessor {",
				"  public void doSomething(final PrivateAPIObject2 a) {",
				"    a.function();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public Accessor() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedAccessor, r.getGeneratedCode("Accessor"));
		});
	}

	@Test
	public void ambigousPrivateAPI_03() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import foo.PrivateAPIObject2",
				"import static extension foo.PrivateAPIObject3.*",
				"",
				"@PrivateAPI(isCallerOnly=true)",
				"class Accessor {",
				"  def doSomething(a : PrivateAPIObject2) {",
				"    a.function",
				"  }",
				"}",
				"");
		final String expectedAccessor = multilineString(
				"import foo.PrivateAPIObject2;",
				"import io.sarl.lang.annotation.PrivateAPI;",
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@PrivateAPI(isCallerOnly = true)",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class Accessor {",
				"  public void doSomething(final PrivateAPIObject2 a) {",
				"    a.function();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public Accessor() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedAccessor, r.getGeneratedCode("Accessor"));
		});
	}


	@Test
	public void ambigousPrivateAPI_04() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.annotation.PrivateAPI",
				"import io.sarl.core.AgentTask",
				"import io.sarl.core.Schedules",
				"",
				"agent Accessor {",
				"  uses Schedules",
				"  var t : AgentTask",
				"  def action : void {",
				"    this.t.name = \"hello\"",
				"  }",
				"}",
				"");
		final String expectedAccessor = multilineString(
				"import io.sarl.core.AgentTask;",
				"import io.sarl.core.Schedules;",
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
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public class Accessor extends Agent {",
				"  private AgentTask t;",
				"  ",
				"  protected void action() {",
				"    Schedules _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER = this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER();",
				"    _$CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER.setName(this.t, \"hello\");",
				"  }",
				"  ",
				"  @Extension",
				"  @ImportedCapacityFeature(Schedules.class)",
				"  @SyntheticMember",
				"  private transient AtomicSkillReference $CAPACITY_USE$IO_SARL_CORE_SCHEDULES;",
				"  ",
				"  @SyntheticMember",
				"  @Pure",
				"  private Schedules $CAPACITY_USE$IO_SARL_CORE_SCHEDULES$CALLER() {",
				"    if (this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES == null || this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES.get() == null) {",
				"      this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES = $getSkill(Schedules.class);",
				"    }",
				"    return $castSkill(Schedules.class, this.$CAPACITY_USE$IO_SARL_CORE_SCHEDULES);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public Accessor(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  @Inject",
				"  public Accessor(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
				"    super(arg0, arg1, arg2);",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (r) -> {
			assertEquals(expectedAccessor, r.getGeneratedCode("Accessor"));
		});
	}

}
