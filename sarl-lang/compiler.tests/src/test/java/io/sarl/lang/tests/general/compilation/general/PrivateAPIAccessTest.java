/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;


/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@SuppressWarnings("all")
@DisplayName("Compilation: @PrivateAPIAccess")
@Tag("core")
@Tag("compileToJava")
public class PrivateAPIAccessTest extends AbstractSarlTest {

	@Test
	public void privateFunctionPrivateCaller_01() throws Exception {
		String source = multilineString(
				"import io.sarl.lang.core.annotation.PrivateAPI",
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
				"import io.sarl.lang.core.annotation.PrivateAPI;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
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
				"import io.sarl.lang.core.annotation.PrivateAPI",
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
				"import io.sarl.lang.core.annotation.PrivateAPI;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@PrivateAPI(isCallerOnly = true)",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
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
				"import io.sarl.lang.core.annotation.PrivateAPI",
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
				"import io.sarl.lang.core.annotation.PrivateAPI;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
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
				"import io.sarl.lang.core.annotation.PrivateAPI",
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
				"import io.sarl.lang.core.annotation.PrivateAPI;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@PrivateAPI(isCallerOnly = true)",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
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
				"import io.sarl.lang.core.annotation.PrivateAPI",
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
				"import io.sarl.lang.core.annotation.PrivateAPI;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@PrivateAPI",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
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
				"import io.sarl.lang.core.annotation.PrivateAPI",
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
				"import io.sarl.lang.core.annotation.PrivateAPI;",
				"import io.sarl.lang.core.annotation.SarlElementType;",
				"import io.sarl.lang.core.annotation.SarlSpecification;",
				"import io.sarl.lang.core.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
				"",
				"@PrivateAPI(isCallerOnly = true)",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@XbaseGenerated",
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

}
