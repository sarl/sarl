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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Java problem: Invalid overriding with @Accessors.
 *
 * <p>https://github.com/sarl/sarl/issues/1073
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1073"
 */
@DisplayName("Bug #1073")
@SuppressWarnings("all")
@Tag("core")
public class Bug1073Test extends AbstractSarlTest {

	private final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1073",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"interface A {",
			"   def setCommandLineArguments(a : String*)",
			"}",
			"class B implements A {",
			"   @Accessors",
			"   var commandLineArguments : String[]",
			"}");
	
	private final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1073;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class B implements A {",
			"  @Accessors",
			"  private String[] commandLineArguments;",
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
			"  public B() {",
			"    super();",
			"  }",
			"  ",
			"  @Pure",
			"  public String[] getCommandLineArguments() {",
			"    return this.commandLineArguments;",
			"  }",
			"  ",
			"  public void setCommandLineArguments(final String... commandLineArguments) {",
			"    this.commandLineArguments = commandLineArguments;",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing varargs and inherited setter")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling varargs and inherited setter")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1073.B");
			assertEquals(JAVA_CODE_01, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1073.B");
			assertNotNull(type);
		});
	}

	private final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1073",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"interface A {",
			"   def setCommandLineArguments(a : String[])",
			"}",
			"class B implements A {",
			"   @Accessors",
			"   var commandLineArguments : String[]",
			"}");
	
	private final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1073;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class B implements A {",
			"  @Accessors",
			"  private String[] commandLineArguments;",
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
			"  public B() {",
			"    super();",
			"  }",
			"  ",
			"  @Pure",
			"  public String[] getCommandLineArguments() {",
			"    return this.commandLineArguments;",
			"  }",
			"  ",
			"  public void setCommandLineArguments(final String[] commandLineArguments) {",
			"    this.commandLineArguments = commandLineArguments;",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing array and inherited setter")
	@Tag("sarlParsing")
	public void parsing02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling array and inherited setter")
	@Tag("compileToJava")
	public void compiling02() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1073.B");
			assertEquals(JAVA_CODE_02, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1073.B");
			assertNotNull(type);
		});
	}

	private final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1073",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"interface A {",
			"   def setCommandLineArguments(a : String)",
			"}",
			"class B implements A {",
			"   @Accessors",
			"   var commandLineArguments : String[]",
			"   def setCommandLineArguments(a : String) {}",
			"}");
	
	private final String JAVA_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1073;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class B implements A {",
			"  @Accessors",
			"  private String[] commandLineArguments;",
			"  ",
			"  public void setCommandLineArguments(final String a) {",
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
			"  public B() {",
			"    super();",
			"  }",
			"  ",
			"  @Pure",
			"  public String[] getCommandLineArguments() {",
			"    return this.commandLineArguments;",
			"  }",
			"  ",
			"  public void setCommandLineArguments(final String[] commandLineArguments) {",
			"    this.commandLineArguments = commandLineArguments;",
			"  }",
			"}",
			"");

	@Test
	@DisplayName("Parsing string and inherited setter")
	@Tag("sarlParsing")
	public void parsing03() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling string and inherited setter")
	@Tag("compileToJava")
	public void compiling03() throws Exception {
		getCompileHelper().compile(SARL_CODE_03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1073.B");
			assertEquals(JAVA_CODE_03, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1073.B");
			assertNotNull(type);
		});
	}

}
