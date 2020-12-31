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
package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/** Text for issue 505: Reference instance methods or fields in abstract class.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/505"
 */
@DisplayName("Bug #505")
@SuppressWarnings("all")
@Tag("core")
public class Bug505Test extends AbstractSarlTest {

	private String snippet1 = multilineString(
			"abstract class TestClass {",
			"\tprivate var testString : String;",
			"\tpublic new(testString : String)",
			"\t{",
			"\t\tthis.testString = testString;",
			"\t}",
			"\tpublic def getTestString() : String",
			"\t{",
			"\t\treturn this.testString;",
			"\t}",
			"}");

	private String snippet2 = multilineString(
			"abstract class TestClass {",
			"\tdef testMethod1 {",
			"\t\tthis.testMethod2()",
			"\t}",
			"\tdef testMethod2 { }",
			"}");

	private String snippet3 = multilineString(
			"abstract class TestClass {",
			"\tdef testMethod1 {",
			"\t\ttestMethod2()",
			"\t}",
			"\tdef testMethod2 { }",
			"}");

	private String snippet4 = multilineString(
			"class TestClass {",
			"\tvar testString : String;",
			"\tnew(testString : String)",
			"\t{",
			"\t\tthis.testString = testString;",
			"\t}",
			"\tdef getTestString() : String",
			"\t{",
			"\t\treturn this.testString;",
			"\t}",
			"}");

	private String snippet5 = multilineString(
			"abstract agent TestAgent {",
			"\tprivate var testString : String;",
			"\tdef myfct(testString : String)",
			"\t{",
			"\t\tthis.testString = testString;",
			"\t}",
			"\tdef getTestString() : String",
			"\t{",
			"\t\treturn this.testString;",
			"\t}",
			"}");

	@Test
	@Tag("sarlValidation")
	public void snipset1() throws Exception {
		SarlScript mas = file(getParseHelper(), snippet1);
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void snipset1Compilation() throws Exception {
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import java.util.Objects;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class TestClass {",
				"  private String testString;",
				"  ",
				"  public TestClass(final String testString) {",
				"    this.testString = testString;",
				"  }",
				"  ",
				"  @Pure",
				"  public String getTestString() {",
				"    return this.testString;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    TestClass other = (TestClass) obj;",
				"    if (!Objects.equals(this.testString, other.testString))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Objects.hashCode(this.testString);",
				"    return result;",
				"  }",
				"}",
				"");
		getCompileHelper().compile(snippet1, (r) -> assertEquals(expected, r.getGeneratedCode("TestClass")));
	}

	@Test
	@Tag("sarlValidation")
	public void snipset2() throws Exception {
		SarlScript mas = file(getParseHelper(), snippet2);
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void snipset2Compilation() throws Exception {
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class TestClass {",
				"  public void testMethod1() {",
				"    this.testMethod2();",
				"  }",
				"  ",
				"  public void testMethod2() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public TestClass() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(snippet2, (r) -> assertEquals(expected, r.getGeneratedCode("TestClass")));
	}

	@Test
	@Tag("sarlValidation")
	public void snipset3() throws Exception {
		SarlScript mas = file(getParseHelper(), snippet3);
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void snipset3Compilation() throws Exception {
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class TestClass {",
				"  public void testMethod1() {",
				"    this.testMethod2();",
				"  }",
				"  ",
				"  public void testMethod2() {",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public TestClass() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(snippet3, (r) -> assertEquals(expected, r.getGeneratedCode("TestClass")));
	}

	@Test
	@Tag("sarlValidation")
	public void snipset4() throws Exception {
		SarlScript mas = file(getParseHelper(), snippet4);
		validate(getValidationHelper(), getInjector(), mas).assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void snipset4Compilation() throws Exception {
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import java.util.Objects;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class TestClass {",
				"  private String testString;",
				"  ",
				"  public TestClass(final String testString) {",
				"    this.testString = testString;",
				"  }",
				"  ",
				"  @Pure",
				"  public String getTestString() {",
				"    return this.testString;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    TestClass other = (TestClass) obj;",
				"    if (!Objects.equals(this.testString, other.testString))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Objects.hashCode(this.testString);",
				"    return result;",
				"  }",
				"}",
				"");
		getCompileHelper().compile(snippet4, (r) -> assertEquals(expected, r.getGeneratedCode("TestClass")));
	}

	@Test
	@Tag("sarlValidation")
	public void snipset5() throws Exception {
		SarlScript mas = file(getParseHelper(), snippet5);
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXAssignment(),
				IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
	}

	@Test
	@Tag("compileToJava")
	public void snipset5Compilation() throws Exception {
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.DynamicSkillProvider;",
				"import java.util.Objects;",
				"import java.util.UUID;",
				"import javax.inject.Inject;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class TestAgent extends Agent {",
				"  private String testString;",
				"  ",
				"  protected void myfct(final String testString) {",
				"    this.testString = testString;",
				"  }",
				"  ",
				"  @Pure",
				"  protected String getTestString() {",
				"    return this.testString;",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public boolean equals(final Object obj) {",
				"    if (this == obj)",
				"      return true;",
				"    if (obj == null)",
				"      return false;",
				"    if (getClass() != obj.getClass())",
				"      return false;",
				"    TestAgent other = (TestAgent) obj;",
				"    if (!Objects.equals(this.testString, other.testString))",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Objects.hashCode(this.testString);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public TestAgent(final UUID arg0, final UUID arg1) {",
				"    super(arg0, arg1);",
				"  }",
				"  ", 
				"  @SyntheticMember", 
				"  @Inject", 
				"  public TestAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
				"    super(arg0, arg1, arg2);", 
				"  }",
				"}",
				"");
		getCompileHelper().compile(snippet5, (r) -> assertEquals(expected, r.getGeneratedCode("TestAgent")));
	}

}
