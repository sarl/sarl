/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

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
@SuppressWarnings("all")
public class Bug505 extends AbstractSarlTest {

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
	public void snipset1() throws Exception {
		SarlScript mas = file(snippet1);
		validate(mas).assertNoErrors();
	}

	@Test
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
				"    if (!Objects.equals(this.testString, other.testString)) {",
				"      return false;",
				"    }",
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
	public void snipset2() throws Exception {
		SarlScript mas = file(snippet2);
		validate(mas).assertNoIssues();
	}

	@Test
	public void snipset2Compilation() throws Exception {
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class TestClass {",
				"  @Pure",
				"  public void testMethod1() {",
				"    this.testMethod2();",
				"  }",
				"  ",
				"  @Pure",
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
	public void snipset3() throws Exception {
		SarlScript mas = file(snippet3);
		validate(mas).assertNoIssues();
	}

	@Test
	public void snipset3Compilation() throws Exception {
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public abstract class TestClass {",
				"  @Pure",
				"  public void testMethod1() {",
				"    this.testMethod2();",
				"  }",
				"  ",
				"  @Pure",
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
	public void snipset4() throws Exception {
		SarlScript mas = file(snippet4);
		validate(mas).assertNoIssues();
	}

	@Test
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
				"    if (!Objects.equals(this.testString, other.testString)) {",
				"      return false;",
				"    }",
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
	public void snipset5() throws Exception {
		SarlScript mas = file(snippet5);
		validate(mas).assertWarning(
				XbasePackage.eINSTANCE.getXAssignment(),
				IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM);
	}

	@Test
	public void snipset5Compilation() throws Exception {
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import io.sarl.lang.core.Agent;",
				"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
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
				"    if (!Objects.equals(this.testString, other.testString)) {",
				"      return false;",
				"    }",
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
				"  @Deprecated",
				"  @Inject",
				"  public TestAgent(final BuiltinCapacitiesProvider arg0, final UUID arg1, final UUID arg2) {",
				"    super(arg0, arg1, arg2);",
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
