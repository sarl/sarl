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
package io.sarl.lang.tests.general.compilation.oop;

import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper.Result;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
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
@DisplayName("Compilation: class")
@Tag("core")
@Tag("compileToJava")
public class ClassCompilerTest {

	@Nested
	public class TopLevelTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "class C1 { }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "class C1 { var v = 45 }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  private int v = 45;",
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
					"    C1 other = (C1) obj;",
					"    if (other.v != this.v)",
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
					"    result = prime * result + Integer.hashCode(this.v);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "class C1 { val v = 45 }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  private final int v = 45;",
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
					"    C1 other = (C1) obj;",
					"    if (other.v != this.v)",
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
					"    result = prime * result + Integer.hashCode(this.v);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "class C1 { def fct { 4 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Pure",
					"  public int fct() {",
					"    return 4;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "class C1 { def fct(a : int) { a } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Pure",
					"  public int fct(final int a) {",
					"    return a;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "class C1 { def fct(a : int*) { 5 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Pure",
					"  public int fct(final int... a) {",
					"    return 5;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "class C1 { def fct(a : int = 6) { 5 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @DefaultValueSource",
					"  @Pure",
					"  public int fct(@DefaultValue(\"C1#FCT_0\") final int a) {",
					"    return 5;",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter a",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"6\")",
					"  private final int $DEFAULT_VALUE$FCT_0() {",
					"    return 6;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  @Pure",
					"  public final int fct() {",
					"    return fct($DEFAULT_VALUE$FCT_0());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void methodOverriding_explicitReturnType() throws Exception {
			String source = multilineString(
					"package io.sarl.docs.reference.oop",
					"class Person {",
					"	var firstName : String",
					"	var lastName : String",
					"	def getFullName : String {",
					"		this.firstName + \" \" + this.lastName",
					"	}",
					"}",
					"class PersonEx extends Person {",
					"	var title : String",
					"	override getFullName : String {",
					"		return title + \" \" + super.fullName",
					"	}",
					"}");
			final String expectedPerson = multilineString(
					"package io.sarl.docs.reference.oop;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.Objects;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Person {",
					"  private String firstName;",
					"  ",
					"  private String lastName;",
					"  ",
					"  @Pure",
					"  public String getFullName() {",
					"    return ((this.firstName + \" \") + this.lastName);",
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
					"    Person other = (Person) obj;",
					"    if (!Objects.equals(this.firstName, other.firstName))",
					"      return false;",
					"    if (!Objects.equals(this.lastName, other.lastName))",
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
					"    result = prime * result + Objects.hashCode(this.firstName);",
					"    result = prime * result + Objects.hashCode(this.lastName);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Person() {",
					"    super();",
					"  }",
					"}",
					"");
			final String expectedPersonEx = multilineString(
					"package io.sarl.docs.reference.oop;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.Objects;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class PersonEx extends Person {",
					"  private String title;",
					"  ",
					"  @Override",
					"  @Pure",
					"  public String getFullName() {",
					"    String _fullName = super.getFullName();",
					"    return ((this.title + \" \") + _fullName);",
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
					"    PersonEx other = (PersonEx) obj;",
					"    if (!Objects.equals(this.title, other.title))",
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
					"    result = prime * result + Objects.hashCode(this.title);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public PersonEx() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, (r) -> {
					assertEquals(expectedPerson, r.getGeneratedCode("io.sarl.docs.reference.oop.Person"));
					assertEquals(expectedPersonEx, r.getGeneratedCode("io.sarl.docs.reference.oop.PersonEx"));
				});
		}
		
		public void methodOverriding_inferredReturnType() throws Exception {
			String source = multilineString(
					"package io.sarl.docs.reference.oop",
					"class Person {",
					"	var firstName : String",
					"	var lastName : String",
					"	def getFullName : String {",
					"		this.firstName + \" \" + this.lastName",
					"	}",
					"}",
					"class PersonEx extends Person {",
					"	var title : String",
					"	override getFullName {",
					"		return title + \" \" + super.fullName",
					"	}",
					"}");
			final String expectedPerson = multilineString(
					"package io.sarl.docs.reference.oop;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class Person {",
					"  private String firstName;",
					"  ",
					"  private String lastName;",
					"  ",
					"  public String getFullName() {",
					"    return ((this.firstName + \" \") + this.lastName);",
					"  }",
					"}",
					"");
			final String expectedPersonEx = multilineString(
					"package io.sarl.docs.reference.oop;",
					"",
					"import io.sarl.docs.reference.oop.Person;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class PersonEx extends Person {",
					"  private String title;",
					"  ",
					"  @Override",
					"  public String getFullName() {",
					"    String _fullName = super.getFullName();",
					"    return ((this.title + \" \") + _fullName);",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, new IAcceptor<Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedPerson, r.getGeneratedCode("io.sarl.docs.reference.oop.Person"));
					assertEquals(expectedPersonEx, r.getGeneratedCode("io.sarl.docs.reference.oop.PersonEx"));
				}
			});
		}
		
	}

	@Nested
	public class InClassTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "class Container { class C1 { } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  public class C1 {",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "class Container { static class C1 { var v = 45 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  public static class C1 {",
					"    private int v = 45;",
					"    ",
					"    @Override",
					"    @Pure",
					"    @SyntheticMember",
					"    public boolean equals(final Object obj) {",
					"      if (this == obj)",
					"        return true;",
					"      if (obj == null)",
					"        return false;",
					"      if (getClass() != obj.getClass())",
					"        return false;",
					"      C1 other = (C1) obj;",
					"      if (other.v != this.v)",
					"        return false;",
					"      return super.equals(obj);",
					"    }",
					"    ",
					"    @Override",
					"    @Pure",
					"    @SyntheticMember",
					"    public int hashCode() {",
					"      int result = super.hashCode();",
					"      final int prime = 31;",
					"      result = prime * result + Integer.hashCode(this.v);",
					"      return result;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "class Container { static class C1 { val v = 45 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  public static class C1 {",
					"    private final int v = 45;",
					"    ",
					"    @Override",
					"    @Pure",
					"    @SyntheticMember",
					"    public boolean equals(final Object obj) {",
					"      if (this == obj)",
					"        return true;",
					"      if (obj == null)",
					"        return false;",
					"      if (getClass() != obj.getClass())",
					"        return false;",
					"      C1 other = (C1) obj;",
					"      if (other.v != this.v)",
					"        return false;",
					"      return super.equals(obj);",
					"    }",
					"    ",
					"    @Override",
					"    @Pure",
					"    @SyntheticMember",
					"    public int hashCode() {",
					"      int result = super.hashCode();",
					"      final int prime = 31;",
					"      result = prime * result + Integer.hashCode(this.v);",
					"      return result;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "class Container { class C1 { def fct { 4 } } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  public class C1 {",
					"    @Pure",
					"    public int fct() {",
					"      return 4;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "class Container { class C1 { def fct(a : int) { a } } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  public class C1 {",
					"    @Pure",
					"    public int fct(final int a) {",
					"      return a;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "class Container { class C1 { def fct(a : int*) { 5 } } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  public class C1 {",
					"    @Pure",
					"    public int fct(final int... a) {",
					"      return 5;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "class Container { class C1 { def fct(a : int = 6) { 5 } } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  public class C1 {",
					"    @DefaultValueSource",
					"    @Pure",
					"    public int fct(@DefaultValue(\"Container$C1#FCT_0\") final int a) {",
					"      return 5;",
					"    }",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @Pure",
					"    @SyntheticMember",
					"    @SarlSourceCode(\"6\")",
					"    private final int $DEFAULT_VALUE$FCT_0() {",
					"      return 6;",
					"    }",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @SyntheticMember",
					"    @Pure",
					"    public final int fct() {",
					"      return fct($DEFAULT_VALUE$FCT_0());",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class InAgentTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "agent Container { static class C1 { } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  protected static class C1 {",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void variable() throws Exception {
			String source = "agent Container { static class C1 { var v = 45 } }";
			String expected = multilineString(
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  protected static class C1 {",
					"    private int v = 45;",
					"    ",
					"    @Override",
					"    @Pure",
					"    @SyntheticMember",
					"    public boolean equals(final Object obj) {",
					"      if (this == obj)",
					"        return true;",
					"      if (obj == null)",
					"        return false;",
					"      if (getClass() != obj.getClass())",
					"        return false;",
					"      C1 other = (C1) obj;",
					"      if (other.v != this.v)",
					"        return false;",
					"      return super.equals(obj);",
					"    }",
					"    ",
					"    @Override",
					"    @Pure",
					"    @SyntheticMember",
					"    public int hashCode() {",
					"      int result = super.hashCode();",
					"      final int prime = 31;",
					"      result = prime * result + Integer.hashCode(this.v);",
					"      return result;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void value() throws Exception {
			String source = "agent Container { static class C1 { val v = 45 } }";
			String expected = multilineString(
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  protected static class C1 {",
					"    private final int v = 45;",
					"    ",
					"    @Override",
					"    @Pure",
					"    @SyntheticMember",
					"    public boolean equals(final Object obj) {",
					"      if (this == obj)",
					"        return true;",
					"      if (obj == null)",
					"        return false;",
					"      if (getClass() != obj.getClass())",
					"        return false;",
					"      C1 other = (C1) obj;",
					"      if (other.v != this.v)",
					"        return false;",
					"      return super.equals(obj);",
					"    }",
					"    ",
					"    @Override",
					"    @Pure",
					"    @SyntheticMember",
					"    public int hashCode() {",
					"      int result = super.hashCode();",
					"      final int prime = 31;",
					"      result = prime * result + Integer.hashCode(this.v);",
					"      return result;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_0() throws Exception {
			String source = "agent Container { class C1 { def fct { 4 } } }";
			String expected = multilineString(
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  protected class C1 {",
					"    @Pure",
					"    public int fct() {",
					"      return 4;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  @Inject",
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
					"    super(arg0, arg1, arg2);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_1() throws Exception {
			String source = "agent Container { class C1 { def fct(a : int) { a } } }";
			String expected = multilineString(
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  protected class C1 {",
					"    @Pure",
					"    public int fct(final int a) {",
					"      return a;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  @Inject",
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
					"    super(arg0, arg1, arg2);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_2() throws Exception {
			String source = "agent Container { class C1 { def fct(a : int*) { 5 } } }";
			String expected = multilineString(
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  protected class C1 {",
					"    @Pure",
					"    public int fct(final int... a) {",
					"      return 5;",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  @Inject",
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
					"    super(arg0, arg1, arg2);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void method_3() throws Exception {
			String source = "agent Container { class C1 { def fct(a : int = 6) { 5 } } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
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
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"  protected class C1 {",
					"    @DefaultValueSource",
					"    @Pure",
					"    public int fct(@DefaultValue(\"Container$C1#FCT_0\") final int a) {",
					"      return 5;",
					"    }",
					"    ",
					"    /**",
					"     * Default value for the parameter a",
					"     */",
					"    @Pure",
					"    @SyntheticMember",
					"    @SarlSourceCode(\"6\")",
					"    private final int $DEFAULT_VALUE$FCT_0() {",
					"      return 6;",
					"    }",
					"    ",
					"    @DefaultValueUse(\"int\")",
					"    @SyntheticMember",
					"    @Pure",
					"    public final int fct() {",
					"      return fct($DEFAULT_VALUE$FCT_0());",
					"    }",
					"    ",
					"    @SyntheticMember",
					"    public C1() {",
					"      super();",
					"    }",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  @Inject",
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
					"    super(arg0, arg1, arg2);",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class GenericTest extends AbstractSarlTest {

		@Test
		public void classGeneric_X() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X> {",
					"	var x : X",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1<X extends Object> {",
					"  private X x;",
					"  ",
					"  public void setX(final X param) {",
					"    this.x = param;",
					"  }",
					"  ",
					"  @Pure",
					"  public X getX() {",
					"    return this.x;",
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
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void classGeneric_XextendsNumber() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X extends Number> {",
					"	var x : X",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1<X extends Number> {",
					"  private X x;",
					"  ",
					"  public void setX(final X param) {",
					"    this.x = param;",
					"  }",
					"  ",
					"  @Pure",
					"  public X getX() {",
					"    return this.x;",
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
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void classGeneric_XY() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X,Y> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1<X extends Object, Y extends Object> {",
					"  private X x;",
					"  ",
					"  @Pure",
					"  public Y getY() {",
					"    return null;",
					"  }",
					"  ",
					"  public void setX(final X param) {",
					"    this.x = param;",
					"  }",
					"  ",
					"  @Pure",
					"  public X getX() {",
					"    return this.x;",
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
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void classGeneric_XYextendsX() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1<X,Y extends X> {",
					"	var x : X",
					"	def getY : Y { null }",
					"	def setX(param : X) { this.x = param }",
					"	def getX : X { this.x }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1<X extends Object, Y extends X> {",
					"  private X x;",
					"  ",
					"  @Pure",
					"  public Y getY() {",
					"    return null;",
					"  }",
					"  ",
					"  public void setX(final X param) {",
					"    this.x = param;",
					"  }",
					"  ",
					"  @Pure",
					"  public X getX() {",
					"    return this.x;",
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
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_X_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X { var xxx : X }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object> void setX(final X param) {",
					"    X xxx = null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_X_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X> setX(param : X) : void { var xxx : X }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object> void setX(final X param) {",
					"    X xxx = null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XextendsNumber_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X extends Number { var xxx : X }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Number> void setX(final X param) {",
					"    X xxx = null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XextendsNumber_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X extends Number> setX(param : X) : void { var xxx : X }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Number> void setX(final X param) {",
					"    X xxx = null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XY_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X, Y { var xxx : X; var yyy : Y }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object, Y extends Object> void setX(final X param) {",
					"    X xxx = null;",
					"    Y yyy = null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XY_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X, Y> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object, Y extends Object> void setX(final X param) {",
					"    X xxx = null;",
					"    Y yyy = null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XYextendsX_sarlNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def setX(param : X) : void with X, Y extends X { var xxx : X; var yyy : Y }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object, Y extends X> void setX(final X param) {",
					"    X xxx = null;",
					"    Y yyy = null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void functionGeneric_XYextendsX_javaNotation() throws Exception {
			String source = multilineString(
					"package io.sarl.lang.tests.test",
					"class C1 {",
					"	def <X, Y extends X> setX(param : X) : void { var xxx : X; var yyy : Y }",
					"}");
			String expected = multilineString(
					"package io.sarl.lang.tests.test;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public <X extends Object, Y extends X> void setX(final X param) {",
					"    X xxx = null;",
					"    Y yyy = null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

}
