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
@DisplayName("Compilation: equals")
@Tag("core")
@Tag("compileToJava")
public class EqualsFunctionTest extends AbstractSarlTest {

	@Test
	public void noField_noFinalEquals_noFinalHashCode_noEquals_noHashCode() throws Exception {
		String source = multilineString(
				"class C1 {",
				"}"
				);
		final String expected = multilineString(
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
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void field_noFinalEquals_noFinalHashCode_noEquals_noHashCode() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  var field : int",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  private int field;",
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
				"    if (other.field != this.field)",
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
				"    result = prime * result + Integer.hashCode(this.field);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().assertCompilesTo(source, expected);
	}

	@Test
	public void noField_finalEquals_noFinalHashCode_noEquals_noHashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def equals(o : Object) { return super.equals(o) }",
				"}",
				"class C1 extends C0 {",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void field_finalEquals_noFinalHashCode_noEquals_noHashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def equals(o : Object) { return super.equals(o) }",
				"}",
				"class C1 extends C0 {",
				"  var field : int",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  private int field;",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void noField_noFinalEquals_finalHashCode_noEquals_noHashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def hashCode() : int { return super.hashCode() }",
				"}",
				"class C1 extends C0 {",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void field_noFinalEquals_finalHashCode_noEquals_noHashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def hashCode() : int { return super.hashCode() }",
				"}",
				"class C1 extends C0 {",
				"  var field : int",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  private int field;",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void noField_finalEquals_finalHashCode_noEquals_noHashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def equals(o : Object) { return super.equals(o) }",
				"  final def hashCode() : int { return super.hashCode() }",
				"}",
				"class C1 extends C0 {",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void field_finalEquals_finalHashCode_noEquals_noHashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def equals(o : Object) { return super.equals(o) }",
				"  final def hashCode() : int { return super.hashCode() }",
				"}",
				"class C1 extends C0 {",
				"  var field : int",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  private int field;",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	//-------------------------------- 1

	@Test
	public void noField_noFinalEquals_noFinalHashCode_equals_noHashCode() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def equals(o : Object) : boolean { super.equals(o) }",
				"}"
				);
		final String expected = multilineString(
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
				"  public boolean equals(final Object o) {",
				"    return super.equals(o);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void field_noFinalEquals_noFinalHashCode_equals_noHashCode() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  var field : int",
				"  def equals(o : Object) : boolean { super.equals(o) }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  private int field;",
				"  ",
				"  @Pure",
				"  public boolean equals(final Object o) {",
				"    return super.equals(o);",
				"  }",
				"  ",
				"  @Override",
				"  @Pure",
				"  @SyntheticMember",
				"  public int hashCode() {",
				"    int result = super.hashCode();",
				"    final int prime = 31;",
				"    result = prime * result + Integer.hashCode(this.field);",
				"    return result;",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void noField_noFinalEquals_finalHashCode_equals_noHashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def hashCode() : int { return super.hashCode() }",
				"}",
				"class C1 extends C0 {",
				"  def equals(o : Object) : boolean { super.equals(o) }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  @Pure",
				"  public boolean equals(final Object o) {",
				"    return super.equals(o);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void field_noFinalEquals_finalHashCode_equals_noHashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def hashCode() : int { return super.hashCode() }",
				"}",
				"class C1 extends C0 {",
				"  var field : int",
				"  def equals(o : Object) : boolean { super.equals(o) }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  private int field;",
				"  ",
				"  @Pure",
				"  public boolean equals(final Object o) {",
				"    return super.equals(o);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	//------------------------------ 2

	@Test
	public void noField_noFinalEquals_noFinalHashCode_noEquals_hashCode() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  def hashCode : int { return super.hashCode }",
				"}"
				);
		final String expected = multilineString(
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
				"  public int hashCode() {",
				"    return super.hashCode();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void field_noFinalEquals_noFinalHashCode_noEquals_hashCode() throws Exception {
		String source = multilineString(
				"class C1 {",
				"  var field : int",
				"  def hashCode : int { return super.hashCode }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 {",
				"  private int field;",
				"  ",
				"  @Pure",
				"  public int hashCode() {",
				"    return super.hashCode();",
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
				"    C1 other = (C1) obj;",
				"    if (other.field != this.field)",
				"      return false;",
				"    return super.equals(obj);",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void noField_finalEquals_noFinalHashCode_noEquals_hashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def equals(o : Object) { return super.equals(o) }",
				"}",
				"class C1 extends C0 {",
				"  def hashCode : int { return super.hashCode }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  @Pure",
				"  public int hashCode() {",
				"    return super.hashCode();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

	@Test
	public void field_finalEquals_noFinalHashCode_noEquals_hashCode() throws Exception {
		String source = multilineString(
				"class C0 {",
				"  final def equals(o : Object) { return super.equals(o) }",
				"}",
				"class C1 extends C0 {",
				"  var field : int",
				"  def hashCode : int { return super.hashCode }",
				"}"
				);
		final String expected = multilineString(
				"import io.sarl.lang.annotation.SarlElementType;",
				"import io.sarl.lang.annotation.SarlSpecification;",
				"import io.sarl.lang.annotation.SyntheticMember;",
				"import org.eclipse.xtext.xbase.lib.Pure;",
				"",
				"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
				"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
				"@SuppressWarnings(\"all\")",
				"public class C1 extends C0 {",
				"  private int field;",
				"  ",
				"  @Pure",
				"  public int hashCode() {",
				"    return super.hashCode();",
				"  }",
				"  ",
				"  @SyntheticMember",
				"  public C1() {",
				"    super();",
				"  }",
				"}",
				"");
		getCompileHelper().compile(source, (it) -> {
			assertEquals(expected, it.getGeneratedCode("C1"));
		});
	}

}
