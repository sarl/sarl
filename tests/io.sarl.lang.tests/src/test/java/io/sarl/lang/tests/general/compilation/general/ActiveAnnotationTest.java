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
@DisplayName("Compilation: active annotations")
@Tag("core")
@Tag("compileToJava")
public class ActiveAnnotationTest {
	
	@Nested
	@DisplayName("Compilation: @Accessors")
	public class AccessorsTest extends AbstractSarlTest {

		@Test
		public void inClassField_01() throws Exception {
			String source = multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"class C1 {",
					"	@Accessors var field : double = 0",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtend.lib.annotations.Accessors;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Accessors",
					"  private double field = 0;",
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
					"    if (Double.doubleToLongBits(other.field) != Double.doubleToLongBits(this.field))",
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
					"    result = prime * result + Double.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"  ",
					"  @Pure",
					"  public double getField() {",
					"    return this.field;",
					"  }",
					"  ",
					"  public void setField(final double field) {",
					"    this.field = field;",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void inClassField_02() throws Exception {
			String source = multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"class C1 {",
					"	@Accessors(PROTECTED_SETTER) var field : double = 0",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtend.lib.annotations.AccessorType;",
					"import org.eclipse.xtend.lib.annotations.Accessors;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Accessors(AccessorType.PROTECTED_SETTER)",
					"  private double field = 0;",
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
					"    if (Double.doubleToLongBits(other.field) != Double.doubleToLongBits(this.field))",
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
					"    result = prime * result + Double.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"  ",
					"  protected void setField(final double field) {",
					"    this.field = field;",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void inClass_01() throws Exception {
			String source = multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"@Accessors class C1 {",
					"	var field : double = 0",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtend.lib.annotations.Accessors;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@Accessors",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  private double field = 0;",
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
					"    if (Double.doubleToLongBits(other.field) != Double.doubleToLongBits(this.field))",
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
					"    result = prime * result + Double.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"  ",
					"  @Pure",
					"  public double getField() {",
					"    return this.field;",
					"  }",
					"  ",
					"  public void setField(final double field) {",
					"    this.field = field;",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

		@Test
		public void inClass_02() throws Exception {
			String source = multilineString(
					"import org.eclipse.xtend.lib.annotations.Accessors",
					"import org.eclipse.xtend.lib.annotations.AccessorType",
					"@Accessors(PROTECTED_SETTER) class C1 {",
					"	var field : double = 0",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtend.lib.annotations.AccessorType;",
					"import org.eclipse.xtend.lib.annotations.Accessors;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@Accessors(AccessorType.PROTECTED_SETTER)",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  private double field = 0;",
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
					"    if (Double.doubleToLongBits(other.field) != Double.doubleToLongBits(this.field))",
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
					"    result = prime * result + Double.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"  ",
					"  protected void setField(final double field) {",
					"    this.field = field;",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	@DisplayName("Compilation: @Data")
	public class DataTest extends AbstractSarlTest {

		@Test
		public void inClass_01() throws Exception {
			String source = multilineString(
					"import org.eclipse.xtend.lib.annotations.Data",
					"@Data class C1 {",
					"	val field : double",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtend.lib.annotations.Data;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@Data",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  private final double field;",
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
					"    if (Double.doubleToLongBits(other.field) != Double.doubleToLongBits(this.field))",
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
					"    result = prime * result + Double.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  public C1(final double field) {",
					"    super();",
					"    this.field = field;",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  public String toString() {",
					"    ToStringBuilder b = new ToStringBuilder(this);",
					"    b.add(\"field\", this.field);",
					"    return b.toString();",
					"  }",
					"  ",
					"  @Pure",
					"  public double getField() {",
					"    return this.field;",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	@DisplayName("Compilation: @Delegate")
	public class DelegateTest extends AbstractSarlTest {

		@Test
		public void inClass_01() throws Exception {
			String source = multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"interface I1 {",
					"  def myFct",
					"}",
					"class C1 implements I1 {",
					"	@Delegate var field : I1",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtend.lib.annotations.Delegate;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 implements I1 {",
					"  @Delegate",
					"  private I1 field;",
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
					"  ",
					"  public void myFct() {",
					"    this.field.myFct();",
					"  }",
					"}",
					""
					);
			getCompileHelper().compile(source, (it) -> {
				assertEquals(expected, it.getGeneratedCode("C1"));
			});
		}


		@Test
		public void inClass_02() throws Exception {
			String source = multilineString(
					"import org.eclipse.xtend.lib.annotations.Delegate",
					"interface I1 {",
					"  def compareTo(a : String) : int",
					"}",
					"class MyDelegate implements I1 {",
					"   def compareTo(a : String) : int { 0 }",
					"}",
					"class C1 implements I1 {",
					"	@Delegate def provideDelegate(methodName : String, paramTypes : Class<?>[], actualArguments : Object[]) : I1 {",
					"      return new MyDelegate",
					"   }",
					"}"
					);
				String expected = multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtend.lib.annotations.Delegate;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class C1 implements I1 {",
						"  @Delegate",
						"  @Pure",
						"  public I1 provideDelegate(final String methodName, final Class<?>[] paramTypes, final Object[] actualArguments) {",
						"    return new MyDelegate();",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public C1() {",
						"    super();",
						"  }",
						"  ",
						"  public int compareTo(final String a) {",
						"    return this.provideDelegate(\"compareTo\", new Class[]{String.class}, new Object[]{a}).compareTo(a);",
						"  }",
						"}",
						""
						);
				getCompileHelper().compile(source, (it) -> {
					assertEquals(expected, it.getGeneratedCode("C1"));
				});
		}

	}

	@Nested
	@DisplayName("Compilation: @ToString")
	public class ToStringTest extends AbstractSarlTest {

		@Test
		public void inClass_01() throws Exception {
			String source = multilineString(
					"import org.eclipse.xtend.lib.annotations.ToString",
					"@ToString class C1 {",
					"	var field : double = 0",
					"}"
					);
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtend.lib.annotations.ToString;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;",
					"",
					"@ToString",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  private double field = 0;",
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
					"    if (Double.doubleToLongBits(other.field) != Double.doubleToLongBits(this.field))",
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
					"    result = prime * result + Double.hashCode(this.field);",
					"    return result;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  public String toString() {",
					"    ToStringBuilder b = new ToStringBuilder(this);",
					"    b.add(\"field\", this.field);",
					"    return b.toString();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

}
