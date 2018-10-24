/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast;

import static io.sarl.lang.scoping.extensions.numbers.cast.PrimitiveDoubleCastExtensions.toAtomicDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.PrimitiveDoubleCastExtensions.toAtomicInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.PrimitiveDoubleCastExtensions.toAtomicLong;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
import com.google.inject.Inject;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@RunWith(Suite.class)
@SuiteClasses({
	PrimitiveDoubleCastExtensionsTest.Code.class,
	PrimitiveDoubleCastExtensionsTest.Compilation.class
})
@SuppressWarnings("all")
public class PrimitiveDoubleCastExtensionsTest {

	/** This class tests the implementation of the functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Code extends AbstractSarlTest {

		private static double left = 4;

		@Test
		public void toAtomicLong_double() {
			AtomicLong value = toAtomicLong(left);
			assertNotNull(value);
			assertEpsilonEquals(4., value.doubleValue());
		}

		@Test
		public void toAtomicInteger_double() {
			AtomicInteger value = toAtomicInteger(left);
			assertNotNull(value);
			assertEpsilonEquals(4., value.doubleValue());
		}

		@Test
		public void toAtomicDouble_double() {
			AtomicDouble value = toAtomicDouble(left);
			assertNotNull(value);
			assertEpsilonEquals(4., value.doubleValue());
		}

	}

	/** This class tests if the generated Java code corresponds to the inline definition of the functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Compilation extends AbstractSarlTest {

		@Test
		public void as_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : byte {",
					"    left as byte",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public byte fct(final double left) {",
					"    return ((byte) left);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : short {",
					"    left as short",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public short fct(final double left) {",
					"    return ((short) left);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : int {",
					"    left as int",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public int fct(final double left) {",
					"    return ((int) left);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : long {",
					"    left as long",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public long fct(final double left) {",
					"    return ((long) left);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : float {",
					"    left as float",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public float fct(final double left) {",
					"    return ((float) left);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : double {",
					"    left as double",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final double left) {",
					"    return ((double) left);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_Byte() throws Exception {
			// FIXME: The generated code is invalid => Java compilation error. This is an issue into Xbase. 
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : Byte {",
					"    left as Byte",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public Byte fct(final double left) {",
					"    return ((Byte) Double.valueOf(left));",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_Short() throws Exception {
			// FIXME: The generated code is invalid => Java compilation error. This is an issue into Xbase. 
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : Short {",
					"    left as Short",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public Short fct(final double left) {",
					"    return ((Short) Double.valueOf(left));",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_Integer() throws Exception {
			// FIXME: The generated code is invalid => Java compilation error. This is an issue into Xbase. 
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : Integer {",
					"    left as Integer",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public Integer fct(final double left) {",
					"    return ((Integer) Double.valueOf(left));",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_Long() throws Exception {
			// FIXME: The generated code is invalid => Java compilation error. This is an issue into Xbase. 
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : Long {",
					"    left as Long",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public Long fct(final double left) {",
					"    return ((Long) Double.valueOf(left));",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_Float() throws Exception {
			// FIXME: The generated code is invalid => Java compilation error. This is an issue into Xbase. 
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : Float {",
					"    left as Float",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public Float fct(final double left) {",
					"    return ((Float) Double.valueOf(left));",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : Double {",
					"    left as Double",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public Double fct(final double left) {",
					"    return ((Double) Double.valueOf(left));",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_AtomicInteger() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : double) : AtomicInteger {",
					"    left as AtomicInteger",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from double or Double to AtomicInteger");
		}

		@Test
		public void as_AtomicLong() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : double) : AtomicLong {",
					"    left as AtomicLong",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from double or Double to AtomicLong");
		}

		@Test
		public void as_AtomicDouble() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : double) : AtomicLong {",
					"    left as AtomicDouble",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from double or Double to AtomicDouble");
		}

		@Test
		public void as_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : double) : Number {",
					"    left as Number",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public Number fct(final double left) {",
					"    return ((Number) Double.valueOf(left));",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

	}

}
