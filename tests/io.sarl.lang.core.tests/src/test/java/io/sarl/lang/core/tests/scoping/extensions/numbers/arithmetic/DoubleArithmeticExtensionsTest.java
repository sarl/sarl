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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic;

import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_power;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_divide;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_minus;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_modulo;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_multiply;
import static io.sarl.lang.scoping.extensions.numbers.arithmetic.DoubleArithmeticExtensions.operator_plus;

import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
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
	DoubleArithmeticExtensionsTest.Code.class,
	DoubleArithmeticExtensionsTest.Compilation.class
})
@SuppressWarnings("all")
public class DoubleArithmeticExtensionsTest {

	/** This class tests the implementation of the functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Code extends AbstractSarlTest {

		private static Double left = Double.valueOf(4);

		private static int right = 3;

		@Test
		public void operator_minus_Double_long() throws Exception {
			assertEpsilonEquals(1, operator_minus(left, (long) right));
		}

		@Test
		public void operator_minus_Double_byte() throws Exception {
			assertEpsilonEquals(1, operator_minus(left, (byte)right));
		}

		@Test
		public void operator_minus_Double_float() throws Exception {
			assertEpsilonEquals(1f, operator_minus(left, (float) right));
		}

		@Test
		public void operator_minus_Double_int() throws Exception {
			assertEpsilonEquals(1, operator_minus(left, (int) right));
		}

		@Test
		public void operator_minus_Double_short() throws Exception {
			assertEpsilonEquals(1, operator_minus(left, (short) right));
		}

		@Test
		public void operator_plus_Double_long() throws Exception {
			assertEpsilonEquals(7l, operator_plus(left, (long) right));
		}

		@Test
		public void operator_plus_Double_byte() throws Exception {
			assertEpsilonEquals(7, operator_plus(left, (byte) right));
		}

		@Test
		public void operator_plus_Double_float() throws Exception {
			assertEpsilonEquals(7f, operator_plus(left, (float) right));
		}

		@Test
		public void operator_plus_Double_int() throws Exception {
			assertEpsilonEquals(7, operator_plus(left, (int) right));
		}

		@Test
		public void operator_plus_Double_short() throws Exception {
			assertEpsilonEquals(7, operator_plus(left, (short) right));
		}

		@Test
		public void operator_divide_Double_long() throws Exception {
			assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (long) right));
		}

		@Test
		public void operator_divide_Double_byte() throws Exception {
			assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (byte) right));
		}

		@Test
		public void operator_divide_Double_float() throws Exception {
			assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (float) right));
		}

		@Test
		public void operator_divide_Double_int() throws Exception {
			assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (int) right));
		}

		@Test
		public void operator_divide_Double_short() throws Exception {
			assertEpsilonEquals(1.333333333333333333333, operator_divide(left, (short) right));
		}

		@Test
		public void operator_multiply_Double_long() throws Exception {
			assertEpsilonEquals(12l, operator_multiply(left, (long) right));
		}

		@Test
		public void operator_multiply_Double_byte() throws Exception {
			assertEpsilonEquals(12, operator_multiply(left, (byte) right));
		}

		@Test
		public void operator_multiply_Double_float() throws Exception {
			assertEpsilonEquals(12f, operator_multiply(left, (float) right));
		}

		@Test
		public void operator_multiply_Double_int() throws Exception {
			assertEpsilonEquals(12, operator_multiply(left, (int) right));
		}

		@Test
		public void operator_multiply_Double_short() throws Exception {
			assertEpsilonEquals(12, operator_multiply(left, (short) right));
		}

		@Test
		public void operator_modulo_Double_long() throws Exception {
			assertEpsilonEquals(1l, operator_modulo(left, (long) right));
		}

		@Test
		public void operator_modulo_Double_byte() throws Exception {
			assertEpsilonEquals(1l, operator_modulo(left, (byte) right));
		}

		@Test
		public void operator_modulo_Double_float() throws Exception {
			assertEpsilonEquals(1f, operator_modulo(left, (float) right));
		}

		@Test
		public void operator_modulo_Double_int() throws Exception {
			assertEpsilonEquals(1, operator_modulo(left, (int) right));
		}

		@Test
		public void operator_modulo_Double_short() throws Exception {
			assertEpsilonEquals(1, operator_modulo(left, (short) right));
		}

		@Test
		public void operator_modulo_Double_Number() throws Exception {
			assertEpsilonEquals(1, operator_modulo(left, (Number) Double.valueOf(right)));
		}

		@Test
		public void operator_power_Double_byte() throws Exception {
			assertEpsilonEquals(64., operator_power(left, (byte) right));
		}

		@Test
		public void operator_power_Double_short() throws Exception {
			assertEpsilonEquals(64., operator_power(left, (short) right));
		}

		@Test
		public void operator_power_Double_int() throws Exception {
			assertEpsilonEquals(64., operator_power(left, (int) right));
		}

		@Test
		public void operator_power_Double_long() throws Exception {
			assertEpsilonEquals(64., operator_power(left, (long) right));
		}

		@Test
		public void operator_power_Double_float() throws Exception {
			assertEpsilonEquals(64., operator_power(left, (float) right));
		}

		@Test
		public void operator_power_Double_double() throws Exception {
			assertEpsilonEquals(64., operator_power(left, (double) right));
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
		public void operator_minus_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Double) : double {",
					"    -left",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left) {",
					"    return DoubleExtensions.operator_minus(left);",
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
		public void operator_minus_Double_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Double, right : AtomicDouble) : double {",
					"    left - right",
					"  }",
					"}"),
					multilineString(
					"import com.google.common.util.concurrent.AtomicDouble;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicDouble right) {",
					"    return DoubleExtensions.operator_minus(left, right);",
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
		public void operator_minus_Double_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : long) : double {",
					"    left - right",
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
					"  public double fct(final Double left, final long right) {",
					"    return (left.doubleValue() - right);",
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
		public void operator_minus_Double_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Long) : double {",
					"    left - right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Long right) {",
					"    return DoubleExtensions.operator_minus(left, right);",
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
		public void operator_minus_Double_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : byte) : double {",
					"    left - right",
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
					"  public double fct(final Double left, final byte right) {",
					"    return (left.doubleValue() - right);",
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
		public void operator_minus_Double_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Byte) : double {",
					"    left - right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Byte right) {",
					"    return DoubleExtensions.operator_minus(left, right);",
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
		public void operator_minus_Double_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : float) : double {",
					"    left - right",
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
					"  public double fct(final Double left, final float right) {",
					"    return (left.doubleValue() - right);",
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
		public void operator_minus_Double_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Float) : double {",
					"    left - right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Float right) {",
					"    return DoubleExtensions.operator_minus(left, right);",
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
		public void operator_minus_Double_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : int) : double {",
					"    left - right",
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
					"  public double fct(final Double left, final int right) {",
					"    return (left.doubleValue() - right);",
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
		public void operator_minus_Double_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Integer) : double {",
					"    left - right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Integer right) {",
					"    return DoubleExtensions.operator_minus(left, right);",
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
		public void operator_minus_Double_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : short) : double {",
					"    left - right",
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
					"  public double fct(final Double left, final short right) {",
					"    return (left.doubleValue() - right);",
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
		public void operator_minus_Double_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Short) : double {",
					"    left - right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Short right) {",
					"    return DoubleExtensions.operator_minus(left, right);",
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
		public void operator_minus_Double_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Double, right : AtomicInteger) : double {",
					"    left - right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicInteger;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicInteger right) {",
					"    return DoubleExtensions.operator_minus(left, right);",
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
		public void operator_minus_Double_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Double, right : AtomicLong) : double {",
					"    left - right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicLong;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicLong right) {",
					"    return DoubleExtensions.operator_minus(left, right);",
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
		public void operator_plus_Double_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Long) : double {",
					"    left + right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Long right) {",
					"    return DoubleExtensions.operator_plus(left, right);",
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
		public void operator_plus_Double_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : long) : double {",
					"    left + right",
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
					"  public double fct(final Double left, final long right) {",
					"    return (left.doubleValue() + right);",
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
		public void operator_plus_Double_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : byte) : double {",
					"    left + right",
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
					"  public double fct(final Double left, final byte right) {",
					"    return (left.doubleValue() + right);",
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
		public void operator_plus_Double_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Byte) : double {",
					"    left + right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Byte right) {",
					"    return DoubleExtensions.operator_plus(left, right);",
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
		public void operator_plus_Double_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Float) : double {",
					"    left + right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Float right) {",
					"    return DoubleExtensions.operator_plus(left, right);",
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
		public void operator_plus_Double_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : float) : double {",
					"    left + right",
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
					"  public double fct(final Double left, final float right) {",
					"    return (left.doubleValue() + right);",
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
		public void operator_plus_Double_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Integer) : double {",
					"    left + right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Integer right) {",
					"    return DoubleExtensions.operator_plus(left, right);",
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
		public void operator_plus_Double_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : int) : double {",
					"    left + right",
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
					"  public double fct(final Double left, final int right) {",
					"    return (left.doubleValue() + right);",
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
		public void operator_plus_Double_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : short) : double {",
					"    left + right",
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
					"  public double fct(final Double left, final short right) {",
					"    return (left.doubleValue() + right);",
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
		public void operator_plus_Double_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Short) : double {",
					"    left + right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Short right) {",
					"    return DoubleExtensions.operator_plus(left, right);",
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
		public void operator_plus_Double_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Double, right : AtomicInteger) : double {",
					"    left + right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicInteger;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicInteger right) {",
					"    return DoubleExtensions.operator_plus(left, right);",
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
		public void operator_plus_Double_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Long) : double {",
					"    left + right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Long right) {",
					"    return DoubleExtensions.operator_plus(left, right);",
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
		public void operator_plus_Double_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Double, right : AtomicDouble) : double {",
					"    left + right",
					"  }",
					"}"),
					multilineString(
					"import com.google.common.util.concurrent.AtomicDouble;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicDouble right) {",
					"    return DoubleExtensions.operator_plus(left, right);",
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
		public void operator_power_Double_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Double, right : AtomicDouble) : double {",
					"    left ** right",
					"  }",
					"}"),
					multilineString(
					"import com.google.common.util.concurrent.AtomicDouble;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicDouble right) {",
					"    return DoubleExtensions.operator_power(left, right);",
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
		public void operator_power_Double_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : byte) : double {",
					"    left ** right",
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
					"  public double fct(final Double left, final byte right) {",
					"    return Math.pow(left.doubleValue(), right);",
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
		public void operator_power_Double_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : short) : double {",
					"    left ** right",
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
					"  public double fct(final Double left, final short right) {",
					"    return Math.pow(left.doubleValue(), right);",
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
		public void operator_power_Double_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : int) : double {",
					"    left ** right",
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
					"  public double fct(final Double left, final int right) {",
					"    return Math.pow(left.doubleValue(), right);",
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
		public void operator_power_Double_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : long) : double {",
					"    left ** right",
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
					"  public double fct(final Double left, final long right) {",
					"    return Math.pow(left.doubleValue(), right);",
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
		public void operator_power_Double_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : float) : double {",
					"    left ** right",
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
					"  public double fct(final Double left, final float right) {",
					"    return Math.pow(left.doubleValue(), right);",
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
		public void operator_power_Double_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : double) : double {",
					"    left ** right",
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
					"  public double fct(final Double left, final double right) {",
					"    return Math.pow(left.doubleValue(), right);",
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
		public void operator_divide_Double_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : long) : double {",
					"    left / right",
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
					"  public double fct(final Double left, final long right) {",
					"    return (left.doubleValue() / right);",
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
		public void operator_divide_Double_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Long) : double {",
					"    left / right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Long right) {",
					"    return DoubleExtensions.operator_divide(left, right);",
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
		public void operator_divide_Double_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : byte) : double {",
					"    left / right",
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
					"  public double fct(final Double left, final byte right) {",
					"    return (left.doubleValue() / right);",
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
		public void operator_divide_Double_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Byte) : double {",
					"    left / right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Byte right) {",
					"    return DoubleExtensions.operator_divide(left, right);",
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
		public void operator_divide_Double_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : float) : double {",
					"    left / right",
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
					"  public double fct(final Double left, final float right) {",
					"    return (left.doubleValue() / right);",
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
		public void operator_divide_Double_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Float) : double {",
					"    left / right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Float right) {",
					"    return DoubleExtensions.operator_divide(left, right);",
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
		public void operator_divide_Double_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : int) : double {",
					"    left / right",
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
					"  public double fct(final Double left, final int right) {",
					"    return (left.doubleValue() / right);",
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
		public void operator_divide_Double_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Integer) : double {",
					"    left / right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Integer right) {",
					"    return DoubleExtensions.operator_divide(left, right);",
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
		public void operator_divide_Double_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Double, right : AtomicDouble) : double {",
					"    left / right",
					"  }",
					"}"),
					multilineString(
					"import com.google.common.util.concurrent.AtomicDouble;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicDouble right) {",
					"    return DoubleExtensions.operator_divide(left, right);",
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
		public void operator_divide_Double_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : short) : double {",
					"    left / right",
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
					"  public double fct(final Double left, final short right) {",
					"    return (left.doubleValue() / right);",
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
		public void operator_divide_Double_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Short) : double {",
					"    left / right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Short right) {",
					"    return DoubleExtensions.operator_divide(left, right);",
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
		public void operator_divide_Double_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Double, right : AtomicInteger) : double {",
					"    left / right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicInteger;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicInteger right) {",
					"    return DoubleExtensions.operator_divide(left, right);",
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
		public void operator_divide_Double_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Double, right : AtomicLong) : double {",
					"    left / right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicLong;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicLong right) {",
					"    return DoubleExtensions.operator_divide(left, right);",
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
		public void operator_multiply_Double_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : long) : double {",
					"    left * right",
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
					"  public double fct(final Double left, final long right) {",
					"    return (left.doubleValue() * right);",
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
		public void operator_multiply_Double_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Long) : double {",
					"    left * right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Long right) {",
					"    return DoubleExtensions.operator_multiply(left, right);",
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
		public void operator_multiply_Double_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : byte) : double {",
					"    left * right",
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
					"  public double fct(final Double left, final byte right) {",
					"    return (left.doubleValue() * right);",
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
		public void operator_multiply_Double_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Byte) : double {",
					"    left * right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Byte right) {",
					"    return DoubleExtensions.operator_multiply(left, right);",
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
		public void operator_multiply_Double_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : float) : double {",
					"    left * right",
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
					"  public double fct(final Double left, final float right) {",
					"    return (left.doubleValue() * right);",
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
		public void operator_multiply_Double_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Float) : double {",
					"    left * right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Float right) {",
					"    return DoubleExtensions.operator_multiply(left, right);",
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
		public void operator_multiply_Double_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : int) : double {",
					"    left * right",
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
					"  public double fct(final Double left, final int right) {",
					"    return (left.doubleValue() * right);",
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
		public void operator_multiply_Double_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Integer) : double {",
					"    left * right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Integer right) {",
					"    return DoubleExtensions.operator_multiply(left, right);",
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
		public void operator_multiply_Double_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Double, right : AtomicDouble) : double {",
					"    left * right",
					"  }",
					"}"),
					multilineString(
					"import com.google.common.util.concurrent.AtomicDouble;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicDouble right) {",
					"    return DoubleExtensions.operator_multiply(left, right);",
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
		public void operator_multiply_Double_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : short) : double {",
					"    left * right",
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
					"  public double fct(final Double left, final short right) {",
					"    return (left.doubleValue() * right);",
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
		public void operator_multiply_Double_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Short) : double {",
					"    left * right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final Short right) {",
					"    return DoubleExtensions.operator_multiply(left, right);",
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
		public void operator_multiply_Double_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Double, right : AtomicInteger) : double {",
					"    left * right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicInteger;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicInteger right) {",
					"    return DoubleExtensions.operator_multiply(left, right);",
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
		public void operator_multiply_Double_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Double, right : AtomicLong) : double {",
					"    left * right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicLong;",
					"import org.eclipse.xtext.xbase.lib.DoubleExtensions;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicLong right) {",
					"    return DoubleExtensions.operator_multiply(left, right);",
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
		public void operator_modulo_Double_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : long) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final long right) {",
					"    return (left.doubleValue() % right);",
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
		public void operator_modulo_Double_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Long) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final Long right) {",
					"    return (left.doubleValue() % right.doubleValue());",
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
		public void operator_modulo_Double_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Byte) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final Byte right) {",
					"    return (left.doubleValue() % right.doubleValue());",
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
		public void operator_modulo_Double_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : byte) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final byte right) {",
					"    return (left.doubleValue() % right);",
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
		public void operator_modulo_Double_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : float) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final float right) {",
					"    return (left.doubleValue() % right);",
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
		public void operator_modulo_Double_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Float) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final Float right) {",
					"    return (left.doubleValue() % right.doubleValue());",
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
		public void operator_modulo_Double_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : int) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final int right) {",
					"    return (left.doubleValue() % right);",
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
		public void operator_modulo_Double_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Integer) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final Integer right) {",
					"    return (left.doubleValue() % right.doubleValue());",
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
		public void operator_modulo_Double_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Double, right : AtomicDouble) : double {",
					"    left % right",
					"  }",
					"}"),
					multilineString(
					"import com.google.common.util.concurrent.AtomicDouble;",
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
					"  public double fct(final Double left, final AtomicDouble right) {",
					"    return (left.doubleValue() % right.doubleValue());",
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
		public void operator_modulo_Double_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : short) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final short right) {",
					"    return (left.doubleValue() % right);",
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
		public void operator_modulo_Double_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Double, right : Short) : double {",
					"    left % right",
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
					"  public double fct(final Double left, final Short right) {",
					"    return (left.doubleValue() % right.doubleValue());",
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
		public void operator_modulo_Double_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Double, right : AtomicInteger) : double {",
					"    left % right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicInteger;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicInteger right) {",
					"    return (left.doubleValue() % right.doubleValue());",
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
		public void operator_modulo_Double_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Double, right : AtomicLong) : double {",
					"    left % right",
					"  }",
					"}"),
					multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.concurrent.atomic.AtomicLong;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class A {",
					"  @Pure",
					"  public double fct(final Double left, final AtomicLong right) {",
					"    return (left.doubleValue() % right.doubleValue());",
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
