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

package io.sarl.lang.core.tests.scoping.extensions.numbers.comparison;

import static io.sarl.lang.scoping.extensions.numbers.comparison.FloatComparisonExtensions.operator_equals;
import static io.sarl.lang.scoping.extensions.numbers.comparison.FloatComparisonExtensions.operator_greaterEqualsThan;
import static io.sarl.lang.scoping.extensions.numbers.comparison.FloatComparisonExtensions.operator_greaterThan;
import static io.sarl.lang.scoping.extensions.numbers.comparison.FloatComparisonExtensions.operator_lessEqualsThan;
import static io.sarl.lang.scoping.extensions.numbers.comparison.FloatComparisonExtensions.operator_lessThan;
import static io.sarl.lang.scoping.extensions.numbers.comparison.FloatComparisonExtensions.operator_notEquals;
import static io.sarl.lang.scoping.extensions.numbers.comparison.FloatComparisonExtensions.operator_spaceship;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
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
	FloatComparisonExtensionsTest.Code.class,
	FloatComparisonExtensionsTest.Compilation.class
})
@SuppressWarnings("all")
public class FloatComparisonExtensionsTest {

	/** This class tests the implementation of the functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Code extends AbstractSarlTest {

		private static Float left = 4f;

		private static Float left2 = 2f;

		private static int right = 3;

		private static int right2 = 5;

		@Test
		public void operator_greaterEqualsThan_Byte_Number() {
			assertTrue(operator_greaterEqualsThan(left, new AtomicDouble(right)));
			assertFalse(operator_greaterEqualsThan(left2, new AtomicDouble(right)));
			assertTrue(operator_greaterEqualsThan(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_lessEqualsThan_Byte_Number() {
			assertFalse(operator_lessEqualsThan(left, new AtomicDouble(right)));
			assertTrue(operator_lessEqualsThan(left2, new AtomicDouble(right)));
			assertTrue(operator_lessEqualsThan(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_greaterThan_Byte_Number() {
			assertTrue(operator_greaterThan(left, new AtomicDouble(right)));
			assertFalse(operator_greaterThan(left2, new AtomicDouble(right)));
			assertFalse(operator_greaterThan(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_lessThan_Byte_Number() {
			assertFalse(operator_lessThan(left, new AtomicDouble(right)));
			assertTrue(operator_lessThan(left2, new AtomicDouble(right)));
			assertFalse(operator_lessThan(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_equals_Byte_Number() {
			assertFalse(operator_equals(left, new AtomicDouble(right)));
			assertFalse(operator_equals(left2, new AtomicDouble(right)));
			assertTrue(operator_equals(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_notEquals_Byte_Number() {
			assertTrue(operator_notEquals(left, new AtomicDouble(right)));
			assertTrue(operator_notEquals(left2, new AtomicDouble(right)));
			assertFalse(operator_notEquals(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_spaceship_Byte_byte() {
			assertStrictlyPositive(operator_spaceship(left, (byte) right));
			assertStrictlyNegative(operator_spaceship(left, (byte) right2));
			assertZero(operator_spaceship(left, left.byteValue()));
		}

		@Test
		public void operator_spaceship_Byte_short() {
			assertStrictlyPositive(operator_spaceship(left, (short) right));
			assertStrictlyNegative(operator_spaceship(left, (short) right2));
			assertZero(operator_spaceship(left, (short) left.floatValue()));
		}

		@Test
		public void operator_spaceship_Byte_int() {
			assertStrictlyPositive(operator_spaceship(left, (int) right));
			assertStrictlyNegative(operator_spaceship(left, (int) right2));
			assertZero(operator_spaceship(left, (int) left.floatValue()));
		}

		@Test
		public void operator_spaceship_Byte_long() {
			assertStrictlyPositive(operator_spaceship(left, (long) right));
			assertStrictlyNegative(operator_spaceship(left, (long) right2));
			assertZero(operator_spaceship(left, (long) left.longValue()));
		}

		@Test
		public void operator_spaceship_Byte_float() {
			assertStrictlyPositive(operator_spaceship(left, (float) right));
			assertStrictlyNegative(operator_spaceship(left, (float) right2));
			assertZero(operator_spaceship(left, (float) left));
		}

		@Test
		public void operator_spaceship_Byte_double() {
			assertStrictlyPositive(operator_spaceship(left, (double) right));
			assertStrictlyNegative(operator_spaceship(left, (double) right2));
			assertZero(operator_spaceship(left, (double) left));
		}

		@Test
		public void operator_spaceship_Byte_Byte() {
			assertStrictlyPositive(operator_spaceship(left, Byte.valueOf((byte) right)));
			assertStrictlyNegative(operator_spaceship(left, Byte.valueOf((byte) right2)));
			assertZero(operator_spaceship(left, Byte.valueOf(left.byteValue())));
		}

		@Test
		public void operator_spaceship_Byte_Short() {
			assertStrictlyPositive(operator_spaceship(left, Short.valueOf((short) right)));
			assertStrictlyNegative(operator_spaceship(left, Short.valueOf((short) right2)));
			assertZero(operator_spaceship(left, Short.valueOf((short) left.floatValue())));
		}

		@Test
		public void operator_spaceship_Byte_Integer() {
			assertStrictlyPositive(operator_spaceship(left, Integer.valueOf(right)));
			assertStrictlyNegative(operator_spaceship(left, Integer.valueOf(right2)));
			assertZero(operator_spaceship(left, Integer.valueOf(left.intValue())));
		}

		@Test
		public void operator_spaceship_Byte_Long() {
			assertStrictlyPositive(operator_spaceship(left, Long.valueOf(right)));
			assertStrictlyNegative(operator_spaceship(left, Long.valueOf(right2)));
			assertZero(operator_spaceship(left, Long.valueOf(left.longValue())));
		}

		@Test
		public void operator_spaceship_Byte_Float() {
			assertStrictlyPositive(operator_spaceship(left, Float.valueOf(right)));
			assertStrictlyNegative(operator_spaceship(left, Float.valueOf(right2)));
			assertZero(operator_spaceship(left, Float.valueOf(left)));
		}

		@Test
		public void operator_spaceship_Byte_Double() {
			assertStrictlyPositive(operator_spaceship(left, Double.valueOf(right)));
			assertStrictlyNegative(operator_spaceship(left, Double.valueOf(right2)));
			assertZero(operator_spaceship(left, Double.valueOf(left)));
		}

		@Test
		public void operator_spaceship_Byte_AtomicInteger() {
			assertStrictlyPositive(operator_spaceship(left, new AtomicInteger(right)));
			assertStrictlyNegative(operator_spaceship(left, new AtomicInteger(right2)));
			assertZero(operator_spaceship(left, new AtomicInteger(left.intValue())));
		}

		@Test
		public void operator_spaceship_Byte_AtomicLong() {
			assertStrictlyPositive(operator_spaceship(left, new AtomicLong(right)));
			assertStrictlyNegative(operator_spaceship(left, new AtomicLong(right2)));
			assertZero(operator_spaceship(left, new AtomicLong(left.longValue())));
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
		public void operator_greaterThan_Byte_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : byte) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final byte right) {",
					"    return (left.floatValue() > right);",
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
		public void operator_greaterThan_Byte_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : short) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final short right) {",
					"    return (left.floatValue() > right);",
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
		public void operator_greaterThan_Byte_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : int) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final int right) {",
					"    return (left.floatValue() > right);",
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
		public void operator_greaterThan_Byte_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : long) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final long right) {",
					"    return (left.floatValue() > right);",
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
		public void operator_greaterThan_Byte_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : float) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final float right) {",
					"    return (left.floatValue() > right);",
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
		public void operator_greaterThan_Byte_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : double) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final double right) {",
					"    return (left.floatValue() > right);",
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
		public void operator_greaterThan_Byte_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Byte) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final Byte right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterThan_Byte_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Short) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final Short right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterThan_Byte_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Integer) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final Integer right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterThan_Byte_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Long) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final Long right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterThan_Byte_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Float) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final Float right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterThan_Byte_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Double) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final Double right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterThan_Byte_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Float, right : AtomicInteger) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final AtomicInteger right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterThan_Byte_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Float, right : AtomicLong) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final AtomicLong right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterThan_Byte_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Float, right : AtomicDouble) : boolean {",
					"    left > right",
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
					"  public boolean fct(final Float left, final AtomicDouble right) {",
					"    return (left.floatValue() > right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : byte) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final byte right) {",
					"    return (left.floatValue() >= right);",
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
		public void operator_greaterEqualsThan_Byte_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : short) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final short right) {",
					"    return (left.floatValue() >= right);",
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
		public void operator_greaterEqualsThan_Byte_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : int) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final int right) {",
					"    return (left.floatValue() >= right);",
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
		public void operator_greaterEqualsThan_Byte_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : long) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final long right) {",
					"    return (left.floatValue() >= right);",
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
		public void operator_greaterEqualsThan_Byte_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : float) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final float right) {",
					"    return (left.floatValue() >= right);",
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
		public void operator_greaterEqualsThan_Byte_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : double) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final double right) {",
					"    return (left.floatValue() >= right);",
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
		public void operator_greaterEqualsThan_Byte_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Byte) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final Byte right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Short) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final Short right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Integer) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final Integer right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Long) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final Long right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Float) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final Float right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Double) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final Double right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Float, right : AtomicInteger) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final AtomicInteger right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Float, right : AtomicLong) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final AtomicLong right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_Byte_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Float, right : AtomicDouble) : boolean {",
					"    left >= right",
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
					"  public boolean fct(final Float left, final AtomicDouble right) {",
					"    return (left.floatValue() >= right.doubleValue());",
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
		public void operator_lowerThan_Byte_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : byte) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final byte right) {",
					"    return (left.floatValue() < right);",
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
		public void operator_lowerThan_Byte_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : short) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final short right) {",
					"    return (left.floatValue() < right);",
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
		public void operator_lowerThan_Byte_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : int) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final int right) {",
					"    return (left.floatValue() < right);",
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
		public void operator_lowerThan_Byte_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : long) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final long right) {",
					"    return (left.floatValue() < right);",
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
		public void operator_lowerThan_Byte_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : float) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final float right) {",
					"    return (left.floatValue() < right);",
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
		public void operator_lowerThan_Byte_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : double) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final double right) {",
					"    return (left.floatValue() < right);",
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
		public void operator_lowerThan_Byte_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Byte) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final Byte right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerThan_Byte_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Short) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final Short right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerThan_Byte_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Integer) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final Integer right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerThan_Byte_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Long) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final Long right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerThan_Byte_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Float) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final Float right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerThan_Byte_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Double) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final Double right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerThan_Byte_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Float, right : AtomicInteger) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final AtomicInteger right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerThan_Byte_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Float, right : AtomicLong) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final AtomicLong right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerThan_Byte_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Float, right : AtomicDouble) : boolean {",
					"    left < right",
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
					"  public boolean fct(final Float left, final AtomicDouble right) {",
					"    return (left.floatValue() < right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : byte) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final byte right) {",
					"    return (left.floatValue() <= right);",
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
		public void operator_lowerEqualsThan_Byte_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : short) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final short right) {",
					"    return (left.floatValue() <= right);",
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
		public void operator_lowerEqualsThan_Byte_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : int) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final int right) {",
					"    return (left.floatValue() <= right);",
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
		public void operator_lowerEqualsThan_Byte_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : long) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final long right) {",
					"    return (left.floatValue() <= right);",
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
		public void operator_lowerEqualsThan_Byte_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : float) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final float right) {",
					"    return (left.floatValue() <= right);",
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
		public void operator_lowerEqualsThan_Byte_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : double) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final double right) {",
					"    return (left.floatValue() <= right);",
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
		public void operator_lowerEqualsThan_Byte_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Byte) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final Byte right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Short) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final Short right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Integer) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final Integer right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Long) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final Long right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Float) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final Float right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Double) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final Double right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Float, right : AtomicInteger) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final AtomicInteger right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Float, right : AtomicLong) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final AtomicLong right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_Byte_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Float, right : AtomicDouble) : boolean {",
					"    left <= right",
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
					"  public boolean fct(final Float left, final AtomicDouble right) {",
					"    return (left.floatValue() <= right.doubleValue());",
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
		public void operator_equals_Byte_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : byte) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final byte right) {",
					"    return (left != null && (left.floatValue() == right));",
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
		public void operator_equals_Byte_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : short) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final short right) {",
					"    return (left != null && (left.floatValue() == right));",
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
		public void operator_equals_Byte_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : int) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final int right) {",
					"    return (left != null && (left.floatValue() == right));",
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
		public void operator_equals_Byte_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : long) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final long right) {",
					"    return (left != null && (left.floatValue() == right));",
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
		public void operator_equals_Byte_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : float) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final float right) {",
					"    return (left != null && (left.floatValue() == right));",
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
		public void operator_equals_Byte_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : double) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final double right) {",
					"    return (left != null && (left.floatValue() == right));",
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
		public void operator_equals_Byte_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Byte) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final Byte right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_equals_Byte_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Short) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final Short right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_equals_Byte_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Integer) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final Integer right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_equals_Byte_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Long) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final Long right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_equals_Byte_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Float) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final Float right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_equals_Byte_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Double) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final Double right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_equals_Byte_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Float, right : AtomicInteger) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final AtomicInteger right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_equals_Byte_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Float, right : AtomicLong) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final AtomicLong right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_equals_Byte_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Float, right : AtomicDouble) : boolean {",
					"    left == right",
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
					"  public boolean fct(final Float left, final AtomicDouble right) {",
					"    return (left == null ? (right == null) : (right != null && left.floatValue() == right.doubleValue()));",
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
		public void operator_notEquals_Byte_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : byte) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final byte right) {",
					"    return (left == null || (left.floatValue() != right));",
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
		public void operator_notEquals_Byte_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : short) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final short right) {",
					"    return (left == null || (left.floatValue() != right));",
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
		public void operator_notEquals_Byte_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : int) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final int right) {",
					"    return (left == null || (left.floatValue() != right));",
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
		public void operator_notEquals_Byte_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : long) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final long right) {",
					"    return (left == null || (left.floatValue() != right));",
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
		public void operator_notEquals_Byte_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : float) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final float right) {",
					"    return (left == null || (left.floatValue() != right));",
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
		public void operator_notEquals_Byte_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : double) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final double right) {",
					"    return (left == null || (left.floatValue() != right));",
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
		public void operator_notEquals_Byte_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Byte) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final Byte right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_notEquals_Byte_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Short) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final Short right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_notEquals_Byte_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Integer) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final Integer right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_notEquals_Byte_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Long) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final Long right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_notEquals_Byte_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Float) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final Float right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_notEquals_Byte_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Double) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final Double right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_notEquals_Byte_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Float, right : AtomicInteger) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final AtomicInteger right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_notEquals_Byte_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Float, right : AtomicLong) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final AtomicLong right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_notEquals_Byte_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Float, right : AtomicDouble) : boolean {",
					"    left != right",
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
					"  public boolean fct(final Float left, final AtomicDouble right) {",
					"    return (left == null ? (right != null) : (right == null || left.floatValue() != right.doubleValue()));",
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
		public void operator_spaceship_Byte_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : byte) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final byte right) {",
					"    return Float.compare(left.floatValue(), right);",
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
		public void operator_spaceship_Byte_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : short) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final short right) {",
					"    return Float.compare(left.floatValue(), right);",
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
		public void operator_spaceship_Byte_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : int) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final int right) {",
					"    return Float.compare(left.floatValue(), right);",
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
		public void operator_spaceship_Byte_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : long) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final long right) {",
					"    return Float.compare(left.floatValue(), right);",
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
		public void operator_spaceship_Byte_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : float) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final float right) {",
					"    return Float.compare(left.floatValue(), right);",
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
		public void operator_spaceship_Byte_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : double) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final double right) {",
					"    return Double.compare(left.floatValue(), right);",
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
		public void operator_spaceship_Byte_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Byte) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final Byte right) {",
					"    return Float.compare(left.floatValue(), right.byteValue());",
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
		public void operator_spaceship_Byte_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Short) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final Short right) {",
					"    return Float.compare(left.floatValue(), right.shortValue());",
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
		public void operator_spaceship_Byte_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Integer) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final Integer right) {",
					"    return Float.compare(left.floatValue(), right.intValue());",
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
		public void operator_spaceship_Byte_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Long) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final Long right) {",
					"    return Float.compare(left.floatValue(), right.longValue());",
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
		public void operator_spaceship_Byte_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Float) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final Float right) {",
					"    return Float.compare(left.floatValue(), right.floatValue());",
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
		public void operator_spaceship_Byte_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Float, right : Double) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final Double right) {",
					"    return Double.compare(left.floatValue(), right.doubleValue());",
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
		public void operator_spaceship_Byte_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Float, right : AtomicInteger) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final AtomicInteger right) {",
					"    return Float.compare(left.floatValue(), right.intValue());",
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
		public void operator_spaceship_Byte_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Float, right : AtomicLong) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final AtomicLong right) {",
					"    return Float.compare(left.floatValue(), right.longValue());",
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
		public void operator_spaceship_Byte_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Float, right : AtomicDouble) : int {",
					"    left <=> right",
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
					"  public int fct(final Float left, final AtomicDouble right) {",
					"    return Double.compare(left.floatValue(), right.doubleValue());",
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
