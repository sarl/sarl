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

import static io.sarl.lang.scoping.extensions.numbers.comparison.PrimitiveShortComparisonExtensions.operator_equals;
import static io.sarl.lang.scoping.extensions.numbers.comparison.PrimitiveShortComparisonExtensions.operator_greaterEqualsThan;
import static io.sarl.lang.scoping.extensions.numbers.comparison.PrimitiveShortComparisonExtensions.operator_greaterThan;
import static io.sarl.lang.scoping.extensions.numbers.comparison.PrimitiveShortComparisonExtensions.operator_lessEqualsThan;
import static io.sarl.lang.scoping.extensions.numbers.comparison.PrimitiveShortComparisonExtensions.operator_lessThan;
import static io.sarl.lang.scoping.extensions.numbers.comparison.PrimitiveShortComparisonExtensions.operator_notEquals;
import static io.sarl.lang.scoping.extensions.numbers.comparison.PrimitiveShortComparisonExtensions.operator_spaceship;

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
	PrimitiveShortComparisonExtensionsTest.Code.class,
	PrimitiveShortComparisonExtensionsTest.Compilation.class
})
@SuppressWarnings("all")
public class PrimitiveShortComparisonExtensionsTest {

	/** This class tests the implementation of the functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Code extends AbstractSarlTest {

		private static short left = 4;

		private static short left2 = 2;

		private static int right = 3;

		private static int right2 = 5;

		@Test
		public void operator_greaterEqualsThan_short_Number() {
			assertTrue(operator_greaterEqualsThan(left, new AtomicDouble(right)));
			assertFalse(operator_greaterEqualsThan(left2, new AtomicDouble(right)));
			assertTrue(operator_greaterEqualsThan(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_lessEqualsThan_short_Number() {
			assertFalse(operator_lessEqualsThan(left, new AtomicDouble(right)));
			assertTrue(operator_lessEqualsThan(left2, new AtomicDouble(right)));
			assertTrue(operator_lessEqualsThan(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_greaterThan_short_Number() {
			assertTrue(operator_greaterThan(left, new AtomicDouble(right)));
			assertFalse(operator_greaterThan(left2, new AtomicDouble(right)));
			assertFalse(operator_greaterThan(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_lessThan_short_Number() {
			assertFalse(operator_lessThan(left, new AtomicDouble(right)));
			assertTrue(operator_lessThan(left2, new AtomicDouble(right)));
			assertFalse(operator_lessThan(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_equals_short_Number() {
			assertFalse(operator_equals(left, new AtomicDouble(right)));
			assertFalse(operator_equals(left2, new AtomicDouble(right)));
			assertTrue(operator_equals(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_notEquals_short_Number() {
			assertTrue(operator_notEquals(left, new AtomicDouble(right)));
			assertTrue(operator_notEquals(left2, new AtomicDouble(right)));
			assertFalse(operator_notEquals(left, new AtomicDouble(left)));
		}

		@Test
		public void operator_spaceship_short_byte() {
			assertStrictlyPositive(operator_spaceship(left, (byte) right));
			assertStrictlyNegative(operator_spaceship(left, (byte) right2));
			assertZero(operator_spaceship(left, (byte) left));
		}

		@Test
		public void operator_spaceship_short_short() {
			assertStrictlyPositive(operator_spaceship(left, (short) right));
			assertStrictlyNegative(operator_spaceship(left, (short) right2));
			assertZero(operator_spaceship(left, (short) left));
		}

		@Test
		public void operator_spaceship_short_int() {
			assertStrictlyPositive(operator_spaceship(left, (int) right));
			assertStrictlyNegative(operator_spaceship(left, (int) right2));
			assertZero(operator_spaceship(left, (int) left));
		}

		@Test
		public void operator_spaceship_short_long() {
			assertStrictlyPositive(operator_spaceship(left, (long) right));
			assertStrictlyNegative(operator_spaceship(left, (long) right2));
			assertZero(operator_spaceship(left, (long) left));
		}

		@Test
		public void operator_spaceship_short_float() {
			assertStrictlyPositive(operator_spaceship(left, (float) right));
			assertStrictlyNegative(operator_spaceship(left, (float) right2));
			assertZero(operator_spaceship(left, (float) left));
		}

		@Test
		public void operator_spaceship_short_double() {
			assertStrictlyPositive(operator_spaceship(left, (double) right));
			assertStrictlyNegative(operator_spaceship(left, (double) right2));
			assertZero(operator_spaceship(left, (double) left));
		}

		@Test
		public void operator_spaceship_short_Byte() {
			assertStrictlyPositive(operator_spaceship(left, Byte.valueOf((byte) right)));
			assertStrictlyNegative(operator_spaceship(left, Byte.valueOf((byte) right2)));
			assertZero(operator_spaceship(left, Byte.valueOf((byte) left)));
		}

		@Test
		public void operator_spaceship_short_Short() {
			assertStrictlyPositive(operator_spaceship(left, Short.valueOf((short) right)));
			assertStrictlyNegative(operator_spaceship(left, Short.valueOf((short) right2)));
			assertZero(operator_spaceship(left, Short.valueOf((short) left)));
		}

		@Test
		public void operator_spaceship_short_Integer() {
			assertStrictlyPositive(operator_spaceship(left, Integer.valueOf(right)));
			assertStrictlyNegative(operator_spaceship(left, Integer.valueOf(right2)));
			assertZero(operator_spaceship(left, Integer.valueOf(left)));
		}

		@Test
		public void operator_spaceship_short_Long() {
			assertStrictlyPositive(operator_spaceship(left, Long.valueOf(right)));
			assertStrictlyNegative(operator_spaceship(left, Long.valueOf(right2)));
			assertZero(operator_spaceship(left, Long.valueOf(left)));
		}

		@Test
		public void operator_spaceship_short_Float() {
			assertStrictlyPositive(operator_spaceship(left, Float.valueOf(right)));
			assertStrictlyNegative(operator_spaceship(left, Float.valueOf(right2)));
			assertZero(operator_spaceship(left, Float.valueOf(left)));
		}

		@Test
		public void operator_spaceship_short_Double() {
			assertStrictlyPositive(operator_spaceship(left, Double.valueOf(right)));
			assertStrictlyNegative(operator_spaceship(left, Double.valueOf(right2)));
			assertZero(operator_spaceship(left, Double.valueOf(left)));
		}

		@Test
		public void operator_spaceship_short_AtomicInteger() {
			assertStrictlyPositive(operator_spaceship(left, new AtomicInteger(right)));
			assertStrictlyNegative(operator_spaceship(left, new AtomicInteger(right2)));
			assertZero(operator_spaceship(left, new AtomicInteger(left)));
		}

		@Test
		public void operator_spaceship_short_AtomicLong() {
			assertStrictlyPositive(operator_spaceship(left, new AtomicLong(right)));
			assertStrictlyNegative(operator_spaceship(left, new AtomicLong(right2)));
			assertZero(operator_spaceship(left, new AtomicLong(left)));
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
		public void operator_greaterThan_short_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : byte) : boolean {",
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
					"  public boolean fct(final short left, final byte right) {",
					"    return (left > right);",
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
		public void operator_greaterThan_short_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : short) : boolean {",
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
					"  public boolean fct(final short left, final short right) {",
					"    return (left > right);",
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
		public void operator_greaterThan_short_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : int) : boolean {",
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
					"  public boolean fct(final short left, final int right) {",
					"    return (left > right);",
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
		public void operator_greaterThan_short_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : long) : boolean {",
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
					"  public boolean fct(final short left, final long right) {",
					"    return (left > right);",
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
		public void operator_greaterThan_short_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : float) : boolean {",
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
					"  public boolean fct(final short left, final float right) {",
					"    return (left > right);",
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
		public void operator_greaterThan_short_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : double) : boolean {",
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
					"  public boolean fct(final short left, final double right) {",
					"    return (left > right);",
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
		public void operator_greaterThan_short_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Byte) : boolean {",
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
					"  public boolean fct(final short left, final Byte right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterThan_short_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Short) : boolean {",
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
					"  public boolean fct(final short left, final Short right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterThan_short_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Integer) : boolean {",
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
					"  public boolean fct(final short left, final Integer right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterThan_short_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Long) : boolean {",
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
					"  public boolean fct(final short left, final Long right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterThan_short_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Float) : boolean {",
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
					"  public boolean fct(final short left, final Float right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterThan_short_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Double) : boolean {",
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
					"  public boolean fct(final short left, final Double right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterThan_short_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : short, right : AtomicInteger) : boolean {",
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
					"  public boolean fct(final short left, final AtomicInteger right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterThan_short_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : short, right : AtomicLong) : boolean {",
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
					"  public boolean fct(final short left, final AtomicLong right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterThan_short_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : short, right : AtomicDouble) : boolean {",
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
					"  public boolean fct(final short left, final AtomicDouble right) {",
					"    return (left > right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : byte) : boolean {",
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
					"  public boolean fct(final short left, final byte right) {",
					"    return (left >= right);",
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
		public void operator_greaterEqualsThan_short_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : short) : boolean {",
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
					"  public boolean fct(final short left, final short right) {",
					"    return (left >= right);",
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
		public void operator_greaterEqualsThan_short_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : int) : boolean {",
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
					"  public boolean fct(final short left, final int right) {",
					"    return (left >= right);",
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
		public void operator_greaterEqualsThan_short_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : long) : boolean {",
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
					"  public boolean fct(final short left, final long right) {",
					"    return (left >= right);",
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
		public void operator_greaterEqualsThan_short_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : float) : boolean {",
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
					"  public boolean fct(final short left, final float right) {",
					"    return (left >= right);",
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
		public void operator_greaterEqualsThan_short_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : double) : boolean {",
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
					"  public boolean fct(final short left, final double right) {",
					"    return (left >= right);",
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
		public void operator_greaterEqualsThan_short_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Byte) : boolean {",
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
					"  public boolean fct(final short left, final Byte right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Short) : boolean {",
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
					"  public boolean fct(final short left, final Short right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Integer) : boolean {",
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
					"  public boolean fct(final short left, final Integer right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Long) : boolean {",
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
					"  public boolean fct(final short left, final Long right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Float) : boolean {",
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
					"  public boolean fct(final short left, final Float right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Double) : boolean {",
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
					"  public boolean fct(final short left, final Double right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : short, right : AtomicInteger) : boolean {",
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
					"  public boolean fct(final short left, final AtomicInteger right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : short, right : AtomicLong) : boolean {",
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
					"  public boolean fct(final short left, final AtomicLong right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_greaterEqualsThan_short_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : short, right : AtomicDouble) : boolean {",
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
					"  public boolean fct(final short left, final AtomicDouble right) {",
					"    return (left >= right.doubleValue());",
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
		public void operator_lowerThan_short_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : byte) : boolean {",
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
					"  public boolean fct(final short left, final byte right) {",
					"    return (left < right);",
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
		public void operator_lowerThan_short_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : short) : boolean {",
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
					"  public boolean fct(final short left, final short right) {",
					"    return (left < right);",
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
		public void operator_lowerThan_short_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : int) : boolean {",
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
					"  public boolean fct(final short left, final int right) {",
					"    return (left < right);",
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
		public void operator_lowerThan_short_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : long) : boolean {",
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
					"  public boolean fct(final short left, final long right) {",
					"    return (left < right);",
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
		public void operator_lowerThan_short_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : float) : boolean {",
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
					"  public boolean fct(final short left, final float right) {",
					"    return (left < right);",
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
		public void operator_lowerThan_short_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : double) : boolean {",
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
					"  public boolean fct(final short left, final double right) {",
					"    return (left < right);",
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
		public void operator_lowerThan_short_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Byte) : boolean {",
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
					"  public boolean fct(final short left, final Byte right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerThan_short_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Short) : boolean {",
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
					"  public boolean fct(final short left, final Short right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerThan_short_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Integer) : boolean {",
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
					"  public boolean fct(final short left, final Integer right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerThan_short_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Long) : boolean {",
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
					"  public boolean fct(final short left, final Long right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerThan_short_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Float) : boolean {",
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
					"  public boolean fct(final short left, final Float right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerThan_short_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Double) : boolean {",
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
					"  public boolean fct(final short left, final Double right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerThan_short_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : short, right : AtomicInteger) : boolean {",
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
					"  public boolean fct(final short left, final AtomicInteger right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerThan_short_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : short, right : AtomicLong) : boolean {",
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
					"  public boolean fct(final short left, final AtomicLong right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerThan_short_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : short, right : AtomicDouble) : boolean {",
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
					"  public boolean fct(final short left, final AtomicDouble right) {",
					"    return (left < right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : byte) : boolean {",
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
					"  public boolean fct(final short left, final byte right) {",
					"    return (left <= right);",
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
		public void operator_lowerEqualsThan_short_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : short) : boolean {",
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
					"  public boolean fct(final short left, final short right) {",
					"    return (left <= right);",
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
		public void operator_lowerEqualsThan_short_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : int) : boolean {",
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
					"  public boolean fct(final short left, final int right) {",
					"    return (left <= right);",
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
		public void operator_lowerEqualsThan_short_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : long) : boolean {",
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
					"  public boolean fct(final short left, final long right) {",
					"    return (left <= right);",
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
		public void operator_lowerEqualsThan_short_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : float) : boolean {",
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
					"  public boolean fct(final short left, final float right) {",
					"    return (left <= right);",
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
		public void operator_lowerEqualsThan_short_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : double) : boolean {",
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
					"  public boolean fct(final short left, final double right) {",
					"    return (left <= right);",
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
		public void operator_lowerEqualsThan_short_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Byte) : boolean {",
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
					"  public boolean fct(final short left, final Byte right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Short) : boolean {",
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
					"  public boolean fct(final short left, final Short right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Integer) : boolean {",
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
					"  public boolean fct(final short left, final Integer right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Long) : boolean {",
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
					"  public boolean fct(final short left, final Long right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Float) : boolean {",
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
					"  public boolean fct(final short left, final Float right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Double) : boolean {",
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
					"  public boolean fct(final short left, final Double right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : short, right : AtomicInteger) : boolean {",
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
					"  public boolean fct(final short left, final AtomicInteger right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : short, right : AtomicLong) : boolean {",
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
					"  public boolean fct(final short left, final AtomicLong right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_lowerEqualsThan_short_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : short, right : AtomicDouble) : boolean {",
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
					"  public boolean fct(final short left, final AtomicDouble right) {",
					"    return (left <= right.doubleValue());",
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
		public void operator_equals_short_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : byte) : boolean {",
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
					"  public boolean fct(final short left, final byte right) {",
					"    return (left == right);",
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
		public void operator_equals_short_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : short) : boolean {",
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
					"  public boolean fct(final short left, final short right) {",
					"    return (left == right);",
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
		public void operator_equals_short_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : int) : boolean {",
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
					"  public boolean fct(final short left, final int right) {",
					"    return (left == right);",
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
		public void operator_equals_short_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : long) : boolean {",
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
					"  public boolean fct(final short left, final long right) {",
					"    return (left == right);",
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
		public void operator_equals_short_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : float) : boolean {",
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
					"  public boolean fct(final short left, final float right) {",
					"    return (left == right);",
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
		public void operator_equals_short_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : double) : boolean {",
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
					"  public boolean fct(final short left, final double right) {",
					"    return (left == right);",
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
		public void operator_equals_short_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Byte) : boolean {",
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
					"  public boolean fct(final short left, final Byte right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_equals_short_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Short) : boolean {",
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
					"  public boolean fct(final short left, final Short right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_equals_short_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Integer) : boolean {",
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
					"  public boolean fct(final short left, final Integer right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_equals_short_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Long) : boolean {",
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
					"  public boolean fct(final short left, final Long right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_equals_short_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Float) : boolean {",
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
					"  public boolean fct(final short left, final Float right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_equals_short_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Double) : boolean {",
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
					"  public boolean fct(final short left, final Double right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_equals_short_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : short, right : AtomicInteger) : boolean {",
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
					"  public boolean fct(final short left, final AtomicInteger right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_equals_short_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : short, right : AtomicLong) : boolean {",
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
					"  public boolean fct(final short left, final AtomicLong right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_equals_short_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : short, right : AtomicDouble) : boolean {",
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
					"  public boolean fct(final short left, final AtomicDouble right) {",
					"    return (right != null && left == right.doubleValue());",
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
		public void operator_notEquals_short_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : byte) : boolean {",
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
					"  public boolean fct(final short left, final byte right) {",
					"    return (left != right);",
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
		public void operator_notEquals_short_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : short) : boolean {",
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
					"  public boolean fct(final short left, final short right) {",
					"    return (left != right);",
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
		public void operator_notEquals_short_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : int) : boolean {",
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
					"  public boolean fct(final short left, final int right) {",
					"    return (left != right);",
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
		public void operator_notEquals_short_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : long) : boolean {",
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
					"  public boolean fct(final short left, final long right) {",
					"    return (left != right);",
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
		public void operator_notEquals_short_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : float) : boolean {",
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
					"  public boolean fct(final short left, final float right) {",
					"    return (left != right);",
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
		public void operator_notEquals_short_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : double) : boolean {",
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
					"  public boolean fct(final short left, final double right) {",
					"    return (left != right);",
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
		public void operator_notEquals_short_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Byte) : boolean {",
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
					"  public boolean fct(final short left, final Byte right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_notEquals_short_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Short) : boolean {",
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
					"  public boolean fct(final short left, final Short right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_notEquals_short_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Integer) : boolean {",
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
					"  public boolean fct(final short left, final Integer right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_notEquals_short_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Long) : boolean {",
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
					"  public boolean fct(final short left, final Long right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_notEquals_short_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Float) : boolean {",
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
					"  public boolean fct(final short left, final Float right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_notEquals_short_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Double) : boolean {",
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
					"  public boolean fct(final short left, final Double right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_notEquals_short_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : short, right : AtomicInteger) : boolean {",
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
					"  public boolean fct(final short left, final AtomicInteger right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_notEquals_short_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : short, right : AtomicLong) : boolean {",
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
					"  public boolean fct(final short left, final AtomicLong right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_notEquals_short_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : short, right : AtomicDouble) : boolean {",
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
					"  public boolean fct(final short left, final AtomicDouble right) {",
					"    return (right == null || left != right.doubleValue());",
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
		public void operator_spaceship_short_byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : byte) : int {",
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
					"  public int fct(final short left, final byte right) {",
					"    return Short.compare(left, right);",
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
		public void operator_spaceship_short_short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : short) : int {",
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
					"  public int fct(final short left, final short right) {",
					"    return Short.compare(left, right);",
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
		public void operator_spaceship_short_int() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : int) : int {",
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
					"  public int fct(final short left, final int right) {",
					"    return Integer.compare(left, right);",
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
		public void operator_spaceship_short_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : long) : int {",
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
					"  public int fct(final short left, final long right) {",
					"    return Long.compare(left, right);",
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
		public void operator_spaceship_short_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : float) : int {",
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
					"  public int fct(final short left, final float right) {",
					"    return Float.compare(left, right);",
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
		public void operator_spaceship_short_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : double) : int {",
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
					"  public int fct(final short left, final double right) {",
					"    return Double.compare(left, right);",
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
		public void operator_spaceship_short_Byte() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Byte) : int {",
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
					"  public int fct(final short left, final Byte right) {",
					"    return Short.compare(left, right.byteValue());",
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
		public void operator_spaceship_short_Short() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Short) : int {",
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
					"  public int fct(final short left, final Short right) {",
					"    return Short.compare(left, right.shortValue());",
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
		public void operator_spaceship_short_Integer() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Integer) : int {",
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
					"  public int fct(final short left, final Integer right) {",
					"    return Integer.compare(left, right.intValue());",
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
		public void operator_spaceship_short_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Long) : int {",
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
					"  public int fct(final short left, final Long right) {",
					"    return Long.compare(left, right.longValue());",
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
		public void operator_spaceship_short_Float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Float) : int {",
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
					"  public int fct(final short left, final Float right) {",
					"    return Float.compare(left, right.floatValue());",
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
		public void operator_spaceship_short_Double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : short, right : Double) : int {",
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
					"  public int fct(final short left, final Double right) {",
					"    return Double.compare(left, right.doubleValue());",
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
		public void operator_spaceship_short_AtomicInteger() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : short, right : AtomicInteger) : int {",
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
					"  public int fct(final short left, final AtomicInteger right) {",
					"    return Integer.compare(left, right.intValue());",
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
		public void operator_spaceship_short_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : short, right : AtomicLong) : int {",
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
					"  public int fct(final short left, final AtomicLong right) {",
					"    return Long.compare(left, right.longValue());",
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
		public void operator_spaceship_short_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : short, right : AtomicDouble) : int {",
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
					"  public int fct(final short left, final AtomicDouble right) {",
					"    return Double.compare(left, right.doubleValue());",
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
