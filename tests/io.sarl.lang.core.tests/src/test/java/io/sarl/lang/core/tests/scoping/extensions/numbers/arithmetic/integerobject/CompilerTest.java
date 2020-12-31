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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.integerobject;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.globalcompilation.GlobalCompilationSuite;
import io.sarl.tests.api.globalcompilation.GlobalCompilationTestContribution;
import io.sarl.tests.api.globalcompilation.ResourceSetGlobalCompilationContext;

/**
 * <p>FIXME: Enable when the speed issue of implicitly imported functions is solved.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
@DisplayName("Compiling Integer operators")
@GlobalCompilationSuite
@Disabled("not yet added to the compiler")
@Tag("core")
@Tag("compileToJava")
public class CompilerTest extends AbstractSarlTest {

	@GlobalCompilationTestContribution
	public void operator_minus_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Integer) : int {",
				"    -left",
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
						"  public int fct(final Integer left) {",
						"    return (-(left.intValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Integer, right : AtomicDouble) : double {",
				"    left - right",
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
						"  public double fct(final Integer left, final AtomicDouble right) {",
						"    return (left.intValue() - right.doubleValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : long) : long {",
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
						"  public long fct(final Integer left, final long right) {",
						"    return (left.intValue() - right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Long) : long {",
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
						"  public long fct(final Integer left, final Long right) {",
						"    return (left.intValue() - right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : byte) : int {",
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
						"  public int fct(final Integer left, final byte right) {",
						"    return (left.intValue() - right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Byte) : int {",
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
						"  public int fct(final Integer left, final Byte right) {",
						"    return (left.intValue() - right.byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : float) : float {",
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
						"  public float fct(final Integer left, final float right) {",
						"    return (left.intValue() - right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Float) : float {",
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
						"  public float fct(final Integer left, final Float right) {",
						"    return (left.intValue() - right.floatValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : int) : int {",
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
						"  public int fct(final Integer left, final int right) {",
						"    return (left.intValue() - right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Integer) : int {",
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
						"  public int fct(final Integer left, final Integer right) {",
						"    return (left.intValue() - right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : short) : int {",
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
						"  public int fct(final Integer left, final short right) {",
						"    return (left.intValue() - right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Short) : int {",
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
						"  public int fct(final Integer left, final Short right) {",
						"    return (left.intValue() - right.shortValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Integer, right : AtomicInteger) : int {",
				"    left - right",
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
						"  public int fct(final Integer left, final AtomicInteger right) {",
						"    return (left.intValue() - right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_minus_Integer_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Integer, right : AtomicLong) : long {",
				"    left - right",
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
						"  public long fct(final Integer left, final AtomicLong right) {",
						"    return (left.intValue() - right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Long) : long {",
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
						"  public long fct(final Integer left, final Long right) {",
						"    return (left.intValue() + right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : long) : long {",
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
						"  public long fct(final Integer left, final long right) {",
						"    return (left.intValue() + right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : byte) : int {",
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
						"  public int fct(final Integer left, final byte right) {",
						"    return (left.intValue() + right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Byte) : int {",
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
						"  public int fct(final Integer left, final Byte right) {",
						"    return (left.intValue() + right.byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Float) : float {",
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
						"  public float fct(final Integer left, final Float right) {",
						"    return (left.intValue() + right.floatValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : float) : float {",
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
						"  public float fct(final Integer left, final float right) {",
						"    return (left.intValue() + right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Integer) : int {",
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
						"  public int fct(final Integer left, final Integer right) {",
						"    return (left.intValue() + right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : int) : int {",
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
						"  public int fct(final Integer left, final int right) {",
						"    return (left.intValue() + right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : short) : int {",
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
						"  public int fct(final Integer left, final short right) {",
						"    return (left.intValue() + right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Short) : int {",
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
						"  public int fct(final Integer left, final Short right) {",
						"    return (left.intValue() + right.shortValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Integer, right : AtomicInteger) : int {",
				"    left + right",
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
						"  public int fct(final Integer left, final AtomicInteger right) {",
						"    return (left.intValue() + right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Long) : long {",
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
						"  public long fct(final Integer left, final Long right) {",
						"    return (left.intValue() + right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_plus_Integer_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Integer, right : AtomicDouble) : double {",
				"    left + right",
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
						"  public double fct(final Integer left, final AtomicDouble right) {",
						"    return (left.intValue() + right.doubleValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_power_Integer_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Integer, right : AtomicDouble) : double {",
				"    left ** right",
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
						"  public double fct(final Integer left, final AtomicDouble right) {",
						"    return Math.pow(left.intValue(), right.doubleValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_power_Integer_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : byte) : double {",
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
						"  public double fct(final Integer left, final byte right) {",
						"    return Math.pow(left.intValue(), right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_power_Integer_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : short) : double {",
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
						"  public double fct(final Integer left, final short right) {",
						"    return Math.pow(left.intValue(), right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_power_Integer_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : int) : double {",
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
						"  public double fct(final Integer left, final int right) {",
						"    return Math.pow(left.intValue(), right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_power_Integer_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : long) : double {",
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
						"  public double fct(final Integer left, final long right) {",
						"    return Math.pow(left.intValue(), right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_power_Integer_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : float) : double {",
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
						"  public double fct(final Integer left, final float right) {",
						"    return Math.pow(left.intValue(), right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_power_Integer_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : double) : double {",
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
						"  public double fct(final Integer left, final double right) {",
						"    return Math.pow(left.intValue(), right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : long) : long {",
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
						"  public long fct(final Integer left, final long right) {",
						"    return (left.intValue() / right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Long) : long {",
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
						"  public long fct(final Integer left, final Long right) {",
						"    return (left.intValue() / right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : byte) : int {",
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
						"  public int fct(final Integer left, final byte right) {",
						"    return (left.intValue() / right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Byte) : int {",
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
						"  public int fct(final Integer left, final Byte right) {",
						"    return (left.intValue() / right.byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : float) : float {",
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
						"  public float fct(final Integer left, final float right) {",
						"    return (left.intValue() / right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Float) : float {",
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
						"  public float fct(final Integer left, final Float right) {",
						"    return (left.intValue() / right.floatValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : int) : int {",
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
						"  public int fct(final Integer left, final int right) {",
						"    return (left.intValue() / right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Integer) : int {",
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
						"  public int fct(final Integer left, final Integer right) {",
						"    return (left.intValue() / right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Integer, right : AtomicDouble) : double {",
				"    left / right",
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
						"  public double fct(final Integer left, final AtomicDouble right) {",
						"    return (left.intValue() / right.doubleValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : short) : int {",
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
						"  public int fct(final Integer left, final short right) {",
						"    return (left.intValue() / right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Short) : int {",
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
						"  public int fct(final Integer left, final Short right) {",
						"    return (left.intValue() / right.shortValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Integer, right : AtomicInteger) : int {",
				"    left / right",
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
						"  public int fct(final Integer left, final AtomicInteger right) {",
						"    return (left.intValue() / right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_divide_Integer_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Integer, right : AtomicLong) : long {",
				"    left / right",
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
						"  public long fct(final Integer left, final AtomicLong right) {",
						"    return (left.intValue() / right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : long) : long {",
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
						"  public long fct(final Integer left, final long right) {",
						"    return (left.intValue() * right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Long) : long {",
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
						"  public long fct(final Integer left, final Long right) {",
						"    return (left.intValue() * right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : byte) : int {",
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
						"  public int fct(final Integer left, final byte right) {",
						"    return (left.intValue() * right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Byte) : int {",
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
						"  public int fct(final Integer left, final Byte right) {",
						"    return (left.intValue() * right.byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : float) : float {",
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
						"  public float fct(final Integer left, final float right) {",
						"    return (left.intValue() * right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Float) : float {",
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
						"  public float fct(final Integer left, final Float right) {",
						"    return (left.intValue() * right.floatValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : int) : int {",
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
						"  public int fct(final Integer left, final int right) {",
						"    return (left.intValue() * right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Integer) : int {",
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
						"  public int fct(final Integer left, final Integer right) {",
						"    return (left.intValue() * right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Integer, right : AtomicDouble) : double {",
				"    left * right",
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
						"  public double fct(final Integer left, final AtomicDouble right) {",
						"    return (left.intValue() * right.doubleValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : short) : int {",
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
						"  public int fct(final Integer left, final short right) {",
						"    return (left.intValue() * right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Short) : int {",
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
						"  public int fct(final Integer left, final Short right) {",
						"    return (left.intValue() * right.shortValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Integer, right : AtomicInteger) : int {",
				"    left * right",
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
						"  public int fct(final Integer left, final AtomicInteger right) {",
						"    return (left.intValue() * right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_multiply_Integer_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Integer, right : AtomicLong) : long {",
				"    left * right",
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
						"  public long fct(final Integer left, final AtomicLong right) {",
						"    return (left.intValue() * right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : long) : long {",
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
						"  public long fct(final Integer left, final long right) {",
						"    return (left.intValue() % right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Long) : long {",
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
						"  public long fct(final Integer left, final Long right) {",
						"    return (left.intValue() % right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Byte) : int {",
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
						"  public int fct(final Integer left, final Byte right) {",
						"    return (left.intValue() % right.byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : byte) : int {",
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
						"  public int fct(final Integer left, final byte right) {",
						"    return (left.intValue() % right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : float) : float {",
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
						"  public float fct(final Integer left, final float right) {",
						"    return (left.intValue() % right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Float) : float {",
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
						"  public float fct(final Integer left, final Float right) {",
						"    return (left.intValue() % right.floatValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : int) : int {",
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
						"  public int fct(final Integer left, final int right) {",
						"    return (left.intValue() % right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Integer) : int {",
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
						"  public int fct(final Integer left, final Integer right) {",
						"    return (left.intValue() % right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Integer, right : AtomicDouble) : double {",
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
						"  public double fct(final Integer left, final AtomicDouble right) {",
						"    return (left.intValue() % right.doubleValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : short) : int {",
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
						"  public int fct(final Integer left, final short right) {",
						"    return (left.intValue() % right);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Integer, right : Short) : int {",
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
						"  public int fct(final Integer left, final Short right) {",
						"    return (left.intValue() % right.shortValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Integer, right : AtomicInteger) : int {",
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
						"  public int fct(final Integer left, final AtomicInteger right) {",
						"    return (left.intValue() % right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_modulo_Integer_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Integer, right : AtomicLong) : long {",
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
						"  public long fct(final Integer left, final AtomicLong right) {",
						"    return (left.intValue() % right.longValue());",
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
