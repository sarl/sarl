/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.floatprimitive;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.tests.api.globalcompilation.GlobalCompilationSuite;
import io.sarl.lang.tests.api.globalcompilation.GlobalCompilationTestContribution;
import io.sarl.lang.tests.api.globalcompilation.ResourceSetGlobalCompilationContext;

/**
 * <p>XXX: Enable when the speed issue of implicitly imported functions is solved.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
@DisplayName("Compiling float operators")
@GlobalCompilationSuite
@Disabled("not yet added to the compiler")
@Tag("core")
@Tag("compileToJava")
public class CompilerTest extends AbstractSarlTest {

	@GlobalCompilationTestContribution
	public void operator_minus_float_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : double {",
				"    left - right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public double fct(final float left, final AtomicDouble right) {",
						"    return (left - right.doubleValue());",
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
	public void operator_minus_float_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long) : float {",
				"    left - right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Long right) {",
						"    return (left - right.longValue());",
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
	public void operator_minus_float_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte) : float {",
				"    left - right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Byte right) {",
						"    return (left - right.byteValue());",
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
	public void operator_minus_float_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : float {",
				"    left - right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Float right) {",
						"    return (left - right.floatValue());",
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
	public void operator_minus_float_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer) : float {",
				"    left - right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Integer right) {",
						"    return (left - right.intValue());",
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
	public void operator_minus_float_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short) : float {",
				"    left - right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Short right) {",
						"    return (left - right.shortValue());",
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
	public void operator_minus_float_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger) : float {",
				"    left - right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicInteger right) {",
						"    return (left - right.intValue());",
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
	public void operator_minus_float_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong): float {",
				"    left - right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicLong right) {",
						"    return (left - right.longValue());",
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
	public void operator_plus_float_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long): float {",
				"    left + right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Long right) {",
						"    return (left + right.longValue());",
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
	public void operator_plus_float_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte): float {",
				"    left + right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Byte right) {",
						"    return (left + right.byteValue());",
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
	public void operator_plus_float_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : float {",
				"    left + right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Float right) {",
						"    return (left + right.floatValue());",
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
	public void operator_plus_float_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer): float {",
				"    left + right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Integer right) {",
						"    return (left + right.intValue());",
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
	public void operator_plus_float_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short): float {",
				"    left + right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Short right) {",
						"    return (left + right.shortValue());",
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
	public void operator_plus_float_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger): float {",
				"    left + right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicInteger right) {",
						"    return (left + right.intValue());",
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
	public void operator_plus_float_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long): float {",
				"    left + right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Long right) {",
						"    return (left + right.longValue());",
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
	public void operator_plus_float_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : double {",
				"    left + right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public double fct(final float left, final AtomicDouble right) {",
						"    return (left + right.doubleValue());",
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
	public void operator_power_float_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : double {",
				"    left ** right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public double fct(final float left, final AtomicDouble right) {",
						"    return Math.pow(left, right.doubleValue());",
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
	public void operator_divide_float_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long): float {",
				"    left / right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Long right) {",
						"    return (left / right.longValue());",
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
	public void operator_divide_float_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte): float {",
				"    left / right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Byte right) {",
						"    return (left / right.byteValue());",
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
	public void operator_divide_float_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : float {",
				"    left / right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Float right) {",
						"    return (left / right.floatValue());",
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
	public void operator_divide_float_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer): float {",
				"    left / right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Integer right) {",
						"    return (left / right.intValue());",
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
	public void operator_divide_float_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : double {",
				"    left / right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public double fct(final float left, final AtomicDouble right) {",
						"    return (left / right.doubleValue());",
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
	public void operator_divide_float_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short): float {",
				"    left / right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Short right) {",
						"    return (left / right.shortValue());",
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
	public void operator_divide_float_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger): float {",
				"    left / right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicInteger right) {",
						"    return (left / right.intValue());",
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
	public void operator_divide_float_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong): float {",
				"    left / right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicLong right) {",
						"    return (left / right.longValue());",
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
	public void operator_multiply_float_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long): float {",
				"    left * right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Long right) {",
						"    return (left * right.longValue());",
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
	public void operator_multiply_float_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte): float {",
				"    left * right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Byte right) {",
						"    return (left * right.byteValue());",
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
	public void operator_multiply_float_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : float {",
				"    left * right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Float right) {",
						"    return (left * right.floatValue());",
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
	public void operator_multiply_float_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer): float {",
				"    left * right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Integer right) {",
						"    return (left * right.intValue());",
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
	public void operator_multiply_float_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : double {",
				"    left * right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public double fct(final float left, final AtomicDouble right) {",
						"    return (left * right.doubleValue());",
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
	public void operator_multiply_float_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short): float {",
				"    left * right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Short right) {",
						"    return (left * right.shortValue());",
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
	public void operator_multiply_float_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger): float {",
				"    left * right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicInteger right) {",
						"    return (left * right.intValue());",
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
	public void operator_multiply_float_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong): float {",
				"    left * right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicLong right) {",
						"    return (left * right.longValue());",
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
	public void operator_modulo_float_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long): float {",
				"    left % right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Long right) {",
						"    return (left % right.longValue());",
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
	public void operator_modulo_float_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte): float {",
				"    left % right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Byte right) {",
						"    return (left % right.byteValue());",
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
	public void operator_modulo_float_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : float {",
				"    left % right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Float right) {",
						"    return (left % right.floatValue());",
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
	public void operator_modulo_float_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer): float {",
				"    left % right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Integer right) {",
						"    return (left % right.intValue());",
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
	public void operator_modulo_float_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : double {",
				"    left % right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public double fct(final float left, final AtomicDouble right) {",
						"    return (left % right.doubleValue());",
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
	public void operator_modulo_float_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short): float {",
				"    left % right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final Short right) {",
						"    return (left % right.shortValue());",
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
	public void operator_modulo_float_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger): float {",
				"    left % right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicInteger right) {",
						"    return (left % right.intValue());",
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
	public void operator_modulo_float_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong): float {",
				"    left % right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final float left, final AtomicLong right) {",
						"    return (left % right.longValue());",
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
