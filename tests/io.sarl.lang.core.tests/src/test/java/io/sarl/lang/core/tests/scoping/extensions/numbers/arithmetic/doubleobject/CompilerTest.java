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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.doubleobject;

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
@DisplayName("Compiling Double operators")
@GlobalCompilationSuite
@Disabled("not yet added to the compiler")
@Tag("core")
@Tag("compileToJava")
public class CompilerTest extends AbstractSarlTest {
	
	@GlobalCompilationTestContribution
	public void  operator_minus_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_minus_Double_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_plus_Double_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_power_Double_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_power_Double_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_power_Double_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_power_Double_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_power_Double_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_power_Double_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_power_Double_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_divide_Double_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_multiply_Double_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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

	@GlobalCompilationTestContribution
	public void  operator_modulo_Double_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
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
