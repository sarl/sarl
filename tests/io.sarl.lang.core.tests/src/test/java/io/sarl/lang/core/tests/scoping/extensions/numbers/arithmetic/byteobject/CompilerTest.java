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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.byteobject;

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
@DisplayName("Compiling Byte operators")
@GlobalCompilationSuite
@Disabled("not yet added to the compiler")
@Tag("core")
@Tag("compileToJava")
public class CompilerTest extends AbstractSarlTest {

	@GlobalCompilationTestContribution
	public void operator_minus_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : int {",
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
						"  public int fct(final Byte left) {",
						"    return (-(left.byteValue()));",
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
	public void operator_minus_Byte_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Byte, right : AtomicDouble) : double {",
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
						"  public double fct(final Byte left, final AtomicDouble right) {",
						"    return (left.byteValue() - right.doubleValue());",
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
	public void operator_minus_Byte_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : long) : long {",
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
						"  public long fct(final Byte left, final long right) {",
						"    return (left.byteValue() - right);",
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
	public void operator_minus_Byte_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Long) : long {",
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
						"  public long fct(final Byte left, final Long right) {",
						"    return (left.byteValue() - right.longValue());",
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
	public void operator_minus_Byte_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : byte) : int {",
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
						"  public int fct(final Byte left, final byte right) {",
						"    return (left.byteValue() - right);",
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
	public void operator_minus_Byte_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Byte) : int {",
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
						"  public int fct(final Byte left, final Byte right) {",
						"    return (left.byteValue() - right.byteValue());",
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
	public void operator_minus_Byte_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : float) : float {",
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
						"  public float fct(final Byte left, final float right) {",
						"    return (left.byteValue() - right);",
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
	public void operator_minus_Byte_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Float) : float {",
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
						"  public float fct(final Byte left, final Float right) {",
						"    return (left.byteValue() - right.floatValue());",
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
	public void operator_minus_Byte_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : int) : int {",
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
						"  public int fct(final Byte left, final int right) {",
						"    return (left.byteValue() - right);",
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
	public void operator_minus_Byte_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Integer) : int {",
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
						"  public int fct(final Byte left, final Integer right) {",
						"    return (left.byteValue() - right.intValue());",
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
	public void operator_minus_Byte_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : short) : int {",
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
						"  public int fct(final Byte left, final short right) {",
						"    return (left.byteValue() - right);",
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
	public void operator_minus_Byte_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Short) : int {",
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
						"  public int fct(final Byte left, final Short right) {",
						"    return (left.byteValue() - right.shortValue());",
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
	public void operator_minus_Byte_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Byte, right : AtomicInteger) : int {",
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
						"  public int fct(final Byte left, final AtomicInteger right) {",
						"    return (left.byteValue() - right.intValue());",
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
	public void operator_minus_Byte_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Byte, right : AtomicLong) : long {",
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
						"  public long fct(final Byte left, final AtomicLong right) {",
						"    return (left.byteValue() - right.longValue());",
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
	public void operator_plus_Byte_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Long) : long {",
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
						"  public long fct(final Byte left, final Long right) {",
						"    return (left.byteValue() + right.longValue());",
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
	public void operator_plus_Byte_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : long) : long {",
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
						"  public long fct(final Byte left, final long right) {",
						"    return (left.byteValue() + right);",
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
	public void operator_plus_Byte_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : byte) : int {",
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
						"  public int fct(final Byte left, final byte right) {",
						"    return (left.byteValue() + right);",
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
	public void operator_plus_Byte_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Byte) : int {",
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
						"  public int fct(final Byte left, final Byte right) {",
						"    return (left.byteValue() + right.byteValue());",
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
	public void operator_plus_Byte_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Float) : float {",
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
						"  public float fct(final Byte left, final Float right) {",
						"    return (left.byteValue() + right.floatValue());",
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
	public void operator_plus_Byte_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : float) : float {",
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
						"  public float fct(final Byte left, final float right) {",
						"    return (left.byteValue() + right);",
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
	public void operator_plus_Byte_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Integer) : int {",
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
						"  public int fct(final Byte left, final Integer right) {",
						"    return (left.byteValue() + right.intValue());",
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
	public void operator_plus_Byte_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : int) : int {",
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
						"  public int fct(final Byte left, final int right) {",
						"    return (left.byteValue() + right);",
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
	public void operator_plus_Byte_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : short) : int {",
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
						"  public int fct(final Byte left, final short right) {",
						"    return (left.byteValue() + right);",
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
	public void operator_plus_Byte_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Short) : int {",
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
						"  public int fct(final Byte left, final Short right) {",
						"    return (left.byteValue() + right.shortValue());",
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
	public void operator_plus_Byte_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Byte, right : AtomicInteger) : int {",
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
						"  public int fct(final Byte left, final AtomicInteger right) {",
						"    return (left.byteValue() + right.intValue());",
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
	public void operator_plus_Byte_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Long) : long {",
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
						"  public long fct(final Byte left, final Long right) {",
						"    return (left.byteValue() + right.longValue());",
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
	public void operator_plus_Byte_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Byte, right : AtomicDouble) : double {",
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
						"  public double fct(final Byte left, final AtomicDouble right) {",
						"    return (left.byteValue() + right.doubleValue());",
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
	public void operator_power_Byte_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Byte, right : AtomicDouble) : double {",
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
						"  public double fct(final Byte left, final AtomicDouble right) {",
						"    return Math.pow(left.byteValue(), right.doubleValue());",
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
	public void operator_power_Byte_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : byte) : double {",
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
						"  public double fct(final Byte left, final byte right) {",
						"    return Math.pow(left.byteValue(), right);",
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
	public void operator_power_Byte_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : short) : double {",
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
						"  public double fct(final Byte left, final short right) {",
						"    return Math.pow(left.byteValue(), right);",
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
	public void operator_power_Byte_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : int) : double {",
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
						"  public double fct(final Byte left, final int right) {",
						"    return Math.pow(left.byteValue(), right);",
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
	public void operator_power_Byte_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : long) : double {",
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
						"  public double fct(final Byte left, final long right) {",
						"    return Math.pow(left.byteValue(), right);",
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
	public void operator_power_Byte_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : float) : double {",
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
						"  public double fct(final Byte left, final float right) {",
						"    return Math.pow(left.byteValue(), right);",
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
	public void operator_power_Byte_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : double) : double {",
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
						"  public double fct(final Byte left, final double right) {",
						"    return Math.pow(left.byteValue(), right);",
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
	public void operator_divide_Byte_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : long) : long {",
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
						"  public long fct(final Byte left, final long right) {",
						"    return (left.byteValue() / right);",
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
	public void operator_divide_Byte_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Long) : long {",
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
						"  public long fct(final Byte left, final Long right) {",
						"    return (left.byteValue() / right.longValue());",
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
	public void operator_divide_Byte_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : byte) : int {",
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
						"  public int fct(final Byte left, final byte right) {",
						"    return (left.byteValue() / right);",
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
	public void operator_divide_Byte_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Byte) : int {",
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
						"  public int fct(final Byte left, final Byte right) {",
						"    return (left.byteValue() / right.byteValue());",
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
	public void operator_divide_Byte_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : float) : float {",
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
						"  public float fct(final Byte left, final float right) {",
						"    return (left.byteValue() / right);",
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
	public void operator_divide_Byte_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Float) : float {",
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
						"  public float fct(final Byte left, final Float right) {",
						"    return (left.byteValue() / right.floatValue());",
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
	public void operator_divide_Byte_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : int) : int {",
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
						"  public int fct(final Byte left, final int right) {",
						"    return (left.byteValue() / right);",
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
	public void operator_divide_Byte_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Integer) : int {",
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
						"  public int fct(final Byte left, final Integer right) {",
						"    return (left.byteValue() / right.intValue());",
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
	public void operator_divide_Byte_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Byte, right : AtomicDouble) : double {",
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
						"  public double fct(final Byte left, final AtomicDouble right) {",
						"    return (left.byteValue() / right.doubleValue());",
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
	public void operator_divide_Byte_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : short) : int {",
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
						"  public int fct(final Byte left, final short right) {",
						"    return (left.byteValue() / right);",
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
	public void operator_divide_Byte_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Short) : int {",
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
						"  public int fct(final Byte left, final Short right) {",
						"    return (left.byteValue() / right.shortValue());",
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
	public void operator_divide_Byte_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Byte, right : AtomicInteger) : int {",
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
						"  public int fct(final Byte left, final AtomicInteger right) {",
						"    return (left.byteValue() / right.intValue());",
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
	public void operator_divide_Byte_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Byte, right : AtomicLong) : long {",
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
						"  public long fct(final Byte left, final AtomicLong right) {",
						"    return (left.byteValue() / right.longValue());",
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
	public void operator_multiply_Byte_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : long) : long {",
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
						"  public long fct(final Byte left, final long right) {",
						"    return (left.byteValue() * right);",
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
	public void operator_multiply_Byte_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Long) : long {",
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
						"  public long fct(final Byte left, final Long right) {",
						"    return (left.byteValue() * right.longValue());",
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
	public void operator_multiply_Byte_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : byte) : int {",
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
						"  public int fct(final Byte left, final byte right) {",
						"    return (left.byteValue() * right);",
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
	public void operator_multiply_Byte_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Byte) : int {",
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
						"  public int fct(final Byte left, final Byte right) {",
						"    return (left.byteValue() * right.byteValue());",
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
	public void operator_multiply_Byte_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : float) : float {",
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
						"  public float fct(final Byte left, final float right) {",
						"    return (left.byteValue() * right);",
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
	public void operator_multiply_Byte_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Float) : float {",
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
						"  public float fct(final Byte left, final Float right) {",
						"    return (left.byteValue() * right.floatValue());",
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
	public void operator_multiply_Byte_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : int) : int {",
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
						"  public int fct(final Byte left, final int right) {",
						"    return (left.byteValue() * right);",
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
	public void operator_multiply_Byte_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Integer) : int {",
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
						"  public int fct(final Byte left, final Integer right) {",
						"    return (left.byteValue() * right.intValue());",
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
	public void operator_multiply_Byte_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Byte, right : AtomicDouble) : double {",
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
						"  public double fct(final Byte left, final AtomicDouble right) {",
						"    return (left.byteValue() * right.doubleValue());",
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
	public void operator_multiply_Byte_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : short) : int {",
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
						"  public int fct(final Byte left, final short right) {",
						"    return (left.byteValue() * right);",
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
	public void operator_multiply_Byte_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Short) : int {",
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
						"  public int fct(final Byte left, final Short right) {",
						"    return (left.byteValue() * right.shortValue());",
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
	public void operator_multiply_Byte_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Byte, right : AtomicInteger) : int {",
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
						"  public int fct(final Byte left, final AtomicInteger right) {",
						"    return (left.byteValue() * right.intValue());",
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
	public void operator_multiply_Byte_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Byte, right : AtomicLong) : long {",
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
						"  public long fct(final Byte left, final AtomicLong right) {",
						"    return (left.byteValue() * right.longValue());",
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
	public void operator_modulo_Byte_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : long) : long {",
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
						"  public long fct(final Byte left, final long right) {",
						"    return (left.byteValue() % right);",
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
	public void operator_modulo_Byte_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Long) : long {",
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
						"  public long fct(final Byte left, final Long right) {",
						"    return (left.byteValue() % right.longValue());",
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
	public void operator_modulo_Byte_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Byte) : int {",
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
						"  public int fct(final Byte left, final Byte right) {",
						"    return (left.byteValue() % right.byteValue());",
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
	public void operator_modulo_Byte_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : byte) : int {",
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
						"  public int fct(final Byte left, final byte right) {",
						"    return (left.byteValue() % right);",
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
	public void operator_modulo_Byte_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : float) : float {",
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
						"  public float fct(final Byte left, final float right) {",
						"    return (left.byteValue() % right);",
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
	public void operator_modulo_Byte_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Float) : float {",
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
						"  public float fct(final Byte left, final Float right) {",
						"    return (left.byteValue() % right.floatValue());",
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
	public void operator_modulo_Byte_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : int) : int {",
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
						"  public int fct(final Byte left, final int right) {",
						"    return (left.byteValue() % right);",
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
	public void operator_modulo_Byte_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Integer) : int {",
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
						"  public int fct(final Byte left, final Integer right) {",
						"    return (left.byteValue() % right.intValue());",
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
	public void operator_modulo_Byte_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Byte, right : AtomicDouble) : double {",
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
						"  public double fct(final Byte left, final AtomicDouble right) {",
						"    return (left.byteValue() % right.doubleValue());",
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
	public void operator_modulo_Byte_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : short) : int {",
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
						"  public int fct(final Byte left, final short right) {",
						"    return (left.byteValue() % right);",
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
	public void operator_modulo_Byte_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Byte, right : Short) : int {",
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
						"  public int fct(final Byte left, final Short right) {",
						"    return (left.byteValue() % right.shortValue());",
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
	public void operator_modulo_Byte_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Byte, right : AtomicInteger) : int {",
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
						"  public int fct(final Byte left, final AtomicInteger right) {",
						"    return (left.byteValue() % right.intValue());",
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
	public void operator_modulo_Byte_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Byte, right : AtomicLong) : long {",
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
						"  public long fct(final Byte left, final AtomicLong right) {",
						"    return (left.byteValue() % right.longValue());",
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
