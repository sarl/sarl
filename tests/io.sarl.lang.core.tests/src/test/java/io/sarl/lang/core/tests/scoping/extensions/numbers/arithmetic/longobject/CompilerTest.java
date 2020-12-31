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

package io.sarl.lang.core.tests.scoping.extensions.numbers.arithmetic.longobject;

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
@DisplayName("Compiling Long operators")
@GlobalCompilationSuite
@Disabled("not yet added to the compiler")
@Tag("core")
@Tag("compileToJava")
public class CompilerTest extends AbstractSarlTest {

	@GlobalCompilationTestContribution
	public void operator_minus_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Long) : long {",
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
						"  public long fct(final Long left) {",
						"    return (-(left.longValue()));",
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
	public void operator_minus_Long_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Long, right : AtomicDouble) : double {",
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
						"  public double fct(final Long left, final AtomicDouble right) {",
						"    return (left.longValue() - right.doubleValue());",
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
	public void operator_minus_Long_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : long) : long {",
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
						"  public long fct(final Long left, final long right) {",
						"    return (left.longValue() - right);",
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
	public void operator_minus_Long_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Long) : long {",
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
						"  public long fct(final Long left, final Long right) {",
						"    return (left.longValue() - right.longValue());",
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
	public void operator_minus_Long_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : byte) : long {",
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
						"  public long fct(final Long left, final byte right) {",
						"    return (left.longValue() - right);",
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
	public void operator_minus_Long_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Byte) : long {",
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
						"  public long fct(final Long left, final Byte right) {",
						"    return (left.longValue() - right.byteValue());",
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
	public void operator_minus_Long_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : float) : float {",
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
						"  public float fct(final Long left, final float right) {",
						"    return (left.longValue() - right);",
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
	public void operator_minus_Long_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Float) : float {",
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
						"  public float fct(final Long left, final Float right) {",
						"    return (left.longValue() - right.floatValue());",
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
	public void operator_minus_Long_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : int) : long {",
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
						"  public long fct(final Long left, final int right) {",
						"    return (left.longValue() - right);",
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
	public void operator_minus_Long_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Integer) : long {",
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
						"  public long fct(final Long left, final Integer right) {",
						"    return (left.longValue() - right.intValue());",
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
	public void operator_minus_Long_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : short) : long {",
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
						"  public long fct(final Long left, final short right) {",
						"    return (left.longValue() - right);",
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
	public void operator_minus_Long_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Short) : long {",
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
						"  public long fct(final Long left, final Short right) {",
						"    return (left.longValue() - right.shortValue());",
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
	public void operator_minus_Long_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Long, right : AtomicInteger) : long {",
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
						"  public long fct(final Long left, final AtomicInteger right) {",
						"    return (left.longValue() - right.intValue());",
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
	public void operator_minus_Long_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Long, right : AtomicLong) : long {",
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
						"  public long fct(final Long left, final AtomicLong right) {",
						"    return (left.longValue() - right.longValue());",
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
	public void operator_plus_Long_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Long) : long {",
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
						"  public long fct(final Long left, final Long right) {",
						"    return (left.longValue() + right.longValue());",
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
	public void operator_plus_Long_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : long) : long {",
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
						"  public long fct(final Long left, final long right) {",
						"    return (left.longValue() + right);",
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
	public void operator_plus_Long_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : byte) : long {",
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
						"  public long fct(final Long left, final byte right) {",
						"    return (left.longValue() + right);",
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
	public void operator_plus_Long_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Byte) : long {",
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
						"  public long fct(final Long left, final Byte right) {",
						"    return (left.longValue() + right.byteValue());",
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
	public void operator_plus_Long_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Float) : float {",
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
						"  public float fct(final Long left, final Float right) {",
						"    return (left.longValue() + right.floatValue());",
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
	public void operator_plus_Long_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : float) : float {",
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
						"  public float fct(final Long left, final float right) {",
						"    return (left.longValue() + right);",
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
	public void operator_plus_Long_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Integer) : long {",
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
						"  public long fct(final Long left, final Integer right) {",
						"    return (left.longValue() + right.intValue());",
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
	public void operator_plus_Long_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : int) : long {",
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
						"  public long fct(final Long left, final int right) {",
						"    return (left.longValue() + right);",
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
	public void operator_plus_Long_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : short) : long {",
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
						"  public long fct(final Long left, final short right) {",
						"    return (left.longValue() + right);",
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
	public void operator_plus_Long_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Short) : long {",
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
						"  public long fct(final Long left, final Short right) {",
						"    return (left.longValue() + right.shortValue());",
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
	public void operator_plus_Long_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Long, right : AtomicInteger) : long {",
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
						"  public long fct(final Long left, final AtomicInteger right) {",
						"    return (left.longValue() + right.intValue());",
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
	public void operator_plus_Long_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Long) : long {",
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
						"  public long fct(final Long left, final Long right) {",
						"    return (left.longValue() + right.longValue());",
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
	public void operator_plus_Long_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Long, right : AtomicDouble) : double {",
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
						"  public double fct(final Long left, final AtomicDouble right) {",
						"    return (left.longValue() + right.doubleValue());",
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
	public void operator_power_Long_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Long, right : AtomicDouble) : double {",
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
						"  public double fct(final Long left, final AtomicDouble right) {",
						"    return Math.pow(left.longValue(), right.doubleValue());",
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
	public void operator_power_Long_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : byte) : double {",
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
						"  public double fct(final Long left, final byte right) {",
						"    return Math.pow(left.longValue(), right);",
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
	public void operator_power_Long_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : short) : double {",
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
						"  public double fct(final Long left, final short right) {",
						"    return Math.pow(left.longValue(), right);",
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
	public void operator_power_Long_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : int) : double {",
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
						"  public double fct(final Long left, final int right) {",
						"    return Math.pow(left.longValue(), right);",
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
	public void operator_power_Long_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : long) : double {",
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
						"  public double fct(final Long left, final long right) {",
						"    return Math.pow(left.longValue(), right);",
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
	public void operator_power_Long_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : float) : double {",
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
						"  public double fct(final Long left, final float right) {",
						"    return Math.pow(left.longValue(), right);",
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
	public void operator_power_Long_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : double) : double {",
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
						"  public double fct(final Long left, final double right) {",
						"    return Math.pow(left.longValue(), right);",
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
	public void operator_divide_Long_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : long) : long {",
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
						"  public long fct(final Long left, final long right) {",
						"    return (left.longValue() / right);",
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
	public void operator_divide_Long_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Long) : long {",
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
						"  public long fct(final Long left, final Long right) {",
						"    return (left.longValue() / right.longValue());",
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
	public void operator_divide_Long_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : byte) : long {",
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
						"  public long fct(final Long left, final byte right) {",
						"    return (left.longValue() / right);",
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
	public void operator_divide_Long_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Byte) : long {",
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
						"  public long fct(final Long left, final Byte right) {",
						"    return (left.longValue() / right.byteValue());",
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
	public void operator_divide_Long_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : float) : float {",
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
						"  public float fct(final Long left, final float right) {",
						"    return (left.longValue() / right);",
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
	public void operator_divide_Long_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Float) : float {",
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
						"  public float fct(final Long left, final Float right) {",
						"    return (left.longValue() / right.floatValue());",
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
	public void operator_divide_Long_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : int) : long {",
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
						"  public long fct(final Long left, final int right) {",
						"    return (left.longValue() / right);",
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
	public void operator_divide_Long_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Integer) : long {",
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
						"  public long fct(final Long left, final Integer right) {",
						"    return (left.longValue() / right.intValue());",
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
	public void operator_divide_Long_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Long, right : AtomicDouble) : double {",
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
						"  public double fct(final Long left, final AtomicDouble right) {",
						"    return (left.longValue() / right.doubleValue());",
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
	public void operator_divide_Long_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : short) : long {",
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
						"  public long fct(final Long left, final short right) {",
						"    return (left.longValue() / right);",
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
	public void operator_divide_Long_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Short) : long {",
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
						"  public long fct(final Long left, final Short right) {",
						"    return (left.longValue() / right.shortValue());",
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
	public void operator_divide_Long_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Long, right : AtomicInteger) : long {",
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
						"  public long fct(final Long left, final AtomicInteger right) {",
						"    return (left.longValue() / right.intValue());",
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
	public void operator_divide_Long_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Long, right : AtomicLong) : long {",
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
						"  public long fct(final Long left, final AtomicLong right) {",
						"    return (left.longValue() / right.longValue());",
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
	public void operator_multiply_Long_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : long) : long {",
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
						"  public long fct(final Long left, final long right) {",
						"    return (left.longValue() * right);",
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
	public void operator_multiply_Long_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Long) : long {",
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
						"  public long fct(final Long left, final Long right) {",
						"    return (left.longValue() * right.longValue());",
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
	public void operator_multiply_Long_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : byte) : long {",
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
						"  public long fct(final Long left, final byte right) {",
						"    return (left.longValue() * right);",
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
	public void operator_multiply_Long_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Byte) : long {",
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
						"  public long fct(final Long left, final Byte right) {",
						"    return (left.longValue() * right.byteValue());",
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
	public void operator_multiply_Long_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : float) : float {",
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
						"  public float fct(final Long left, final float right) {",
						"    return (left.longValue() * right);",
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
	public void operator_multiply_Long_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Float) : float {",
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
						"  public float fct(final Long left, final Float right) {",
						"    return (left.longValue() * right.floatValue());",
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
	public void operator_multiply_Long_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : int) : long {",
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
						"  public long fct(final Long left, final int right) {",
						"    return (left.longValue() * right);",
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
	public void operator_multiply_Long_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Integer) : long {",
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
						"  public long fct(final Long left, final Integer right) {",
						"    return (left.longValue() * right.intValue());",
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
	public void operator_multiply_Long_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Long, right : AtomicDouble) : double {",
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
						"  public double fct(final Long left, final AtomicDouble right) {",
						"    return (left.longValue() * right.doubleValue());",
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
	public void operator_multiply_Long_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : short) : long {",
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
						"  public long fct(final Long left, final short right) {",
						"    return (left.longValue() * right);",
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
	public void operator_multiply_Long_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Short) : long {",
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
						"  public long fct(final Long left, final Short right) {",
						"    return (left.longValue() * right.shortValue());",
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
	public void operator_multiply_Long_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Long, right : AtomicInteger) : long {",
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
						"  public long fct(final Long left, final AtomicInteger right) {",
						"    return (left.longValue() * right.intValue());",
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
	public void operator_multiply_Long_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Long, right : AtomicLong) : long {",
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
						"  public long fct(final Long left, final AtomicLong right) {",
						"    return (left.longValue() * right.longValue());",
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
	public void operator_modulo_Long_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : long) : long {",
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
						"  public long fct(final Long left, final long right) {",
						"    return (left.longValue() % right);",
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
	public void operator_modulo_Long_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Long) : long {",
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
						"  public long fct(final Long left, final Long right) {",
						"    return (left.longValue() % right.longValue());",
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
	public void operator_modulo_Long_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Byte) : long {",
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
						"  public long fct(final Long left, final Byte right) {",
						"    return (left.longValue() % right.byteValue());",
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
	public void operator_modulo_Long_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : byte) : long {",
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
						"  public long fct(final Long left, final byte right) {",
						"    return (left.longValue() % right);",
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
	public void operator_modulo_Long_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : float) : float {",
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
						"  public float fct(final Long left, final float right) {",
						"    return (left.longValue() % right);",
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
	public void operator_modulo_Long_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Float) : float {",
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
						"  public float fct(final Long left, final Float right) {",
						"    return (left.longValue() % right.floatValue());",
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
	public void operator_modulo_Long_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : int) : long {",
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
						"  public long fct(final Long left, final int right) {",
						"    return (left.longValue() % right);",
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
	public void operator_modulo_Long_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Integer) : long {",
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
						"  public long fct(final Long left, final Integer right) {",
						"    return (left.longValue() % right.intValue());",
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
	public void operator_modulo_Long_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Long, right : AtomicDouble) : double {",
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
						"  public double fct(final Long left, final AtomicDouble right) {",
						"    return (left.longValue() % right.doubleValue());",
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
	public void operator_modulo_Long_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : short) : long {",
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
						"  public long fct(final Long left, final short right) {",
						"    return (left.longValue() % right);",
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
	public void operator_modulo_Long_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Long, right : Short) : long {",
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
						"  public long fct(final Long left, final Short right) {",
						"    return (left.longValue() % right.shortValue());",
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
	public void operator_modulo_Long_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Long, right : AtomicInteger) : long {",
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
						"  public long fct(final Long left, final AtomicInteger right) {",
						"    return (left.longValue() % right.intValue());",
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
	public void operator_modulo_Long_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Long, right : AtomicLong) : long {",
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
						"  public long fct(final Long left, final AtomicLong right) {",
						"    return (left.longValue() % right.longValue());",
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
