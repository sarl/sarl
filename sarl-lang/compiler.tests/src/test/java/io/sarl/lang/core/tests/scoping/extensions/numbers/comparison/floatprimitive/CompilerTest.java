/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.comparison.floatprimitive;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.tests.api.globalcompilation.GlobalCompilationSuite;
import io.sarl.lang.tests.api.globalcompilation.GlobalCompilationTestContribution;
import io.sarl.lang.tests.api.globalcompilation.ResourceSetGlobalCompilationContext;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@GlobalCompilationSuite
@SuppressWarnings("all")
@DisplayName("Compiling float comparison operators")
@Tag("core")
@Tag("compileToJava")
public class CompilerTest extends AbstractSarlTest {

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : byte) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : short) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : int) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final int right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : long) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : float) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : double) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Integer right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Double) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Double right) {",
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


	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicInteger right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicLong right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterThan_short_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : boolean {",
				"    left > right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicDouble right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : byte) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : short) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : int) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final int right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : long) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : float) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : double) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Integer right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Double) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicInteger right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicLong right) {",
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

	@GlobalCompilationTestContribution
	public void operator_greaterEqualsThan_short_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : boolean {",
				"    left >= right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicDouble right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : byte) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : short) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : int) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final int right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : long) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : float) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : double) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Integer right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Double) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicInteger right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicLong right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerThan_short_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : boolean {",
				"    left < right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicDouble right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : byte) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : short) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : int) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final int right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : long) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : float) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : double) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Integer right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Double) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicInteger right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicLong right) {",
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

	@GlobalCompilationTestContribution
	public void operator_lowerEqualsThan_short_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : boolean {",
				"    left <= right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicDouble right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : byte) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : short) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : int) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final int right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : long) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : float) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : double) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Integer right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Double) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicInteger right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicLong right) {",
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

	@GlobalCompilationTestContribution
	public void operator_equals_short_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : boolean {",
				"    left == right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicDouble right) {",
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


	@GlobalCompilationTestContribution
	public void operator_notEquals_short_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : byte) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : short) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : int) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final int right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : long) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : float) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : double) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Integer right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Double) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final Double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicInteger right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicLong right) {",
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

	@GlobalCompilationTestContribution
	public void operator_notEquals_short_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : boolean {",
				"    left != right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public boolean fct(final float left, final AtomicDouble right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : byte) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final byte right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : short) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final short right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : int) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final int right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : long) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final long right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : float) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : double) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Byte) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final Byte right) {",
						"    return Float.compare(left, right.byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Short) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final Short right) {",
						"    return Float.compare(left, right.shortValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Integer) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final Integer right) {",
						"    return Float.compare(left, right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Long) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final Long right) {",
						"    return Float.compare(left, right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Float) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final Float right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : float, right : Double) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final Double right) {",
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

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : float, right : AtomicInteger) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final AtomicInteger right) {",
						"    return Float.compare(left, right.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : float, right : AtomicLong) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.util.concurrent.atomic.AtomicLong;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final AtomicLong right) {",
						"    return Float.compare(left, right.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void operator_spaceship_short_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : float, right : AtomicDouble) : int {",
				"    left <=> right",
				"  }",
				"}"),
				multilineString(
						"import com.google.common.util.concurrent.AtomicDouble;",
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public int fct(final float left, final AtomicDouble right) {",
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
