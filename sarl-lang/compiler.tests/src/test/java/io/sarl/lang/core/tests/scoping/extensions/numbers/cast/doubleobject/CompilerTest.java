/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.doubleobject;

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
@SuppressWarnings("all")
@DisplayName("Compiling Double cast operator")
@GlobalCompilationSuite
@Tag("core")
@Tag("compileToJava")
public class CompilerTest extends AbstractSarlTest {

	@GlobalCompilationTestContribution
	public void as_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : byte {",
				"    left as byte",
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
						"  public byte fct(final Double left) {",
						"    return (left == null ? 0 : left.byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : short {",
				"    left as short",
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
						"  public short fct(final Double left) {",
						"    return (left == null ? 0 : left.shortValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : int {",
				"    left as int",
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
						"  public int fct(final Double left) {",
						"    return (left == null ? 0 : left.intValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : long {",
				"    left as long",
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
						"  public long fct(final Double left) {",
						"    return (left == null ? 0 : left.longValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : float {",
				"    left as float",
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
						"  public float fct(final Double left) {",
						"    return (left == null ? 0 : left.floatValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : double {",
				"    left as double",
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
						"  public double fct(final Double left) {",
						"    return ((left) == null ? 0 : (left).doubleValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Byte {",
				"    left as Byte",
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
						"  public Byte fct(final Double left) {",
						"    return (left == null ? null : Byte.valueOf(left.byteValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Short {",
				"    left as Short",
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
						"  public Short fct(final Double left) {",
						"    return (left == null ? null : Short.valueOf(left.shortValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Integer {",
				"    left as Integer",
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
						"  public Integer fct(final Double left) {",
						"    return (left == null ? null : Integer.valueOf(left.intValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Long {",
				"    left as Long",
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
						"  public Long fct(final Double left) {",
						"    return (left == null ? null : Long.valueOf(left.longValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Float {",
				"    left as Float",
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
						"  public Float fct(final Double left) {",
						"    return (left == null ? null : Float.valueOf(left.floatValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Double {",
				"    left as Double",
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
						"  public Double fct(final Double left) {",
						"    return left;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Double) : AtomicInteger {",
				"    left as AtomicInteger",
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
						"  public AtomicInteger fct(final Double left) {",
						"    return (left == null ? null : new AtomicInteger(left.intValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Double) : AtomicLong {",
				"    left as AtomicLong",
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
						"  public AtomicLong fct(final Double left) {",
						"    return (left == null ? null : new AtomicLong(left.longValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_AtomicDouble(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Double) : AtomicDouble {",
				"    left as AtomicDouble",
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
						"  public AtomicDouble fct(final Double left) {",
						"    return (left == null ? null : new AtomicDouble(left.doubleValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_BigInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.math.BigInteger",
				"class A {",
				"  def fct(left : Double) : BigInteger {",
				"    left as BigInteger",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.math.BigInteger;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public BigInteger fct(final Double left) {",
						"    return (left == null ? null : BigInteger.valueOf(left.longValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_BigDecimal(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"import java.math.BigDecimal",
				"class A {",
				"  def fct(left : Double) : BigDecimal {",
				"    left as BigDecimal",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.core.annotation.SarlElementType;",
						"import io.sarl.lang.core.annotation.SarlSpecification;",
						"import io.sarl.lang.core.annotation.SyntheticMember;",
						"import java.math.BigDecimal;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@XbaseGenerated",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public BigDecimal fct(final Double left) {",
						"    return (left == null ? null : BigDecimal.valueOf(left.doubleValue()));",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@GlobalCompilationTestContribution
	public void as_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Number {",
				"    left as Number",
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
						"  public Number fct(final Double left) {",
						"    return left;",
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
