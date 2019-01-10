/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.byteobject;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractMassiveCompilationTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@SuppressWarnings("all")
public class CompilerTest extends AbstractMassiveCompilationTest {

	@DifferedTest
	public void as_byte() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : byte {",
				"    left as byte",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public byte fct(final Byte left) {",
						"    return ((byte) (left).byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@DifferedTest
	public void as_short() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : short {",
				"    left as short",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public short fct(final Byte left) {",
						"    return ((short) (left).byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@DifferedTest
	public void as_int() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : int {",
				"    left as int",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
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
						"    return ((int) (left).byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@DifferedTest
	public void as_long() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : long {",
				"    left as long",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public long fct(final Byte left) {",
						"    return ((long) (left).byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@DifferedTest
	public void as_float() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : float {",
				"    left as float",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public float fct(final Byte left) {",
						"    return ((float) (left).byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@DifferedTest
	public void as_double() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : double {",
				"    left as double",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public double fct(final Byte left) {",
						"    return ((double) (left).byteValue());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@DifferedTest
	public void as_Byte() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : Byte {",
				"    left as Byte",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public Byte fct(final Byte left) {",
						"    return ((Byte) left);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public A() {",
						"    super();",
						"  }",
						"}",
						""));
	}

	@DifferedTest
	public void as_Short() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Byte) : Short {",
				"    left as Short",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Integer() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Byte) : Integer {",
				"    left as Integer",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Long() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Byte) : Long {",
				"    left as Long",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Float() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Byte) : Float {",
				"    left as Float",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Double() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Byte) : Double {",
				"    left as Double",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_AtomicInteger() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Byte) : AtomicInteger {",
				"    left as AtomicInteger",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_AtomicLong() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Byte) : AtomicLong {",
				"    left as AtomicLong",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_AtomicDouble() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Byte) : AtomicLong {",
				"    left as AtomicDouble",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Number() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Byte) : Number {",
				"    left as Number",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public Number fct(final Byte left) {",
						"    return ((Number) left);",
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
