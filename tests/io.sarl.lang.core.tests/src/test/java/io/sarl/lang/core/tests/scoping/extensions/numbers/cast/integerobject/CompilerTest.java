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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.integerobject;

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
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Integer) : byte {",
				"    left as byte",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_short() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Integer) : short {",
				"    left as short",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_int() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Integer) : int {",
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
						"  public int fct(final Integer left) {",
						"    return ((int) (left).intValue());",
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
				"  def fct(left : Integer) : long {",
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
						"  public long fct(final Integer left) {",
						"    return ((long) (left).intValue());",
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
				"  def fct(left : Integer) : float {",
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
						"  public float fct(final Integer left) {",
						"    return ((float) (left).intValue());",
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
				"  def fct(left : Integer) : double {",
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
						"  public double fct(final Integer left) {",
						"    return ((double) (left).intValue());",
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
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Integer) : Byte {",
				"    left as Byte",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Short() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Integer) : Short {",
				"    left as Short",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Integer() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Integer) : Integer {",
				"    left as Integer",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public Integer fct(final Integer left) {",
						"    return ((Integer) left);",
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
	public void as_Long() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Integer) : Long {",
				"    left as Long",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Float() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Integer) : Float {",
				"    left as Float",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Double() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Integer) : Double {",
				"    left as Double",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_AtomicInteger() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Integer) : AtomicInteger {",
				"    left as AtomicInteger",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_AtomicLong() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Integer) : AtomicLong {",
				"    left as AtomicLong",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_AtomicDouble() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Integer) : AtomicLong {",
				"    left as AtomicDouble",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Number() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Integer) : Number {",
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
						"  public Number fct(final Integer left) {",
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
