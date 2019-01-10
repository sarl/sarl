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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.doubleobject;

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
				"  def fct(left : Double) : byte {",
				"    left as byte",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_short() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Double) : short {",
				"    left as short",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_int() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Double) : int {",
				"    left as int",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_long() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Double) : long {",
				"    left as long",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_float() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Double) : float {",
				"    left as float",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_double() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : double {",
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
						"  public double fct(final Double left) {",
						"    return ((double) (left).doubleValue());",
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
				"  def fct(left : Double) : Byte {",
				"    left as Byte",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Short() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Double) : Short {",
				"    left as Short",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Integer() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Double) : Integer {",
				"    left as Integer",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Long() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Double) : Long {",
				"    left as Long",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Float() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"class A {",
				"  def fct(left : Double) : Float {",
				"    left as Float",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Double() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Double {",
				"    left as Double",
				"  }",
				"}"),
				multilineString(
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class A {",
						"  @Pure",
						"  public Double fct(final Double left) {",
						"    return ((Double) left);",
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
	public void as_AtomicInteger() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import java.util.concurrent.atomic.AtomicInteger",
				"class A {",
				"  def fct(left : Double) : AtomicInteger {",
				"    left as AtomicInteger",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_AtomicLong() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import java.util.concurrent.atomic.AtomicLong",
				"class A {",
				"  def fct(left : Double) : AtomicLong {",
				"    left as AtomicLong",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_AtomicDouble() throws Exception {
		diffSingleTypeCompileTo_unexpectedCastError(multilineString(
				"import com.google.common.util.concurrent.AtomicDouble",
				"class A {",
				"  def fct(left : Double) : AtomicLong {",
				"    left as AtomicDouble",
				"  }",
				"}"));
	}

	@DifferedTest
	public void as_Number() throws Exception {
		diffSingleTypeCompileTo(multilineString(
				"class A {",
				"  def fct(left : Double) : Number {",
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
						"  public Number fct(final Double left) {",
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
