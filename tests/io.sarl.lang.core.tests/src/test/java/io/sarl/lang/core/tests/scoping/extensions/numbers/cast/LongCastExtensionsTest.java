/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.google.common.util.concurrent.AtomicDouble;
import com.google.inject.Inject;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@RunWith(Suite.class)
@SuiteClasses({
	LongCastExtensionsTest.Compilation.class
})
@SuppressWarnings("all")
public class LongCastExtensionsTest {

	/** This class tests if the generated Java code corresponds to the inline definition of the functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Compilation extends AbstractSarlTest {

		@Test
		public void as_byte() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : Long) : byte {",
					"    left as byte",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to byte");
		}

		@Test
		public void as_short() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : Long) : short {",
					"    left as short",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to Short");
		}

		@Test
		public void as_int() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : Long) : int {",
					"    left as int",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to int");
		}

		@Test
		public void as_long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Long) : long {",
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
					"  public long fct(final Long left) {",
					"    return ((long) (left).longValue());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_float() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Long) : float {",
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
					"  public float fct(final Long left) {",
					"    return ((float) (left).longValue());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_double() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Long) : double {",
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
					"  public double fct(final Long left) {",
					"    return ((double) (left).longValue());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_Byte() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : Long) : Byte {",
					"    left as Byte",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to byte");
		}

		@Test
		public void as_Short() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : Long) : Short {",
					"    left as Short",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to Short");
		}

		@Test
		public void as_Integer() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : Long) : Integer {",
					"    left as Integer",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to Integer");
		}

		@Test
		public void as_Long() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Long) : Long {",
					"    left as Long",
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
					"  public Long fct(final Long left) {",
					"    return ((Long) left);",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		@Test
		public void as_Float() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : Long) : Float {",
					"    left as Float",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to Float");
		}

		@Test
		public void as_Double() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : Long) : Double {",
					"    left as Double",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to Double");
		}

		@Test
		public void as_AtomicInteger() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : Long) : AtomicInteger {",
					"    left as AtomicInteger",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to AtomicInteger");
		}

		@Test
		public void as_AtomicLong() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : Long) : AtomicLong {",
					"    left as AtomicLong",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to AtomicLong");
		}

		@Test
		public void as_AtomicDouble() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : Long) : AtomicLong {",
					"    left as AtomicDouble",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from Long or long to AtomicDouble");
		}

		@Test
		public void as_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"class A {",
					"  def fct(left : Long) : Number {",
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
					"  public Number fct(final Long left) {",
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

}
