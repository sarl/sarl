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

import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toByte;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toFloat;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toInt;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toLongInteger;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicLongCastExtensions.toShort;

import java.util.concurrent.atomic.AtomicLong;

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
	AtomicLongCastExtensionsTest.Code.class,
	AtomicLongCastExtensionsTest.Compilation.class
})
@SuppressWarnings("all")
public class AtomicLongCastExtensionsTest {

	/** This class tests the implementation of the functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Code extends AbstractSarlTest {

		private static AtomicLong left = new AtomicLong(4);

		@Test
		public void toFloat_AtomicLong() {
			assertEpsilonEquals(4, toFloat(left));
		}

		@Test
		public void toByte_AtomicLong() {
			assertEquals(4, toByte(left));
		}

		@Test
		public void toLong_AtomicLong() {
			assertEquals(4, toLong(left));
		}

		@Test
		public void toDouble_AtomicLong() {
			assertEpsilonEquals(4, toDouble(left));
		}

		@Test
		public void toShort_AtomicLong() {
			assertEquals(4, toShort(left));
		}

		@Test
		public void toInt_AtomicLong() {
			assertEquals(4, toInt(left));
		}

		@Test
		public void toIntegerInteger_AtomicLong() {
			Long value = toLongInteger(left);
			assertNotNull(value);
			assertEquals(4, value.longValue());
		}

	}

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
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : byte {",
					"    left as byte",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to byte");
		}

		@Test
		public void as_short() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : short {",
					"    left as short",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to Short");
		}

		@Test
		public void as_int() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : int {",
					"    left as int",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to int");
		}

		@Test
		public void as_long() throws Exception {
			validate(file(multilineString(
					"class A {",
					"  def fct(left : AtomicLong) : long {",
					"    left as long",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to long");
		}

		@Test
		public void as_float() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : float {",
					"    left as float",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to float");
		}

		@Test
		public void as_double() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : double {",
					"    left as double",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to double");
		}

		@Test
		public void as_Byte() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : Byte {",
					"    left as Byte",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to byte");
		}

		@Test
		public void as_Short() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : Short {",
					"    left as Short",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to Short");
		}

		@Test
		public void as_Integer() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : Integer {",
					"    left as Integer",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to Integer");
		}

		@Test
		public void as_Long() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : Long {",
					"    left as Long",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to Long");
		}

		@Test
		public void as_Float() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : Float {",
					"    left as Float",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to Float");
		}

		@Test
		public void as_Double() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : Double {",
					"    left as Double",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to Double");
		}

		@Test
		public void as_AtomicInteger() throws Exception {
			validate(file(multilineString(
					"import java.util.concurrent.atomic.AtomicInteger",
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : AtomicInteger {",
					"    left as AtomicInteger",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to AtomicInteger");
		}

		@Test
		public void as_AtomicLong() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : AtomicLong {",
					"    left as AtomicLong",
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
					"  public AtomicLong fct(final AtomicLong left) {",
					"    return ((AtomicLong) left);",
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
		public void as_AtomicDouble() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : AtomicLong {",
					"    left as AtomicDouble",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicLong to AtomicDouble");
		}

		@Test
		public void as_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicLong) : Number {",
					"    left as Number",
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
					"  public Number fct(final AtomicLong left) {",
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
