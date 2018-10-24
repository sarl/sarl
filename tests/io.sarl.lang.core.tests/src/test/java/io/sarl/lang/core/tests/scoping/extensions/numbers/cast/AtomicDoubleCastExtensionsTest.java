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

import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toByte;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toDouble;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toDoubleObject;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toFloat;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toInt;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toLong;
import static io.sarl.lang.scoping.extensions.numbers.cast.AtomicDoubleCastExtensions.toShort;

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
	AtomicDoubleCastExtensionsTest.Code.class,
	AtomicDoubleCastExtensionsTest.Compilation.class
})
@SuppressWarnings("all")
public class AtomicDoubleCastExtensionsTest {

	/** This class tests the implementation of the functions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Code extends AbstractSarlTest {

		private static AtomicDouble left = new AtomicDouble(4);

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
			Double value = toDoubleObject(left);
			assertNotNull(value);
			assertEpsilonEquals(4, value.doubleValue());
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
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : byte {",
					"    left as byte",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to byte");
		}

		@Test
		public void as_short() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : short {",
					"    left as short",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to Short");
		}

		@Test
		public void as_int() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : int {",
					"    left as int",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to int");
		}

		@Test
		public void as_long() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : long {",
					"    left as long",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to long");
		}

		@Test
		public void as_float() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : float {",
					"    left as float",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to float");
		}

		@Test
		public void as_double() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : double {",
					"    left as double",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to double");
		}

		@Test
		public void as_Byte() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : Byte {",
					"    left as Byte",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to byte");
		}

		@Test
		public void as_Short() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : Short {",
					"    left as Short",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to Short");
		}

		@Test
		public void as_Integer() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : Integer {",
					"    left as Integer",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to Integer");
		}

		@Test
		public void as_Long() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : Long {",
					"    left as Long",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to Long");
		}

		@Test
		public void as_Float() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : Float {",
					"    left as Float",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to Float");
		}

		@Test
		public void as_Double() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : Double {",
					"    left as Double",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to Double");
		}

		@Test
		public void as_AtomicInteger() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"import java.util.concurrent.atomic.AtomicInteger",
					"class A {",
					"  def fct(left : AtomicDouble) : AtomicInteger {",
					"    left as AtomicInteger",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to AtomicInteger");
		}

		@Test
		public void as_AtomicLong() throws Exception {
			validate(file(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"import java.util.concurrent.atomic.AtomicInteger",
					"import java.util.concurrent.atomic.AtomicLong",
					"class A {",
					"  def fct(left : AtomicDouble) : AtomicLong {",
					"    left as AtomicLong",
					"  }",
					"}"))).assertError(
							TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
							IssueCodes.INVALID_CAST,
							"Cannot cast from AtomicDouble to AtomicLong");
		}

		@Test
		public void as_AtomicDouble() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : AtomicDouble {",
					"    left as AtomicDouble",
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
					"  public AtomicDouble fct(final AtomicDouble left) {",
					"    return ((AtomicDouble) left);",
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
		public void as_Number() throws Exception {
			getCompileHelper().assertCompilesTo(multilineString(
					"import com.google.common.util.concurrent.AtomicDouble",
					"class A {",
					"  def fct(left : AtomicDouble) : Number {",
					"    left as Number",
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
					"  public Number fct(final AtomicDouble left) {",
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
