/*
/*
 * Copyright (C) 2014-2018 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.tests.bugs.to00999.bug764;

import static org.junit.Assert.*;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import com.google.common.base.Objects;
import com.google.inject.Inject;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/764"
 */
@SuppressWarnings("all")
public class AtomicLongExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_2028 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong) : long {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_2028() throws Exception {
		validate(file(this.SNIPSET_2028)).assertNoErrors();
	}

	private String SNIPSET_2029 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2029() throws Exception {
		validate(file(this.SNIPSET_2029)).assertNoErrors();
	}

	private String SNIPSET_2030 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2030() throws Exception {
		validate(file(this.SNIPSET_2030)).assertNoErrors();
	}

	private String SNIPSET_2031 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2031() throws Exception {
		validate(file(this.SNIPSET_2031)).assertNoErrors();
	}

	private String SNIPSET_2032 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2032() throws Exception {
		validate(file(this.SNIPSET_2032)).assertNoErrors();
	}

	private String SNIPSET_2033 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2033() throws Exception {
		validate(file(this.SNIPSET_2033)).assertNoErrors();
	}

	private String SNIPSET_2034 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2034() throws Exception {
		validate(file(this.SNIPSET_2034)).assertNoErrors();
	}

	private String SNIPSET_2035 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2035() throws Exception {
		validate(file(this.SNIPSET_2035)).assertNoErrors();
	}

	private String SNIPSET_2036 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2036() throws Exception {
		validate(file(this.SNIPSET_2036)).assertNoErrors();
	}

	private String SNIPSET_2037 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2037() throws Exception {
		validate(file(this.SNIPSET_2037)).assertNoErrors();
	}

	private String SNIPSET_2038 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2038() throws Exception {
		validate(file(this.SNIPSET_2038)).assertNoErrors();
	}

	private String SNIPSET_2039 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2039() throws Exception {
		validate(file(this.SNIPSET_2039)).assertNoErrors();
	}

	private String SNIPSET_2040 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2040() throws Exception {
		validate(file(this.SNIPSET_2040)).assertNoErrors();
	}

	private String SNIPSET_2041 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2041() throws Exception {
		validate(file(this.SNIPSET_2041)).assertNoErrors();
	}

	private String SNIPSET_2042 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2042() throws Exception {
		validate(file(this.SNIPSET_2042)).assertNoErrors();
	}

	private String SNIPSET_2043 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2043() throws Exception {
		validate(file(this.SNIPSET_2043)).assertNoErrors();
	}

	private String SNIPSET_2044 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2044() throws Exception {
		validate(file(this.SNIPSET_2044)).assertNoErrors();
	}

	private String SNIPSET_2045 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2045() throws Exception {
		validate(file(this.SNIPSET_2045)).assertNoErrors();
	}

	private String SNIPSET_2046 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2046() throws Exception {
		validate(file(this.SNIPSET_2046)).assertNoErrors();
	}

	private String SNIPSET_2047 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2047() throws Exception {
		validate(file(this.SNIPSET_2047)).assertNoErrors();
	}

	private String SNIPSET_2048 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2048() throws Exception {
		validate(file(this.SNIPSET_2048)).assertNoErrors();
	}

	private String SNIPSET_2049 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2049() throws Exception {
		validate(file(this.SNIPSET_2049)).assertNoErrors();
	}

	private String SNIPSET_2050 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2050() throws Exception {
		validate(file(this.SNIPSET_2050)).assertNoErrors();
	}

	private String SNIPSET_2051 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2051() throws Exception {
		validate(file(this.SNIPSET_2051)).assertNoErrors();
	}

	private String SNIPSET_2052 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2052() throws Exception {
		validate(file(this.SNIPSET_2052)).assertNoErrors();
	}

	private String SNIPSET_2053 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2053() throws Exception {
		validate(file(this.SNIPSET_2053)).assertNoErrors();
	}

	private String SNIPSET_2054 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2054() throws Exception {
		validate(file(this.SNIPSET_2054)).assertNoErrors();
	}

	private String SNIPSET_2055 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2055() throws Exception {
		validate(file(this.SNIPSET_2055)).assertNoErrors();
	}

	private String SNIPSET_2056 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2056() throws Exception {
		validate(file(this.SNIPSET_2056)).assertNoErrors();
	}

	private String SNIPSET_2057 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2057() throws Exception {
		validate(file(this.SNIPSET_2057)).assertNoErrors();
	}

	private String SNIPSET_2058 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2058() throws Exception {
		validate(file(this.SNIPSET_2058)).assertNoErrors();
	}

	private String SNIPSET_2059 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2059() throws Exception {
		validate(file(this.SNIPSET_2059)).assertNoErrors();
	}

	private String SNIPSET_2060 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2060() throws Exception {
		validate(file(this.SNIPSET_2060)).assertNoErrors();
	}

	private String SNIPSET_2061 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2061() throws Exception {
		validate(file(this.SNIPSET_2061)).assertNoErrors();
	}

	private String SNIPSET_2062 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2062() throws Exception {
		validate(file(this.SNIPSET_2062)).assertNoErrors();
	}

	private String SNIPSET_2063 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2063() throws Exception {
		validate(file(this.SNIPSET_2063)).assertNoErrors();
	}

	private String SNIPSET_2064 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2064() throws Exception {
		validate(file(this.SNIPSET_2064)).assertNoErrors();
	}

	private String SNIPSET_2065 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2065() throws Exception {
		validate(file(this.SNIPSET_2065)).assertNoErrors();
	}

	private String SNIPSET_2066 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2066() throws Exception {
		validate(file(this.SNIPSET_2066)).assertNoErrors();
	}

	private String SNIPSET_2067 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2067() throws Exception {
		validate(file(this.SNIPSET_2067)).assertNoErrors();
	}

	private String SNIPSET_2068 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2068() throws Exception {
		validate(file(this.SNIPSET_2068)).assertNoErrors();
	}

	private String SNIPSET_2069 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2069() throws Exception {
		validate(file(this.SNIPSET_2069)).assertNoErrors();
	}

	private String SNIPSET_2070 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2070() throws Exception {
		validate(file(this.SNIPSET_2070)).assertNoErrors();
	}

	private String SNIPSET_2071 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2071() throws Exception {
		validate(file(this.SNIPSET_2071)).assertNoErrors();
	}

	private String SNIPSET_2072 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2072() throws Exception {
		validate(file(this.SNIPSET_2072)).assertNoErrors();
	}

	private String SNIPSET_2073 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2073() throws Exception {
		validate(file(this.SNIPSET_2073)).assertNoErrors();
	}

	private String SNIPSET_2074 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2074() throws Exception {
		validate(file(this.SNIPSET_2074)).assertNoErrors();
	}

	private String SNIPSET_2075 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2075() throws Exception {
		validate(file(this.SNIPSET_2075)).assertNoErrors();
	}

	private String SNIPSET_2076 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2076() throws Exception {
		validate(file(this.SNIPSET_2076)).assertNoErrors();
	}

	private String SNIPSET_2077 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2077() throws Exception {
		validate(file(this.SNIPSET_2077)).assertNoErrors();
	}

	private String SNIPSET_2078 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2078() throws Exception {
		validate(file(this.SNIPSET_2078)).assertNoErrors();
	}

	private String SNIPSET_2079 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2079() throws Exception {
		validate(file(this.SNIPSET_2079)).assertNoErrors();
	}

	private String SNIPSET_2080 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2080() throws Exception {
		validate(file(this.SNIPSET_2080)).assertNoErrors();
	}

	private String SNIPSET_2081 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2081() throws Exception {
		validate(file(this.SNIPSET_2081)).assertNoErrors();
	}

	private String SNIPSET_2082 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2082() throws Exception {
		validate(file(this.SNIPSET_2082)).assertNoErrors();
	}

	private String SNIPSET_2083 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2083() throws Exception {
		validate(file(this.SNIPSET_2083)).assertNoErrors();
	}

	private String SNIPSET_2084 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2084() throws Exception {
		validate(file(this.SNIPSET_2084)).assertNoErrors();
	}

	private String SNIPSET_2085 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2085() throws Exception {
		validate(file(this.SNIPSET_2085)).assertNoErrors();
	}

	private String SNIPSET_2086 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2086() throws Exception {
		validate(file(this.SNIPSET_2086)).assertNoErrors();
	}

	private String SNIPSET_2087 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2087() throws Exception {
		validate(file(this.SNIPSET_2087)).assertNoErrors();
	}

	private String SNIPSET_2088 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2088() throws Exception {
		validate(file(this.SNIPSET_2088)).assertNoErrors();
	}

	private String SNIPSET_2089 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2089() throws Exception {
		validate(file(this.SNIPSET_2089)).assertNoErrors();
	}

	private String SNIPSET_2090 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2090() throws Exception {
		validate(file(this.SNIPSET_2090)).assertNoErrors();
	}

	private String SNIPSET_2091 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2091() throws Exception {
		validate(file(this.SNIPSET_2091)).assertNoErrors();
	}

	private String SNIPSET_2092 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2092() throws Exception {
		validate(file(this.SNIPSET_2092)).assertNoErrors();
	}

	private String SNIPSET_2093 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2093() throws Exception {
		validate(file(this.SNIPSET_2093)).assertNoErrors();
	}

	private String SNIPSET_2094 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2094() throws Exception {
		validate(file(this.SNIPSET_2094)).assertNoErrors();
	}

	private String SNIPSET_2095 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2095() throws Exception {
		validate(file(this.SNIPSET_2095)).assertNoErrors();
	}

	private String SNIPSET_2096 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2096() throws Exception {
		validate(file(this.SNIPSET_2096)).assertNoErrors();
	}

	private String SNIPSET_2097 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2097() throws Exception {
		validate(file(this.SNIPSET_2097)).assertNoErrors();
	}

	private String SNIPSET_2098 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2098() throws Exception {
		validate(file(this.SNIPSET_2098)).assertNoErrors();
	}

	private String SNIPSET_2099 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2099() throws Exception {
		validate(file(this.SNIPSET_2099)).assertNoErrors();
	}

	private String SNIPSET_2100 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2100() throws Exception {
		validate(file(this.SNIPSET_2100)).assertNoErrors();
	}

	private String SNIPSET_2101 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2101() throws Exception {
		validate(file(this.SNIPSET_2101)).assertNoErrors();
	}

	private String SNIPSET_2102 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2102() throws Exception {
		validate(file(this.SNIPSET_2102)).assertNoErrors();
	}

	private String SNIPSET_2103 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2103() throws Exception {
		validate(file(this.SNIPSET_2103)).assertNoErrors();
	}

	private String SNIPSET_2104 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2104() throws Exception {
		validate(file(this.SNIPSET_2104)).assertNoErrors();
	}

	private String SNIPSET_2105 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2105() throws Exception {
		validate(file(this.SNIPSET_2105)).assertNoErrors();
	}

	private String SNIPSET_2106 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2106() throws Exception {
		validate(file(this.SNIPSET_2106)).assertNoErrors();
	}

	private String SNIPSET_2107 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2107() throws Exception {
		validate(file(this.SNIPSET_2107)).assertNoErrors();
	}

	private String SNIPSET_2108 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2108() throws Exception {
		validate(file(this.SNIPSET_2108)).assertNoErrors();
	}

	private String SNIPSET_2109 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2109() throws Exception {
		validate(file(this.SNIPSET_2109)).assertNoErrors();
	}

	private String SNIPSET_2110 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2110() throws Exception {
		validate(file(this.SNIPSET_2110)).assertNoErrors();
	}

	private String SNIPSET_2111 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2111() throws Exception {
		validate(file(this.SNIPSET_2111)).assertNoErrors();
	}

	private String SNIPSET_2112 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2112() throws Exception {
		validate(file(this.SNIPSET_2112)).assertNoErrors();
	}

	private String SNIPSET_2113 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2113() throws Exception {
		validate(file(this.SNIPSET_2113)).assertNoErrors();
	}

	private String SNIPSET_2114 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2114() throws Exception {
		validate(file(this.SNIPSET_2114)).assertNoErrors();
	}

	private String SNIPSET_2115 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2115() throws Exception {
		validate(file(this.SNIPSET_2115)).assertNoErrors();
	}

	private String SNIPSET_2116 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2116() throws Exception {
		validate(file(this.SNIPSET_2116)).assertNoErrors();
	}

	private String SNIPSET_2117 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2117() throws Exception {
		validate(file(this.SNIPSET_2117)).assertNoErrors();
	}

	private String SNIPSET_2118 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2118() throws Exception {
		validate(file(this.SNIPSET_2118)).assertNoErrors();
	}

	private String SNIPSET_2119 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2119() throws Exception {
		validate(file(this.SNIPSET_2119)).assertNoErrors();
	}

	private String SNIPSET_2120 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2120() throws Exception {
		validate(file(this.SNIPSET_2120)).assertNoErrors();
	}

	private String SNIPSET_2121 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2121() throws Exception {
		validate(file(this.SNIPSET_2121)).assertNoErrors();
	}

	private String SNIPSET_2122 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2122() throws Exception {
		validate(file(this.SNIPSET_2122)).assertNoErrors();
	}

	private String SNIPSET_2123 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2123() throws Exception {
		validate(file(this.SNIPSET_2123)).assertNoErrors();
	}

	private String SNIPSET_2124 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2124() throws Exception {
		validate(file(this.SNIPSET_2124)).assertNoErrors();
	}

	private String SNIPSET_2125 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2125() throws Exception {
		validate(file(this.SNIPSET_2125)).assertNoErrors();
	}

	private String SNIPSET_2126 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2126() throws Exception {
		validate(file(this.SNIPSET_2126)).assertNoErrors();
	}

	private String SNIPSET_2127 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2127() throws Exception {
		validate(file(this.SNIPSET_2127)).assertNoErrors();
	}

	private String SNIPSET_2128 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2128() throws Exception {
		validate(file(this.SNIPSET_2128)).assertNoErrors();
	}

	private String SNIPSET_2129 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2129() throws Exception {
		validate(file(this.SNIPSET_2129)).assertNoErrors();
	}

	private String SNIPSET_2130 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2130() throws Exception {
		validate(file(this.SNIPSET_2130)).assertNoErrors();
	}

	private String SNIPSET_2131 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2131() throws Exception {
		validate(file(this.SNIPSET_2131)).assertNoErrors();
	}

	private String SNIPSET_2132 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2132() throws Exception {
		validate(file(this.SNIPSET_2132)).assertNoErrors();
	}

	private String SNIPSET_2133 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2133() throws Exception {
		validate(file(this.SNIPSET_2133)).assertNoErrors();
	}

	private String SNIPSET_2134 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2134() throws Exception {
		validate(file(this.SNIPSET_2134)).assertNoErrors();
	}

	private String SNIPSET_2135 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2135() throws Exception {
		validate(file(this.SNIPSET_2135)).assertNoErrors();
	}

	private String SNIPSET_2136 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2136() throws Exception {
		validate(file(this.SNIPSET_2136)).assertNoErrors();
	}

	private String SNIPSET_2137 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2137() throws Exception {
		validate(file(this.SNIPSET_2137)).assertNoErrors();
	}

	private String SNIPSET_2138 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2138() throws Exception {
		validate(file(this.SNIPSET_2138)).assertNoErrors();
	}

	private String SNIPSET_2139 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2139() throws Exception {
		validate(file(this.SNIPSET_2139)).assertNoErrors();
	}

	private String SNIPSET_2140 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2140() throws Exception {
		validate(file(this.SNIPSET_2140)).assertNoErrors();
	}

	private String SNIPSET_2141 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2141() throws Exception {
		validate(file(this.SNIPSET_2141)).assertNoErrors();
	}

	private String SNIPSET_2142 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2142() throws Exception {
		validate(file(this.SNIPSET_2142)).assertNoErrors();
	}

	private String SNIPSET_2143 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2143() throws Exception {
		validate(file(this.SNIPSET_2143)).assertNoErrors();
	}

	private String SNIPSET_2144 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2144() throws Exception {
		validate(file(this.SNIPSET_2144)).assertNoErrors();
	}

	private String SNIPSET_2145 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2145() throws Exception {
		validate(file(this.SNIPSET_2145)).assertNoErrors();
	}

	private String SNIPSET_2146 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2146() throws Exception {
		validate(file(this.SNIPSET_2146)).assertNoErrors();
	}

	private String SNIPSET_2147 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2147() throws Exception {
		validate(file(this.SNIPSET_2147)).assertNoErrors();
	}

	private String SNIPSET_2148 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2148() throws Exception {
		validate(file(this.SNIPSET_2148)).assertNoErrors();
	}

	private String SNIPSET_2149 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2149() throws Exception {
		validate(file(this.SNIPSET_2149)).assertNoErrors();
	}

	private String SNIPSET_2150 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2150() throws Exception {
		validate(file(this.SNIPSET_2150)).assertNoErrors();
	}

	private String SNIPSET_2151 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2151() throws Exception {
		validate(file(this.SNIPSET_2151)).assertNoErrors();
	}

	private String SNIPSET_2152 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2152() throws Exception {
		validate(file(this.SNIPSET_2152)).assertNoErrors();
	}

	private String SNIPSET_2153 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2153() throws Exception {
		validate(file(this.SNIPSET_2153)).assertNoErrors();
	}

	private String SNIPSET_2154 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2154() throws Exception {
		validate(file(this.SNIPSET_2154)).assertNoErrors();
	}

	private String SNIPSET_2155 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2155() throws Exception {
		validate(file(this.SNIPSET_2155)).assertNoErrors();
	}

	private String SNIPSET_2156 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2156() throws Exception {
		validate(file(this.SNIPSET_2156)).assertNoErrors();
	}

	private String SNIPSET_2157 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2157() throws Exception {
		validate(file(this.SNIPSET_2157)).assertNoErrors();
	}

	private String SNIPSET_2158 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2158() throws Exception {
		validate(file(this.SNIPSET_2158)).assertNoErrors();
	}

	private String SNIPSET_2159 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2159() throws Exception {
		validate(file(this.SNIPSET_2159)).assertNoErrors();
	}

	private String SNIPSET_2160 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2160() throws Exception {
		validate(file(this.SNIPSET_2160)).assertNoErrors();
	}

	private String SNIPSET_2161 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2161() throws Exception {
		validate(file(this.SNIPSET_2161)).assertNoErrors();
	}

	private String SNIPSET_2162 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2162() throws Exception {
		validate(file(this.SNIPSET_2162)).assertNoErrors();
	}

	private String SNIPSET_2163 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2163() throws Exception {
		validate(file(this.SNIPSET_2163)).assertNoErrors();
	}

	private String SNIPSET_2164 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2164() throws Exception {
		validate(file(this.SNIPSET_2164)).assertNoErrors();
	}

	private String SNIPSET_2165 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2165() throws Exception {
		validate(file(this.SNIPSET_2165)).assertNoErrors();
	}

	private String SNIPSET_2166 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2166() throws Exception {
		validate(file(this.SNIPSET_2166)).assertNoErrors();
	}

	private String SNIPSET_2167 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2167() throws Exception {
		validate(file(this.SNIPSET_2167)).assertNoErrors();
	}

	private String SNIPSET_2168 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2168() throws Exception {
		validate(file(this.SNIPSET_2168)).assertNoErrors();
	}

	private String SNIPSET_2169 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2169() throws Exception {
		validate(file(this.SNIPSET_2169)).assertNoErrors();
	}

	private String SNIPSET_2170 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2170() throws Exception {
		validate(file(this.SNIPSET_2170)).assertNoErrors();
	}

	private String SNIPSET_2171 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2171() throws Exception {
		validate(file(this.SNIPSET_2171)).assertNoErrors();
	}

	private String SNIPSET_2172 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2172() throws Exception {
		validate(file(this.SNIPSET_2172)).assertNoErrors();
	}

	private String SNIPSET_2173 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2173() throws Exception {
		validate(file(this.SNIPSET_2173)).assertNoErrors();
	}

	private String SNIPSET_2174 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2174() throws Exception {
		validate(file(this.SNIPSET_2174)).assertNoErrors();
	}

	private String SNIPSET_2175 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2175() throws Exception {
		validate(file(this.SNIPSET_2175)).assertNoErrors();
	}

	private String SNIPSET_2176 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2176() throws Exception {
		validate(file(this.SNIPSET_2176)).assertNoErrors();
	}

	private String SNIPSET_2177 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2177() throws Exception {
		validate(file(this.SNIPSET_2177)).assertNoErrors();
	}

	private String SNIPSET_2178 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2178() throws Exception {
		validate(file(this.SNIPSET_2178)).assertNoErrors();
	}

	private String SNIPSET_2179 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2179() throws Exception {
		validate(file(this.SNIPSET_2179)).assertNoErrors();
	}

	private String SNIPSET_2180 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2180() throws Exception {
		validate(file(this.SNIPSET_2180)).assertNoErrors();
	}

	private String SNIPSET_2181 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2181() throws Exception {
		validate(file(this.SNIPSET_2181)).assertNoErrors();
	}

	private String SNIPSET_2182 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2182() throws Exception {
		validate(file(this.SNIPSET_2182)).assertNoErrors();
	}

	private String SNIPSET_2183 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : byte) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2183() throws Exception {
		validate(file(this.SNIPSET_2183)).assertNoErrors();
	}

	private String SNIPSET_2184 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2184() throws Exception {
		validate(file(this.SNIPSET_2184)).assertNoErrors();
	}

	private String SNIPSET_2185 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2185() throws Exception {
		validate(file(this.SNIPSET_2185)).assertNoErrors();
	}

	private String SNIPSET_2186 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : short) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2186() throws Exception {
		validate(file(this.SNIPSET_2186)).assertNoErrors();
	}

	private String SNIPSET_2187 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : int) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2187() throws Exception {
		validate(file(this.SNIPSET_2187)).assertNoErrors();
	}

	private String SNIPSET_2188 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2188() throws Exception {
		validate(file(this.SNIPSET_2188)).assertNoErrors();
	}

	private String SNIPSET_2189 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2189() throws Exception {
		validate(file(this.SNIPSET_2189)).assertNoErrors();
	}

	private String SNIPSET_2190 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2190() throws Exception {
		validate(file(this.SNIPSET_2190)).assertNoErrors();
	}

	private String SNIPSET_2191 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Integer) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2191() throws Exception {
		validate(file(this.SNIPSET_2191)).assertNoErrors();
	}

	private String SNIPSET_2192 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2192() throws Exception {
		validate(file(this.SNIPSET_2192)).assertNoErrors();
	}

	private String SNIPSET_2193 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Byte) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2193() throws Exception {
		validate(file(this.SNIPSET_2193)).assertNoErrors();
	}

	private String SNIPSET_2194 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : Short) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2194() throws Exception {
		validate(file(this.SNIPSET_2194)).assertNoErrors();
	}

	private String SNIPSET_2195 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2195() throws Exception {
		validate(file(this.SNIPSET_2195)).assertNoErrors();
	}

	private String SNIPSET_2196 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicLong, b : AtomicInteger) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2196() throws Exception {
		validate(file(this.SNIPSET_2196)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
