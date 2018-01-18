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
public class AtomicIntegerExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_2197 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger) : int {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_2197() throws Exception {
		validate(file(this.SNIPSET_2197)).assertNoErrors();
	}

	private String SNIPSET_2198 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2198() throws Exception {
		validate(file(this.SNIPSET_2198)).assertNoErrors();
	}

	private String SNIPSET_2199 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2199() throws Exception {
		validate(file(this.SNIPSET_2199)).assertNoErrors();
	}

	private String SNIPSET_2200 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2200() throws Exception {
		validate(file(this.SNIPSET_2200)).assertNoErrors();
	}

	private String SNIPSET_2201 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2201() throws Exception {
		validate(file(this.SNIPSET_2201)).assertNoErrors();
	}

	private String SNIPSET_2202 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2202() throws Exception {
		validate(file(this.SNIPSET_2202)).assertNoErrors();
	}

	private String SNIPSET_2203 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2203() throws Exception {
		validate(file(this.SNIPSET_2203)).assertNoErrors();
	}

	private String SNIPSET_2204 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2204() throws Exception {
		validate(file(this.SNIPSET_2204)).assertNoErrors();
	}

	private String SNIPSET_2205 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2205() throws Exception {
		validate(file(this.SNIPSET_2205)).assertNoErrors();
	}

	private String SNIPSET_2206 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2206() throws Exception {
		validate(file(this.SNIPSET_2206)).assertNoErrors();
	}

	private String SNIPSET_2207 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2207() throws Exception {
		validate(file(this.SNIPSET_2207)).assertNoErrors();
	}

	private String SNIPSET_2208 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2208() throws Exception {
		validate(file(this.SNIPSET_2208)).assertNoErrors();
	}

	private String SNIPSET_2209 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2209() throws Exception {
		validate(file(this.SNIPSET_2209)).assertNoErrors();
	}

	private String SNIPSET_2210 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2210() throws Exception {
		validate(file(this.SNIPSET_2210)).assertNoErrors();
	}

	private String SNIPSET_2211 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2211() throws Exception {
		validate(file(this.SNIPSET_2211)).assertNoErrors();
	}

	private String SNIPSET_2212 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2212() throws Exception {
		validate(file(this.SNIPSET_2212)).assertNoErrors();
	}

	private String SNIPSET_2213 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2213() throws Exception {
		validate(file(this.SNIPSET_2213)).assertNoErrors();
	}

	private String SNIPSET_2214 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2214() throws Exception {
		validate(file(this.SNIPSET_2214)).assertNoErrors();
	}

	private String SNIPSET_2215 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2215() throws Exception {
		validate(file(this.SNIPSET_2215)).assertNoErrors();
	}

	private String SNIPSET_2216 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2216() throws Exception {
		validate(file(this.SNIPSET_2216)).assertNoErrors();
	}

	private String SNIPSET_2217 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2217() throws Exception {
		validate(file(this.SNIPSET_2217)).assertNoErrors();
	}

	private String SNIPSET_2218 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2218() throws Exception {
		validate(file(this.SNIPSET_2218)).assertNoErrors();
	}

	private String SNIPSET_2219 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2219() throws Exception {
		validate(file(this.SNIPSET_2219)).assertNoErrors();
	}

	private String SNIPSET_2220 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2220() throws Exception {
		validate(file(this.SNIPSET_2220)).assertNoErrors();
	}

	private String SNIPSET_2221 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2221() throws Exception {
		validate(file(this.SNIPSET_2221)).assertNoErrors();
	}

	private String SNIPSET_2222 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2222() throws Exception {
		validate(file(this.SNIPSET_2222)).assertNoErrors();
	}

	private String SNIPSET_2223 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2223() throws Exception {
		validate(file(this.SNIPSET_2223)).assertNoErrors();
	}

	private String SNIPSET_2224 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2224() throws Exception {
		validate(file(this.SNIPSET_2224)).assertNoErrors();
	}

	private String SNIPSET_2225 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_2225() throws Exception {
		validate(file(this.SNIPSET_2225)).assertNoErrors();
	}

	private String SNIPSET_2226 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2226() throws Exception {
		validate(file(this.SNIPSET_2226)).assertNoErrors();
	}

	private String SNIPSET_2227 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2227() throws Exception {
		validate(file(this.SNIPSET_2227)).assertNoErrors();
	}

	private String SNIPSET_2228 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2228() throws Exception {
		validate(file(this.SNIPSET_2228)).assertNoErrors();
	}

	private String SNIPSET_2229 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2229() throws Exception {
		validate(file(this.SNIPSET_2229)).assertNoErrors();
	}

	private String SNIPSET_2230 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2230() throws Exception {
		validate(file(this.SNIPSET_2230)).assertNoErrors();
	}

	private String SNIPSET_2231 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2231() throws Exception {
		validate(file(this.SNIPSET_2231)).assertNoErrors();
	}

	private String SNIPSET_2232 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2232() throws Exception {
		validate(file(this.SNIPSET_2232)).assertNoErrors();
	}

	private String SNIPSET_2233 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2233() throws Exception {
		validate(file(this.SNIPSET_2233)).assertNoErrors();
	}

	private String SNIPSET_2234 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2234() throws Exception {
		validate(file(this.SNIPSET_2234)).assertNoErrors();
	}

	private String SNIPSET_2235 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2235() throws Exception {
		validate(file(this.SNIPSET_2235)).assertNoErrors();
	}

	private String SNIPSET_2236 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2236() throws Exception {
		validate(file(this.SNIPSET_2236)).assertNoErrors();
	}

	private String SNIPSET_2237 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2237() throws Exception {
		validate(file(this.SNIPSET_2237)).assertNoErrors();
	}

	private String SNIPSET_2238 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2238() throws Exception {
		validate(file(this.SNIPSET_2238)).assertNoErrors();
	}

	private String SNIPSET_2239 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_2239() throws Exception {
		validate(file(this.SNIPSET_2239)).assertNoErrors();
	}

	private String SNIPSET_2240 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2240() throws Exception {
		validate(file(this.SNIPSET_2240)).assertNoErrors();
	}

	private String SNIPSET_2241 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2241() throws Exception {
		validate(file(this.SNIPSET_2241)).assertNoErrors();
	}

	private String SNIPSET_2242 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2242() throws Exception {
		validate(file(this.SNIPSET_2242)).assertNoErrors();
	}

	private String SNIPSET_2243 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2243() throws Exception {
		validate(file(this.SNIPSET_2243)).assertNoErrors();
	}

	private String SNIPSET_2244 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2244() throws Exception {
		validate(file(this.SNIPSET_2244)).assertNoErrors();
	}

	private String SNIPSET_2245 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2245() throws Exception {
		validate(file(this.SNIPSET_2245)).assertNoErrors();
	}

	private String SNIPSET_2246 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2246() throws Exception {
		validate(file(this.SNIPSET_2246)).assertNoErrors();
	}

	private String SNIPSET_2247 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2247() throws Exception {
		validate(file(this.SNIPSET_2247)).assertNoErrors();
	}

	private String SNIPSET_2248 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2248() throws Exception {
		validate(file(this.SNIPSET_2248)).assertNoErrors();
	}

	private String SNIPSET_2249 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2249() throws Exception {
		validate(file(this.SNIPSET_2249)).assertNoErrors();
	}

	private String SNIPSET_2250 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2250() throws Exception {
		validate(file(this.SNIPSET_2250)).assertNoErrors();
	}

	private String SNIPSET_2251 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2251() throws Exception {
		validate(file(this.SNIPSET_2251)).assertNoErrors();
	}

	private String SNIPSET_2252 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2252() throws Exception {
		validate(file(this.SNIPSET_2252)).assertNoErrors();
	}

	private String SNIPSET_2253 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_2253() throws Exception {
		validate(file(this.SNIPSET_2253)).assertNoErrors();
	}

	private String SNIPSET_2254 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2254() throws Exception {
		validate(file(this.SNIPSET_2254)).assertNoErrors();
	}

	private String SNIPSET_2255 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2255() throws Exception {
		validate(file(this.SNIPSET_2255)).assertNoErrors();
	}

	private String SNIPSET_2256 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2256() throws Exception {
		validate(file(this.SNIPSET_2256)).assertNoErrors();
	}

	private String SNIPSET_2257 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2257() throws Exception {
		validate(file(this.SNIPSET_2257)).assertNoErrors();
	}

	private String SNIPSET_2258 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2258() throws Exception {
		validate(file(this.SNIPSET_2258)).assertNoErrors();
	}

	private String SNIPSET_2259 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2259() throws Exception {
		validate(file(this.SNIPSET_2259)).assertNoErrors();
	}

	private String SNIPSET_2260 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2260() throws Exception {
		validate(file(this.SNIPSET_2260)).assertNoErrors();
	}

	private String SNIPSET_2261 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2261() throws Exception {
		validate(file(this.SNIPSET_2261)).assertNoErrors();
	}

	private String SNIPSET_2262 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2262() throws Exception {
		validate(file(this.SNIPSET_2262)).assertNoErrors();
	}

	private String SNIPSET_2263 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2263() throws Exception {
		validate(file(this.SNIPSET_2263)).assertNoErrors();
	}

	private String SNIPSET_2264 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2264() throws Exception {
		validate(file(this.SNIPSET_2264)).assertNoErrors();
	}

	private String SNIPSET_2265 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2265() throws Exception {
		validate(file(this.SNIPSET_2265)).assertNoErrors();
	}

	private String SNIPSET_2266 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2266() throws Exception {
		validate(file(this.SNIPSET_2266)).assertNoErrors();
	}

	private String SNIPSET_2267 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_2267() throws Exception {
		validate(file(this.SNIPSET_2267)).assertNoErrors();
	}

	private String SNIPSET_2268 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2268() throws Exception {
		validate(file(this.SNIPSET_2268)).assertNoErrors();
	}

	private String SNIPSET_2269 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2269() throws Exception {
		validate(file(this.SNIPSET_2269)).assertNoErrors();
	}

	private String SNIPSET_2270 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2270() throws Exception {
		validate(file(this.SNIPSET_2270)).assertNoErrors();
	}

	private String SNIPSET_2271 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2271() throws Exception {
		validate(file(this.SNIPSET_2271)).assertNoErrors();
	}

	private String SNIPSET_2272 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2272() throws Exception {
		validate(file(this.SNIPSET_2272)).assertNoErrors();
	}

	private String SNIPSET_2273 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2273() throws Exception {
		validate(file(this.SNIPSET_2273)).assertNoErrors();
	}

	private String SNIPSET_2274 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2274() throws Exception {
		validate(file(this.SNIPSET_2274)).assertNoErrors();
	}

	private String SNIPSET_2275 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2275() throws Exception {
		validate(file(this.SNIPSET_2275)).assertNoErrors();
	}

	private String SNIPSET_2276 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2276() throws Exception {
		validate(file(this.SNIPSET_2276)).assertNoErrors();
	}

	private String SNIPSET_2277 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2277() throws Exception {
		validate(file(this.SNIPSET_2277)).assertNoErrors();
	}

	private String SNIPSET_2278 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2278() throws Exception {
		validate(file(this.SNIPSET_2278)).assertNoErrors();
	}

	private String SNIPSET_2279 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2279() throws Exception {
		validate(file(this.SNIPSET_2279)).assertNoErrors();
	}

	private String SNIPSET_2280 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2280() throws Exception {
		validate(file(this.SNIPSET_2280)).assertNoErrors();
	}

	private String SNIPSET_2281 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_2281() throws Exception {
		validate(file(this.SNIPSET_2281)).assertNoErrors();
	}

	private String SNIPSET_2282 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2282() throws Exception {
		validate(file(this.SNIPSET_2282)).assertNoErrors();
	}

	private String SNIPSET_2283 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2283() throws Exception {
		validate(file(this.SNIPSET_2283)).assertNoErrors();
	}

	private String SNIPSET_2284 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2284() throws Exception {
		validate(file(this.SNIPSET_2284)).assertNoErrors();
	}

	private String SNIPSET_2285 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2285() throws Exception {
		validate(file(this.SNIPSET_2285)).assertNoErrors();
	}

	private String SNIPSET_2286 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2286() throws Exception {
		validate(file(this.SNIPSET_2286)).assertNoErrors();
	}

	private String SNIPSET_2287 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2287() throws Exception {
		validate(file(this.SNIPSET_2287)).assertNoErrors();
	}

	private String SNIPSET_2288 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2288() throws Exception {
		validate(file(this.SNIPSET_2288)).assertNoErrors();
	}

	private String SNIPSET_2289 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2289() throws Exception {
		validate(file(this.SNIPSET_2289)).assertNoErrors();
	}

	private String SNIPSET_2290 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2290() throws Exception {
		validate(file(this.SNIPSET_2290)).assertNoErrors();
	}

	private String SNIPSET_2291 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2291() throws Exception {
		validate(file(this.SNIPSET_2291)).assertNoErrors();
	}

	private String SNIPSET_2292 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2292() throws Exception {
		validate(file(this.SNIPSET_2292)).assertNoErrors();
	}

	private String SNIPSET_2293 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2293() throws Exception {
		validate(file(this.SNIPSET_2293)).assertNoErrors();
	}

	private String SNIPSET_2294 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2294() throws Exception {
		validate(file(this.SNIPSET_2294)).assertNoErrors();
	}

	private String SNIPSET_2295 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_2295() throws Exception {
		validate(file(this.SNIPSET_2295)).assertNoErrors();
	}

	private String SNIPSET_2296 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2296() throws Exception {
		validate(file(this.SNIPSET_2296)).assertNoErrors();
	}

	private String SNIPSET_2297 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2297() throws Exception {
		validate(file(this.SNIPSET_2297)).assertNoErrors();
	}

	private String SNIPSET_2298 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2298() throws Exception {
		validate(file(this.SNIPSET_2298)).assertNoErrors();
	}

	private String SNIPSET_2299 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2299() throws Exception {
		validate(file(this.SNIPSET_2299)).assertNoErrors();
	}

	private String SNIPSET_2300 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2300() throws Exception {
		validate(file(this.SNIPSET_2300)).assertNoErrors();
	}

	private String SNIPSET_2301 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2301() throws Exception {
		validate(file(this.SNIPSET_2301)).assertNoErrors();
	}

	private String SNIPSET_2302 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2302() throws Exception {
		validate(file(this.SNIPSET_2302)).assertNoErrors();
	}

	private String SNIPSET_2303 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2303() throws Exception {
		validate(file(this.SNIPSET_2303)).assertNoErrors();
	}

	private String SNIPSET_2304 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2304() throws Exception {
		validate(file(this.SNIPSET_2304)).assertNoErrors();
	}

	private String SNIPSET_2305 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2305() throws Exception {
		validate(file(this.SNIPSET_2305)).assertNoErrors();
	}

	private String SNIPSET_2306 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2306() throws Exception {
		validate(file(this.SNIPSET_2306)).assertNoErrors();
	}

	private String SNIPSET_2307 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2307() throws Exception {
		validate(file(this.SNIPSET_2307)).assertNoErrors();
	}

	private String SNIPSET_2308 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2308() throws Exception {
		validate(file(this.SNIPSET_2308)).assertNoErrors();
	}

	private String SNIPSET_2309 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_2309() throws Exception {
		validate(file(this.SNIPSET_2309)).assertNoErrors();
	}

	private String SNIPSET_2310 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2310() throws Exception {
		validate(file(this.SNIPSET_2310)).assertNoErrors();
	}

	private String SNIPSET_2311 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2311() throws Exception {
		validate(file(this.SNIPSET_2311)).assertNoErrors();
	}

	private String SNIPSET_2312 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2312() throws Exception {
		validate(file(this.SNIPSET_2312)).assertNoErrors();
	}

	private String SNIPSET_2313 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2313() throws Exception {
		validate(file(this.SNIPSET_2313)).assertNoErrors();
	}

	private String SNIPSET_2314 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2314() throws Exception {
		validate(file(this.SNIPSET_2314)).assertNoErrors();
	}

	private String SNIPSET_2315 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2315() throws Exception {
		validate(file(this.SNIPSET_2315)).assertNoErrors();
	}

	private String SNIPSET_2316 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2316() throws Exception {
		validate(file(this.SNIPSET_2316)).assertNoErrors();
	}

	private String SNIPSET_2317 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2317() throws Exception {
		validate(file(this.SNIPSET_2317)).assertNoErrors();
	}

	private String SNIPSET_2318 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2318() throws Exception {
		validate(file(this.SNIPSET_2318)).assertNoErrors();
	}

	private String SNIPSET_2319 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2319() throws Exception {
		validate(file(this.SNIPSET_2319)).assertNoErrors();
	}

	private String SNIPSET_2320 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2320() throws Exception {
		validate(file(this.SNIPSET_2320)).assertNoErrors();
	}

	private String SNIPSET_2321 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2321() throws Exception {
		validate(file(this.SNIPSET_2321)).assertNoErrors();
	}

	private String SNIPSET_2322 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2322() throws Exception {
		validate(file(this.SNIPSET_2322)).assertNoErrors();
	}

	private String SNIPSET_2323 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_2323() throws Exception {
		validate(file(this.SNIPSET_2323)).assertNoErrors();
	}

	private String SNIPSET_2324 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2324() throws Exception {
		validate(file(this.SNIPSET_2324)).assertNoErrors();
	}

	private String SNIPSET_2325 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2325() throws Exception {
		validate(file(this.SNIPSET_2325)).assertNoErrors();
	}

	private String SNIPSET_2326 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2326() throws Exception {
		validate(file(this.SNIPSET_2326)).assertNoErrors();
	}

	private String SNIPSET_2327 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2327() throws Exception {
		validate(file(this.SNIPSET_2327)).assertNoErrors();
	}

	private String SNIPSET_2328 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2328() throws Exception {
		validate(file(this.SNIPSET_2328)).assertNoErrors();
	}

	private String SNIPSET_2329 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2329() throws Exception {
		validate(file(this.SNIPSET_2329)).assertNoErrors();
	}

	private String SNIPSET_2330 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2330() throws Exception {
		validate(file(this.SNIPSET_2330)).assertNoErrors();
	}

	private String SNIPSET_2331 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2331() throws Exception {
		validate(file(this.SNIPSET_2331)).assertNoErrors();
	}

	private String SNIPSET_2332 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2332() throws Exception {
		validate(file(this.SNIPSET_2332)).assertNoErrors();
	}

	private String SNIPSET_2333 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2333() throws Exception {
		validate(file(this.SNIPSET_2333)).assertNoErrors();
	}

	private String SNIPSET_2334 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2334() throws Exception {
		validate(file(this.SNIPSET_2334)).assertNoErrors();
	}

	private String SNIPSET_2335 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2335() throws Exception {
		validate(file(this.SNIPSET_2335)).assertNoErrors();
	}

	private String SNIPSET_2336 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2336() throws Exception {
		validate(file(this.SNIPSET_2336)).assertNoErrors();
	}

	private String SNIPSET_2337 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_2337() throws Exception {
		validate(file(this.SNIPSET_2337)).assertNoErrors();
	}

	private String SNIPSET_2338 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2338() throws Exception {
		validate(file(this.SNIPSET_2338)).assertNoErrors();
	}

	private String SNIPSET_2339 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2339() throws Exception {
		validate(file(this.SNIPSET_2339)).assertNoErrors();
	}

	private String SNIPSET_2340 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2340() throws Exception {
		validate(file(this.SNIPSET_2340)).assertNoErrors();
	}

	private String SNIPSET_2341 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2341() throws Exception {
		validate(file(this.SNIPSET_2341)).assertNoErrors();
	}

	private String SNIPSET_2342 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2342() throws Exception {
		validate(file(this.SNIPSET_2342)).assertNoErrors();
	}

	private String SNIPSET_2343 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2343() throws Exception {
		validate(file(this.SNIPSET_2343)).assertNoErrors();
	}

	private String SNIPSET_2344 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2344() throws Exception {
		validate(file(this.SNIPSET_2344)).assertNoErrors();
	}

	private String SNIPSET_2345 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2345() throws Exception {
		validate(file(this.SNIPSET_2345)).assertNoErrors();
	}

	private String SNIPSET_2346 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2346() throws Exception {
		validate(file(this.SNIPSET_2346)).assertNoErrors();
	}

	private String SNIPSET_2347 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2347() throws Exception {
		validate(file(this.SNIPSET_2347)).assertNoErrors();
	}

	private String SNIPSET_2348 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2348() throws Exception {
		validate(file(this.SNIPSET_2348)).assertNoErrors();
	}

	private String SNIPSET_2349 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2349() throws Exception {
		validate(file(this.SNIPSET_2349)).assertNoErrors();
	}

	private String SNIPSET_2350 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2350() throws Exception {
		validate(file(this.SNIPSET_2350)).assertNoErrors();
	}

	private String SNIPSET_2351 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2351() throws Exception {
		validate(file(this.SNIPSET_2351)).assertNoErrors();
	}

	private String SNIPSET_2352 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2352() throws Exception {
		validate(file(this.SNIPSET_2352)).assertNoErrors();
	}

	private String SNIPSET_2353 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2353() throws Exception {
		validate(file(this.SNIPSET_2353)).assertNoErrors();
	}

	private String SNIPSET_2354 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2354() throws Exception {
		validate(file(this.SNIPSET_2354)).assertNoErrors();
	}

	private String SNIPSET_2355 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2355() throws Exception {
		validate(file(this.SNIPSET_2355)).assertNoErrors();
	}

	private String SNIPSET_2356 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : int) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2356() throws Exception {
		validate(file(this.SNIPSET_2356)).assertNoErrors();
	}

	private String SNIPSET_2357 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2357() throws Exception {
		validate(file(this.SNIPSET_2357)).assertNoErrors();
	}

	private String SNIPSET_2358 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2358() throws Exception {
		validate(file(this.SNIPSET_2358)).assertNoErrors();
	}

	private String SNIPSET_2359 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2359() throws Exception {
		validate(file(this.SNIPSET_2359)).assertNoErrors();
	}

	private String SNIPSET_2360 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Integer) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2360() throws Exception {
		validate(file(this.SNIPSET_2360)).assertNoErrors();
	}

	private String SNIPSET_2361 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2361() throws Exception {
		validate(file(this.SNIPSET_2361)).assertNoErrors();
	}

	private String SNIPSET_2362 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2362() throws Exception {
		validate(file(this.SNIPSET_2362)).assertNoErrors();
	}

	private String SNIPSET_2363 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : Short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2363() throws Exception {
		validate(file(this.SNIPSET_2363)).assertNoErrors();
	}

	private String SNIPSET_2364 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2364() throws Exception {
		validate(file(this.SNIPSET_2364)).assertNoErrors();
	}

	private String SNIPSET_2365 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : AtomicInteger, b : AtomicInteger) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2365() throws Exception {
		validate(file(this.SNIPSET_2365)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
