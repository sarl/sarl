/*
/*
 * Copyright (C) 2014-2017 the original authors or authors.
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
public class PrimitiveDoubleExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_338 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double) : double {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_338() throws Exception {
		validate(file(this.SNIPSET_338)).assertNoErrors();
	}

	private String SNIPSET_339 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_339() throws Exception {
		validate(file(this.SNIPSET_339)).assertNoErrors();
	}

	private String SNIPSET_340 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_340() throws Exception {
		validate(file(this.SNIPSET_340)).assertNoErrors();
	}

	private String SNIPSET_341 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_341() throws Exception {
		validate(file(this.SNIPSET_341)).assertNoErrors();
	}

	private String SNIPSET_342 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_342() throws Exception {
		validate(file(this.SNIPSET_342)).assertNoErrors();
	}

	private String SNIPSET_343 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_343() throws Exception {
		validate(file(this.SNIPSET_343)).assertNoErrors();
	}

	private String SNIPSET_344 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_344() throws Exception {
		validate(file(this.SNIPSET_344)).assertNoErrors();
	}

	private String SNIPSET_345 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_345() throws Exception {
		validate(file(this.SNIPSET_345)).assertNoErrors();
	}

	private String SNIPSET_346 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_346() throws Exception {
		validate(file(this.SNIPSET_346)).assertNoErrors();
	}

	private String SNIPSET_347 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_347() throws Exception {
		validate(file(this.SNIPSET_347)).assertNoErrors();
	}

	private String SNIPSET_348 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_348() throws Exception {
		validate(file(this.SNIPSET_348)).assertNoErrors();
	}

	private String SNIPSET_349 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_349() throws Exception {
		validate(file(this.SNIPSET_349)).assertNoErrors();
	}

	private String SNIPSET_350 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_350() throws Exception {
		validate(file(this.SNIPSET_350)).assertNoErrors();
	}

	private String SNIPSET_351 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_351() throws Exception {
		validate(file(this.SNIPSET_351)).assertNoErrors();
	}

	private String SNIPSET_352 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_352() throws Exception {
		validate(file(this.SNIPSET_352)).assertNoErrors();
	}

	private String SNIPSET_353 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_353() throws Exception {
		validate(file(this.SNIPSET_353)).assertNoErrors();
	}

	private String SNIPSET_354 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_354() throws Exception {
		validate(file(this.SNIPSET_354)).assertNoErrors();
	}

	private String SNIPSET_355 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_355() throws Exception {
		validate(file(this.SNIPSET_355)).assertNoErrors();
	}

	private String SNIPSET_356 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_356() throws Exception {
		validate(file(this.SNIPSET_356)).assertNoErrors();
	}

	private String SNIPSET_357 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_357() throws Exception {
		validate(file(this.SNIPSET_357)).assertNoErrors();
	}

	private String SNIPSET_358 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_358() throws Exception {
		validate(file(this.SNIPSET_358)).assertNoErrors();
	}

	private String SNIPSET_359 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_359() throws Exception {
		validate(file(this.SNIPSET_359)).assertNoErrors();
	}

	private String SNIPSET_360 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_360() throws Exception {
		validate(file(this.SNIPSET_360)).assertNoErrors();
	}

	private String SNIPSET_361 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_361() throws Exception {
		validate(file(this.SNIPSET_361)).assertNoErrors();
	}

	private String SNIPSET_362 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_362() throws Exception {
		validate(file(this.SNIPSET_362)).assertNoErrors();
	}

	private String SNIPSET_363 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_363() throws Exception {
		validate(file(this.SNIPSET_363)).assertNoErrors();
	}

	private String SNIPSET_364 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_364() throws Exception {
		validate(file(this.SNIPSET_364)).assertNoErrors();
	}

	private String SNIPSET_365 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_365() throws Exception {
		validate(file(this.SNIPSET_365)).assertNoErrors();
	}

	private String SNIPSET_366 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_366() throws Exception {
		validate(file(this.SNIPSET_366)).assertNoErrors();
	}

	private String SNIPSET_367 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_367() throws Exception {
		validate(file(this.SNIPSET_367)).assertNoErrors();
	}

	private String SNIPSET_368 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_368() throws Exception {
		validate(file(this.SNIPSET_368)).assertNoErrors();
	}

	private String SNIPSET_369 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_369() throws Exception {
		validate(file(this.SNIPSET_369)).assertNoErrors();
	}

	private String SNIPSET_370 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_370() throws Exception {
		validate(file(this.SNIPSET_370)).assertNoErrors();
	}

	private String SNIPSET_371 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_371() throws Exception {
		validate(file(this.SNIPSET_371)).assertNoErrors();
	}

	private String SNIPSET_372 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_372() throws Exception {
		validate(file(this.SNIPSET_372)).assertNoErrors();
	}

	private String SNIPSET_373 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_373() throws Exception {
		validate(file(this.SNIPSET_373)).assertNoErrors();
	}

	private String SNIPSET_374 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_374() throws Exception {
		validate(file(this.SNIPSET_374)).assertNoErrors();
	}

	private String SNIPSET_375 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_375() throws Exception {
		validate(file(this.SNIPSET_375)).assertNoErrors();
	}

	private String SNIPSET_376 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_376() throws Exception {
		validate(file(this.SNIPSET_376)).assertNoErrors();
	}

	private String SNIPSET_377 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_377() throws Exception {
		validate(file(this.SNIPSET_377)).assertNoErrors();
	}

	private String SNIPSET_378 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_378() throws Exception {
		validate(file(this.SNIPSET_378)).assertNoErrors();
	}

	private String SNIPSET_379 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_379() throws Exception {
		validate(file(this.SNIPSET_379)).assertNoErrors();
	}

	private String SNIPSET_380 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_380() throws Exception {
		validate(file(this.SNIPSET_380)).assertNoErrors();
	}

	private String SNIPSET_381 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_381() throws Exception {
		validate(file(this.SNIPSET_381)).assertNoErrors();
	}

	private String SNIPSET_382 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_382() throws Exception {
		validate(file(this.SNIPSET_382)).assertNoErrors();
	}

	private String SNIPSET_383 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_383() throws Exception {
		validate(file(this.SNIPSET_383)).assertNoErrors();
	}

	private String SNIPSET_384 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_384() throws Exception {
		validate(file(this.SNIPSET_384)).assertNoErrors();
	}

	private String SNIPSET_385 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_385() throws Exception {
		validate(file(this.SNIPSET_385)).assertNoErrors();
	}

	private String SNIPSET_386 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_386() throws Exception {
		validate(file(this.SNIPSET_386)).assertNoErrors();
	}

	private String SNIPSET_387 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_387() throws Exception {
		validate(file(this.SNIPSET_387)).assertNoErrors();
	}

	private String SNIPSET_388 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_388() throws Exception {
		validate(file(this.SNIPSET_388)).assertNoErrors();
	}

	private String SNIPSET_389 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_389() throws Exception {
		validate(file(this.SNIPSET_389)).assertNoErrors();
	}

	private String SNIPSET_390 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_390() throws Exception {
		validate(file(this.SNIPSET_390)).assertNoErrors();
	}

	private String SNIPSET_391 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_391() throws Exception {
		validate(file(this.SNIPSET_391)).assertNoErrors();
	}

	private String SNIPSET_392 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_392() throws Exception {
		validate(file(this.SNIPSET_392)).assertNoErrors();
	}

	private String SNIPSET_393 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_393() throws Exception {
		validate(file(this.SNIPSET_393)).assertNoErrors();
	}

	private String SNIPSET_394 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_394() throws Exception {
		validate(file(this.SNIPSET_394)).assertNoErrors();
	}

	private String SNIPSET_395 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_395() throws Exception {
		validate(file(this.SNIPSET_395)).assertNoErrors();
	}

	private String SNIPSET_396 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_396() throws Exception {
		validate(file(this.SNIPSET_396)).assertNoErrors();
	}

	private String SNIPSET_397 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_397() throws Exception {
		validate(file(this.SNIPSET_397)).assertNoErrors();
	}

	private String SNIPSET_398 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_398() throws Exception {
		validate(file(this.SNIPSET_398)).assertNoErrors();
	}

	private String SNIPSET_399 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_399() throws Exception {
		validate(file(this.SNIPSET_399)).assertNoErrors();
	}

	private String SNIPSET_400 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_400() throws Exception {
		validate(file(this.SNIPSET_400)).assertNoErrors();
	}

	private String SNIPSET_401 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_401() throws Exception {
		validate(file(this.SNIPSET_401)).assertNoErrors();
	}

	private String SNIPSET_402 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_402() throws Exception {
		validate(file(this.SNIPSET_402)).assertNoErrors();
	}

	private String SNIPSET_403 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_403() throws Exception {
		validate(file(this.SNIPSET_403)).assertNoErrors();
	}

	private String SNIPSET_404 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_404() throws Exception {
		validate(file(this.SNIPSET_404)).assertNoErrors();
	}

	private String SNIPSET_405 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_405() throws Exception {
		validate(file(this.SNIPSET_405)).assertNoErrors();
	}

	private String SNIPSET_406 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_406() throws Exception {
		validate(file(this.SNIPSET_406)).assertNoErrors();
	}

	private String SNIPSET_407 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_407() throws Exception {
		validate(file(this.SNIPSET_407)).assertNoErrors();
	}

	private String SNIPSET_408 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_408() throws Exception {
		validate(file(this.SNIPSET_408)).assertNoErrors();
	}

	private String SNIPSET_409 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_409() throws Exception {
		validate(file(this.SNIPSET_409)).assertNoErrors();
	}

	private String SNIPSET_410 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_410() throws Exception {
		validate(file(this.SNIPSET_410)).assertNoErrors();
	}

	private String SNIPSET_411 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_411() throws Exception {
		validate(file(this.SNIPSET_411)).assertNoErrors();
	}

	private String SNIPSET_412 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_412() throws Exception {
		validate(file(this.SNIPSET_412)).assertNoErrors();
	}

	private String SNIPSET_413 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_413() throws Exception {
		validate(file(this.SNIPSET_413)).assertNoErrors();
	}

	private String SNIPSET_414 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_414() throws Exception {
		validate(file(this.SNIPSET_414)).assertNoErrors();
	}

	private String SNIPSET_415 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_415() throws Exception {
		validate(file(this.SNIPSET_415)).assertNoErrors();
	}

	private String SNIPSET_416 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_416() throws Exception {
		validate(file(this.SNIPSET_416)).assertNoErrors();
	}

	private String SNIPSET_417 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_417() throws Exception {
		validate(file(this.SNIPSET_417)).assertNoErrors();
	}

	private String SNIPSET_418 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_418() throws Exception {
		validate(file(this.SNIPSET_418)).assertNoErrors();
	}

	private String SNIPSET_419 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_419() throws Exception {
		validate(file(this.SNIPSET_419)).assertNoErrors();
	}

	private String SNIPSET_420 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_420() throws Exception {
		validate(file(this.SNIPSET_420)).assertNoErrors();
	}

	private String SNIPSET_421 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_421() throws Exception {
		validate(file(this.SNIPSET_421)).assertNoErrors();
	}

	private String SNIPSET_422 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_422() throws Exception {
		validate(file(this.SNIPSET_422)).assertNoErrors();
	}

	private String SNIPSET_423 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_423() throws Exception {
		validate(file(this.SNIPSET_423)).assertNoErrors();
	}

	private String SNIPSET_424 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_424() throws Exception {
		validate(file(this.SNIPSET_424)).assertNoErrors();
	}

	private String SNIPSET_425 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_425() throws Exception {
		validate(file(this.SNIPSET_425)).assertNoErrors();
	}

	private String SNIPSET_426 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_426() throws Exception {
		validate(file(this.SNIPSET_426)).assertNoErrors();
	}

	private String SNIPSET_427 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_427() throws Exception {
		validate(file(this.SNIPSET_427)).assertNoErrors();
	}

	private String SNIPSET_428 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_428() throws Exception {
		validate(file(this.SNIPSET_428)).assertNoErrors();
	}

	private String SNIPSET_429 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_429() throws Exception {
		validate(file(this.SNIPSET_429)).assertNoErrors();
	}

	private String SNIPSET_430 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_430() throws Exception {
		validate(file(this.SNIPSET_430)).assertNoErrors();
	}

	private String SNIPSET_431 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_431() throws Exception {
		validate(file(this.SNIPSET_431)).assertNoErrors();
	}

	private String SNIPSET_432 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_432() throws Exception {
		validate(file(this.SNIPSET_432)).assertNoErrors();
	}

	private String SNIPSET_433 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_433() throws Exception {
		validate(file(this.SNIPSET_433)).assertNoErrors();
	}

	private String SNIPSET_434 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_434() throws Exception {
		validate(file(this.SNIPSET_434)).assertNoErrors();
	}

	private String SNIPSET_435 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_435() throws Exception {
		validate(file(this.SNIPSET_435)).assertNoErrors();
	}

	private String SNIPSET_436 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_436() throws Exception {
		validate(file(this.SNIPSET_436)).assertNoErrors();
	}

	private String SNIPSET_437 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_437() throws Exception {
		validate(file(this.SNIPSET_437)).assertNoErrors();
	}

	private String SNIPSET_438 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_438() throws Exception {
		validate(file(this.SNIPSET_438)).assertNoErrors();
	}

	private String SNIPSET_439 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_439() throws Exception {
		validate(file(this.SNIPSET_439)).assertNoErrors();
	}

	private String SNIPSET_440 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_440() throws Exception {
		validate(file(this.SNIPSET_440)).assertNoErrors();
	}

	private String SNIPSET_441 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_441() throws Exception {
		validate(file(this.SNIPSET_441)).assertNoErrors();
	}

	private String SNIPSET_442 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_442() throws Exception {
		validate(file(this.SNIPSET_442)).assertNoErrors();
	}

	private String SNIPSET_443 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_443() throws Exception {
		validate(file(this.SNIPSET_443)).assertNoErrors();
	}

	private String SNIPSET_444 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_444() throws Exception {
		validate(file(this.SNIPSET_444)).assertNoErrors();
	}

	private String SNIPSET_445 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_445() throws Exception {
		validate(file(this.SNIPSET_445)).assertNoErrors();
	}

	private String SNIPSET_446 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_446() throws Exception {
		validate(file(this.SNIPSET_446)).assertNoErrors();
	}

	private String SNIPSET_447 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_447() throws Exception {
		validate(file(this.SNIPSET_447)).assertNoErrors();
	}

	private String SNIPSET_448 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_448() throws Exception {
		validate(file(this.SNIPSET_448)).assertNoErrors();
	}

	private String SNIPSET_449 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_449() throws Exception {
		validate(file(this.SNIPSET_449)).assertNoErrors();
	}

	private String SNIPSET_450 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_450() throws Exception {
		validate(file(this.SNIPSET_450)).assertNoErrors();
	}

	private String SNIPSET_451 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_451() throws Exception {
		validate(file(this.SNIPSET_451)).assertNoErrors();
	}

	private String SNIPSET_452 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_452() throws Exception {
		validate(file(this.SNIPSET_452)).assertNoErrors();
	}

	private String SNIPSET_453 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_453() throws Exception {
		validate(file(this.SNIPSET_453)).assertNoErrors();
	}

	private String SNIPSET_454 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_454() throws Exception {
		validate(file(this.SNIPSET_454)).assertNoErrors();
	}

	private String SNIPSET_455 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_455() throws Exception {
		validate(file(this.SNIPSET_455)).assertNoErrors();
	}

	private String SNIPSET_456 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_456() throws Exception {
		validate(file(this.SNIPSET_456)).assertNoErrors();
	}

	private String SNIPSET_457 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_457() throws Exception {
		validate(file(this.SNIPSET_457)).assertNoErrors();
	}

	private String SNIPSET_458 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_458() throws Exception {
		validate(file(this.SNIPSET_458)).assertNoErrors();
	}

	private String SNIPSET_459 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_459() throws Exception {
		validate(file(this.SNIPSET_459)).assertNoErrors();
	}

	private String SNIPSET_460 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_460() throws Exception {
		validate(file(this.SNIPSET_460)).assertNoErrors();
	}

	private String SNIPSET_461 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_461() throws Exception {
		validate(file(this.SNIPSET_461)).assertNoErrors();
	}

	private String SNIPSET_462 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_462() throws Exception {
		validate(file(this.SNIPSET_462)).assertNoErrors();
	}

	private String SNIPSET_463 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_463() throws Exception {
		validate(file(this.SNIPSET_463)).assertNoErrors();
	}

	private String SNIPSET_464 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_464() throws Exception {
		validate(file(this.SNIPSET_464)).assertNoErrors();
	}

	private String SNIPSET_465 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_465() throws Exception {
		validate(file(this.SNIPSET_465)).assertNoErrors();
	}

	private String SNIPSET_466 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_466() throws Exception {
		validate(file(this.SNIPSET_466)).assertNoErrors();
	}

	private String SNIPSET_467 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_467() throws Exception {
		validate(file(this.SNIPSET_467)).assertNoErrors();
	}

	private String SNIPSET_468 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_468() throws Exception {
		validate(file(this.SNIPSET_468)).assertNoErrors();
	}

	private String SNIPSET_469 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_469() throws Exception {
		validate(file(this.SNIPSET_469)).assertNoErrors();
	}

	private String SNIPSET_470 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_470() throws Exception {
		validate(file(this.SNIPSET_470)).assertNoErrors();
	}

	private String SNIPSET_471 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_471() throws Exception {
		validate(file(this.SNIPSET_471)).assertNoErrors();
	}

	private String SNIPSET_472 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_472() throws Exception {
		validate(file(this.SNIPSET_472)).assertNoErrors();
	}

	private String SNIPSET_473 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_473() throws Exception {
		validate(file(this.SNIPSET_473)).assertNoErrors();
	}

	private String SNIPSET_474 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_474() throws Exception {
		validate(file(this.SNIPSET_474)).assertNoErrors();
	}

	private String SNIPSET_475 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_475() throws Exception {
		validate(file(this.SNIPSET_475)).assertNoErrors();
	}

	private String SNIPSET_476 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_476() throws Exception {
		validate(file(this.SNIPSET_476)).assertNoErrors();
	}

	private String SNIPSET_477 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_477() throws Exception {
		validate(file(this.SNIPSET_477)).assertNoErrors();
	}

	private String SNIPSET_478 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_478() throws Exception {
		validate(file(this.SNIPSET_478)).assertNoErrors();
	}

	private String SNIPSET_479 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_479() throws Exception {
		validate(file(this.SNIPSET_479)).assertNoErrors();
	}

	private String SNIPSET_480 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_480() throws Exception {
		validate(file(this.SNIPSET_480)).assertNoErrors();
	}

	private String SNIPSET_481 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_481() throws Exception {
		validate(file(this.SNIPSET_481)).assertNoErrors();
	}

	private String SNIPSET_482 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_482() throws Exception {
		validate(file(this.SNIPSET_482)).assertNoErrors();
	}

	private String SNIPSET_483 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_483() throws Exception {
		validate(file(this.SNIPSET_483)).assertNoErrors();
	}

	private String SNIPSET_484 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_484() throws Exception {
		validate(file(this.SNIPSET_484)).assertNoErrors();
	}

	private String SNIPSET_485 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_485() throws Exception {
		validate(file(this.SNIPSET_485)).assertNoErrors();
	}

	private String SNIPSET_486 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_486() throws Exception {
		validate(file(this.SNIPSET_486)).assertNoErrors();
	}

	private String SNIPSET_487 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_487() throws Exception {
		validate(file(this.SNIPSET_487)).assertNoErrors();
	}

	private String SNIPSET_488 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_488() throws Exception {
		validate(file(this.SNIPSET_488)).assertNoErrors();
	}

	private String SNIPSET_489 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_489() throws Exception {
		validate(file(this.SNIPSET_489)).assertNoErrors();
	}

	private String SNIPSET_490 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_490() throws Exception {
		validate(file(this.SNIPSET_490)).assertNoErrors();
	}

	private String SNIPSET_491 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_491() throws Exception {
		validate(file(this.SNIPSET_491)).assertNoErrors();
	}

	private String SNIPSET_492 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_492() throws Exception {
		validate(file(this.SNIPSET_492)).assertNoErrors();
	}

	private String SNIPSET_493 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : byte) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_493() throws Exception {
		validate(file(this.SNIPSET_493)).assertNoErrors();
	}

	private String SNIPSET_494 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : long) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_494() throws Exception {
		validate(file(this.SNIPSET_494)).assertNoErrors();
	}

	private String SNIPSET_495 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_495() throws Exception {
		validate(file(this.SNIPSET_495)).assertNoErrors();
	}

	private String SNIPSET_496 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : short) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_496() throws Exception {
		validate(file(this.SNIPSET_496)).assertNoErrors();
	}

	private String SNIPSET_497 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : int) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_497() throws Exception {
		validate(file(this.SNIPSET_497)).assertNoErrors();
	}

	private String SNIPSET_498 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : float) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_498() throws Exception {
		validate(file(this.SNIPSET_498)).assertNoErrors();
	}

	private String SNIPSET_499 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Long) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_499() throws Exception {
		validate(file(this.SNIPSET_499)).assertNoErrors();
	}

	private String SNIPSET_500 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Float) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_500() throws Exception {
		validate(file(this.SNIPSET_500)).assertNoErrors();
	}

	private String SNIPSET_501 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Integer) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_501() throws Exception {
		validate(file(this.SNIPSET_501)).assertNoErrors();
	}

	private String SNIPSET_502 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_502() throws Exception {
		validate(file(this.SNIPSET_502)).assertNoErrors();
	}

	private String SNIPSET_503 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Byte) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_503() throws Exception {
		validate(file(this.SNIPSET_503)).assertNoErrors();
	}

	private String SNIPSET_504 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : Short) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_504() throws Exception {
		validate(file(this.SNIPSET_504)).assertNoErrors();
	}

	private String SNIPSET_505 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicLong) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_505() throws Exception {
		validate(file(this.SNIPSET_505)).assertNoErrors();
	}

	private String SNIPSET_506 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : double, b : AtomicInteger) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_506() throws Exception {
		validate(file(this.SNIPSET_506)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
