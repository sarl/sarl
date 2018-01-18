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
public class PrimitiveShortExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_507 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short) : int {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_507() throws Exception {
		validate(file(this.SNIPSET_507)).assertNoErrors();
	}

	private String SNIPSET_508 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_508() throws Exception {
		validate(file(this.SNIPSET_508)).assertNoErrors();
	}

	private String SNIPSET_509 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_509() throws Exception {
		validate(file(this.SNIPSET_509)).assertNoErrors();
	}

	private String SNIPSET_510 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_510() throws Exception {
		validate(file(this.SNIPSET_510)).assertNoErrors();
	}

	private String SNIPSET_511 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_511() throws Exception {
		validate(file(this.SNIPSET_511)).assertNoErrors();
	}

	private String SNIPSET_512 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_512() throws Exception {
		validate(file(this.SNIPSET_512)).assertNoErrors();
	}

	private String SNIPSET_513 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_513() throws Exception {
		validate(file(this.SNIPSET_513)).assertNoErrors();
	}

	private String SNIPSET_514 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_514() throws Exception {
		validate(file(this.SNIPSET_514)).assertNoErrors();
	}

	private String SNIPSET_515 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_515() throws Exception {
		validate(file(this.SNIPSET_515)).assertNoErrors();
	}

	private String SNIPSET_516 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_516() throws Exception {
		validate(file(this.SNIPSET_516)).assertNoErrors();
	}

	private String SNIPSET_517 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_517() throws Exception {
		validate(file(this.SNIPSET_517)).assertNoErrors();
	}

	private String SNIPSET_518 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_518() throws Exception {
		validate(file(this.SNIPSET_518)).assertNoErrors();
	}

	private String SNIPSET_519 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_519() throws Exception {
		validate(file(this.SNIPSET_519)).assertNoErrors();
	}

	private String SNIPSET_520 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_520() throws Exception {
		validate(file(this.SNIPSET_520)).assertNoErrors();
	}

	private String SNIPSET_521 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_521() throws Exception {
		validate(file(this.SNIPSET_521)).assertNoErrors();
	}

	private String SNIPSET_522 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_522() throws Exception {
		validate(file(this.SNIPSET_522)).assertNoErrors();
	}

	private String SNIPSET_523 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_523() throws Exception {
		validate(file(this.SNIPSET_523)).assertNoErrors();
	}

	private String SNIPSET_524 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_524() throws Exception {
		validate(file(this.SNIPSET_524)).assertNoErrors();
	}

	private String SNIPSET_525 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_525() throws Exception {
		validate(file(this.SNIPSET_525)).assertNoErrors();
	}

	private String SNIPSET_526 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_526() throws Exception {
		validate(file(this.SNIPSET_526)).assertNoErrors();
	}

	private String SNIPSET_527 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_527() throws Exception {
		validate(file(this.SNIPSET_527)).assertNoErrors();
	}

	private String SNIPSET_528 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_528() throws Exception {
		validate(file(this.SNIPSET_528)).assertNoErrors();
	}

	private String SNIPSET_529 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_529() throws Exception {
		validate(file(this.SNIPSET_529)).assertNoErrors();
	}

	private String SNIPSET_530 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_530() throws Exception {
		validate(file(this.SNIPSET_530)).assertNoErrors();
	}

	private String SNIPSET_531 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_531() throws Exception {
		validate(file(this.SNIPSET_531)).assertNoErrors();
	}

	private String SNIPSET_532 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_532() throws Exception {
		validate(file(this.SNIPSET_532)).assertNoErrors();
	}

	private String SNIPSET_533 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_533() throws Exception {
		validate(file(this.SNIPSET_533)).assertNoErrors();
	}

	private String SNIPSET_534 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_534() throws Exception {
		validate(file(this.SNIPSET_534)).assertNoErrors();
	}

	private String SNIPSET_535 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_535() throws Exception {
		validate(file(this.SNIPSET_535)).assertNoErrors();
	}

	private String SNIPSET_536 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_536() throws Exception {
		validate(file(this.SNIPSET_536)).assertNoErrors();
	}

	private String SNIPSET_537 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_537() throws Exception {
		validate(file(this.SNIPSET_537)).assertNoErrors();
	}

	private String SNIPSET_538 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_538() throws Exception {
		validate(file(this.SNIPSET_538)).assertNoErrors();
	}

	private String SNIPSET_539 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_539() throws Exception {
		validate(file(this.SNIPSET_539)).assertNoErrors();
	}

	private String SNIPSET_540 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_540() throws Exception {
		validate(file(this.SNIPSET_540)).assertNoErrors();
	}

	private String SNIPSET_541 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_541() throws Exception {
		validate(file(this.SNIPSET_541)).assertNoErrors();
	}

	private String SNIPSET_542 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_542() throws Exception {
		validate(file(this.SNIPSET_542)).assertNoErrors();
	}

	private String SNIPSET_543 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_543() throws Exception {
		validate(file(this.SNIPSET_543)).assertNoErrors();
	}

	private String SNIPSET_544 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_544() throws Exception {
		validate(file(this.SNIPSET_544)).assertNoErrors();
	}

	private String SNIPSET_545 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_545() throws Exception {
		validate(file(this.SNIPSET_545)).assertNoErrors();
	}

	private String SNIPSET_546 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_546() throws Exception {
		validate(file(this.SNIPSET_546)).assertNoErrors();
	}

	private String SNIPSET_547 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_547() throws Exception {
		validate(file(this.SNIPSET_547)).assertNoErrors();
	}

	private String SNIPSET_548 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_548() throws Exception {
		validate(file(this.SNIPSET_548)).assertNoErrors();
	}

	private String SNIPSET_549 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_549() throws Exception {
		validate(file(this.SNIPSET_549)).assertNoErrors();
	}

	private String SNIPSET_550 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_550() throws Exception {
		validate(file(this.SNIPSET_550)).assertNoErrors();
	}

	private String SNIPSET_551 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_551() throws Exception {
		validate(file(this.SNIPSET_551)).assertNoErrors();
	}

	private String SNIPSET_552 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_552() throws Exception {
		validate(file(this.SNIPSET_552)).assertNoErrors();
	}

	private String SNIPSET_553 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_553() throws Exception {
		validate(file(this.SNIPSET_553)).assertNoErrors();
	}

	private String SNIPSET_554 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_554() throws Exception {
		validate(file(this.SNIPSET_554)).assertNoErrors();
	}

	private String SNIPSET_555 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_555() throws Exception {
		validate(file(this.SNIPSET_555)).assertNoErrors();
	}

	private String SNIPSET_556 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_556() throws Exception {
		validate(file(this.SNIPSET_556)).assertNoErrors();
	}

	private String SNIPSET_557 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_557() throws Exception {
		validate(file(this.SNIPSET_557)).assertNoErrors();
	}

	private String SNIPSET_558 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_558() throws Exception {
		validate(file(this.SNIPSET_558)).assertNoErrors();
	}

	private String SNIPSET_559 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_559() throws Exception {
		validate(file(this.SNIPSET_559)).assertNoErrors();
	}

	private String SNIPSET_560 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_560() throws Exception {
		validate(file(this.SNIPSET_560)).assertNoErrors();
	}

	private String SNIPSET_561 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_561() throws Exception {
		validate(file(this.SNIPSET_561)).assertNoErrors();
	}

	private String SNIPSET_562 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_562() throws Exception {
		validate(file(this.SNIPSET_562)).assertNoErrors();
	}

	private String SNIPSET_563 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_563() throws Exception {
		validate(file(this.SNIPSET_563)).assertNoErrors();
	}

	private String SNIPSET_564 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_564() throws Exception {
		validate(file(this.SNIPSET_564)).assertNoErrors();
	}

	private String SNIPSET_565 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_565() throws Exception {
		validate(file(this.SNIPSET_565)).assertNoErrors();
	}

	private String SNIPSET_566 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_566() throws Exception {
		validate(file(this.SNIPSET_566)).assertNoErrors();
	}

	private String SNIPSET_567 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_567() throws Exception {
		validate(file(this.SNIPSET_567)).assertNoErrors();
	}

	private String SNIPSET_568 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_568() throws Exception {
		validate(file(this.SNIPSET_568)).assertNoErrors();
	}

	private String SNIPSET_569 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_569() throws Exception {
		validate(file(this.SNIPSET_569)).assertNoErrors();
	}

	private String SNIPSET_570 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_570() throws Exception {
		validate(file(this.SNIPSET_570)).assertNoErrors();
	}

	private String SNIPSET_571 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_571() throws Exception {
		validate(file(this.SNIPSET_571)).assertNoErrors();
	}

	private String SNIPSET_572 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_572() throws Exception {
		validate(file(this.SNIPSET_572)).assertNoErrors();
	}

	private String SNIPSET_573 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_573() throws Exception {
		validate(file(this.SNIPSET_573)).assertNoErrors();
	}

	private String SNIPSET_574 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_574() throws Exception {
		validate(file(this.SNIPSET_574)).assertNoErrors();
	}

	private String SNIPSET_575 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_575() throws Exception {
		validate(file(this.SNIPSET_575)).assertNoErrors();
	}

	private String SNIPSET_576 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_576() throws Exception {
		validate(file(this.SNIPSET_576)).assertNoErrors();
	}

	private String SNIPSET_577 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_577() throws Exception {
		validate(file(this.SNIPSET_577)).assertNoErrors();
	}

	private String SNIPSET_578 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_578() throws Exception {
		validate(file(this.SNIPSET_578)).assertNoErrors();
	}

	private String SNIPSET_579 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_579() throws Exception {
		validate(file(this.SNIPSET_579)).assertNoErrors();
	}

	private String SNIPSET_580 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_580() throws Exception {
		validate(file(this.SNIPSET_580)).assertNoErrors();
	}

	private String SNIPSET_581 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_581() throws Exception {
		validate(file(this.SNIPSET_581)).assertNoErrors();
	}

	private String SNIPSET_582 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_582() throws Exception {
		validate(file(this.SNIPSET_582)).assertNoErrors();
	}

	private String SNIPSET_583 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_583() throws Exception {
		validate(file(this.SNIPSET_583)).assertNoErrors();
	}

	private String SNIPSET_584 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_584() throws Exception {
		validate(file(this.SNIPSET_584)).assertNoErrors();
	}

	private String SNIPSET_585 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_585() throws Exception {
		validate(file(this.SNIPSET_585)).assertNoErrors();
	}

	private String SNIPSET_586 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_586() throws Exception {
		validate(file(this.SNIPSET_586)).assertNoErrors();
	}

	private String SNIPSET_587 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_587() throws Exception {
		validate(file(this.SNIPSET_587)).assertNoErrors();
	}

	private String SNIPSET_588 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_588() throws Exception {
		validate(file(this.SNIPSET_588)).assertNoErrors();
	}

	private String SNIPSET_589 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_589() throws Exception {
		validate(file(this.SNIPSET_589)).assertNoErrors();
	}

	private String SNIPSET_590 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_590() throws Exception {
		validate(file(this.SNIPSET_590)).assertNoErrors();
	}

	private String SNIPSET_591 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_591() throws Exception {
		validate(file(this.SNIPSET_591)).assertNoErrors();
	}

	private String SNIPSET_592 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_592() throws Exception {
		validate(file(this.SNIPSET_592)).assertNoErrors();
	}

	private String SNIPSET_593 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_593() throws Exception {
		validate(file(this.SNIPSET_593)).assertNoErrors();
	}

	private String SNIPSET_594 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_594() throws Exception {
		validate(file(this.SNIPSET_594)).assertNoErrors();
	}

	private String SNIPSET_595 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_595() throws Exception {
		validate(file(this.SNIPSET_595)).assertNoErrors();
	}

	private String SNIPSET_596 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_596() throws Exception {
		validate(file(this.SNIPSET_596)).assertNoErrors();
	}

	private String SNIPSET_597 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_597() throws Exception {
		validate(file(this.SNIPSET_597)).assertNoErrors();
	}

	private String SNIPSET_598 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_598() throws Exception {
		validate(file(this.SNIPSET_598)).assertNoErrors();
	}

	private String SNIPSET_599 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_599() throws Exception {
		validate(file(this.SNIPSET_599)).assertNoErrors();
	}

	private String SNIPSET_600 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_600() throws Exception {
		validate(file(this.SNIPSET_600)).assertNoErrors();
	}

	private String SNIPSET_601 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_601() throws Exception {
		validate(file(this.SNIPSET_601)).assertNoErrors();
	}

	private String SNIPSET_602 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_602() throws Exception {
		validate(file(this.SNIPSET_602)).assertNoErrors();
	}

	private String SNIPSET_603 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_603() throws Exception {
		validate(file(this.SNIPSET_603)).assertNoErrors();
	}

	private String SNIPSET_604 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_604() throws Exception {
		validate(file(this.SNIPSET_604)).assertNoErrors();
	}

	private String SNIPSET_605 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_605() throws Exception {
		validate(file(this.SNIPSET_605)).assertNoErrors();
	}

	private String SNIPSET_606 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_606() throws Exception {
		validate(file(this.SNIPSET_606)).assertNoErrors();
	}

	private String SNIPSET_607 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_607() throws Exception {
		validate(file(this.SNIPSET_607)).assertNoErrors();
	}

	private String SNIPSET_608 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_608() throws Exception {
		validate(file(this.SNIPSET_608)).assertNoErrors();
	}

	private String SNIPSET_609 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_609() throws Exception {
		validate(file(this.SNIPSET_609)).assertNoErrors();
	}

	private String SNIPSET_610 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_610() throws Exception {
		validate(file(this.SNIPSET_610)).assertNoErrors();
	}

	private String SNIPSET_611 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_611() throws Exception {
		validate(file(this.SNIPSET_611)).assertNoErrors();
	}

	private String SNIPSET_612 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_612() throws Exception {
		validate(file(this.SNIPSET_612)).assertNoErrors();
	}

	private String SNIPSET_613 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_613() throws Exception {
		validate(file(this.SNIPSET_613)).assertNoErrors();
	}

	private String SNIPSET_614 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_614() throws Exception {
		validate(file(this.SNIPSET_614)).assertNoErrors();
	}

	private String SNIPSET_615 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_615() throws Exception {
		validate(file(this.SNIPSET_615)).assertNoErrors();
	}

	private String SNIPSET_616 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_616() throws Exception {
		validate(file(this.SNIPSET_616)).assertNoErrors();
	}

	private String SNIPSET_617 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_617() throws Exception {
		validate(file(this.SNIPSET_617)).assertNoErrors();
	}

	private String SNIPSET_618 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_618() throws Exception {
		validate(file(this.SNIPSET_618)).assertNoErrors();
	}

	private String SNIPSET_619 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_619() throws Exception {
		validate(file(this.SNIPSET_619)).assertNoErrors();
	}

	private String SNIPSET_620 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_620() throws Exception {
		validate(file(this.SNIPSET_620)).assertNoErrors();
	}

	private String SNIPSET_621 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_621() throws Exception {
		validate(file(this.SNIPSET_621)).assertNoErrors();
	}

	private String SNIPSET_622 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_622() throws Exception {
		validate(file(this.SNIPSET_622)).assertNoErrors();
	}

	private String SNIPSET_623 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_623() throws Exception {
		validate(file(this.SNIPSET_623)).assertNoErrors();
	}

	private String SNIPSET_624 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_624() throws Exception {
		validate(file(this.SNIPSET_624)).assertNoErrors();
	}

	private String SNIPSET_625 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_625() throws Exception {
		validate(file(this.SNIPSET_625)).assertNoErrors();
	}

	private String SNIPSET_626 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_626() throws Exception {
		validate(file(this.SNIPSET_626)).assertNoErrors();
	}

	private String SNIPSET_627 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_627() throws Exception {
		validate(file(this.SNIPSET_627)).assertNoErrors();
	}

	private String SNIPSET_628 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_628() throws Exception {
		validate(file(this.SNIPSET_628)).assertNoErrors();
	}

	private String SNIPSET_629 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_629() throws Exception {
		validate(file(this.SNIPSET_629)).assertNoErrors();
	}

	private String SNIPSET_630 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_630() throws Exception {
		validate(file(this.SNIPSET_630)).assertNoErrors();
	}

	private String SNIPSET_631 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_631() throws Exception {
		validate(file(this.SNIPSET_631)).assertNoErrors();
	}

	private String SNIPSET_632 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_632() throws Exception {
		validate(file(this.SNIPSET_632)).assertNoErrors();
	}

	private String SNIPSET_633 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_633() throws Exception {
		validate(file(this.SNIPSET_633)).assertNoErrors();
	}

	private String SNIPSET_634 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_634() throws Exception {
		validate(file(this.SNIPSET_634)).assertNoErrors();
	}

	private String SNIPSET_635 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_635() throws Exception {
		validate(file(this.SNIPSET_635)).assertNoErrors();
	}

	private String SNIPSET_636 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_636() throws Exception {
		validate(file(this.SNIPSET_636)).assertNoErrors();
	}

	private String SNIPSET_637 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_637() throws Exception {
		validate(file(this.SNIPSET_637)).assertNoErrors();
	}

	private String SNIPSET_638 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_638() throws Exception {
		validate(file(this.SNIPSET_638)).assertNoErrors();
	}

	private String SNIPSET_639 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_639() throws Exception {
		validate(file(this.SNIPSET_639)).assertNoErrors();
	}

	private String SNIPSET_640 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_640() throws Exception {
		validate(file(this.SNIPSET_640)).assertNoErrors();
	}

	private String SNIPSET_641 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_641() throws Exception {
		validate(file(this.SNIPSET_641)).assertNoErrors();
	}

	private String SNIPSET_642 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_642() throws Exception {
		validate(file(this.SNIPSET_642)).assertNoErrors();
	}

	private String SNIPSET_643 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_643() throws Exception {
		validate(file(this.SNIPSET_643)).assertNoErrors();
	}

	private String SNIPSET_644 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_644() throws Exception {
		validate(file(this.SNIPSET_644)).assertNoErrors();
	}

	private String SNIPSET_645 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_645() throws Exception {
		validate(file(this.SNIPSET_645)).assertNoErrors();
	}

	private String SNIPSET_646 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_646() throws Exception {
		validate(file(this.SNIPSET_646)).assertNoErrors();
	}

	private String SNIPSET_647 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_647() throws Exception {
		validate(file(this.SNIPSET_647)).assertNoErrors();
	}

	private String SNIPSET_648 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_648() throws Exception {
		validate(file(this.SNIPSET_648)).assertNoErrors();
	}

	private String SNIPSET_649 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_649() throws Exception {
		validate(file(this.SNIPSET_649)).assertNoErrors();
	}

	private String SNIPSET_650 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_650() throws Exception {
		validate(file(this.SNIPSET_650)).assertNoErrors();
	}

	private String SNIPSET_651 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_651() throws Exception {
		validate(file(this.SNIPSET_651)).assertNoErrors();
	}

	private String SNIPSET_652 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_652() throws Exception {
		validate(file(this.SNIPSET_652)).assertNoErrors();
	}

	private String SNIPSET_653 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_653() throws Exception {
		validate(file(this.SNIPSET_653)).assertNoErrors();
	}

	private String SNIPSET_654 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_654() throws Exception {
		validate(file(this.SNIPSET_654)).assertNoErrors();
	}

	private String SNIPSET_655 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_655() throws Exception {
		validate(file(this.SNIPSET_655)).assertNoErrors();
	}

	private String SNIPSET_656 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_656() throws Exception {
		validate(file(this.SNIPSET_656)).assertNoErrors();
	}

	private String SNIPSET_657 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_657() throws Exception {
		validate(file(this.SNIPSET_657)).assertNoErrors();
	}

	private String SNIPSET_658 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_658() throws Exception {
		validate(file(this.SNIPSET_658)).assertNoErrors();
	}

	private String SNIPSET_659 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_659() throws Exception {
		validate(file(this.SNIPSET_659)).assertNoErrors();
	}

	private String SNIPSET_660 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_660() throws Exception {
		validate(file(this.SNIPSET_660)).assertNoErrors();
	}

	private String SNIPSET_661 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_661() throws Exception {
		validate(file(this.SNIPSET_661)).assertNoErrors();
	}

	private String SNIPSET_662 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_662() throws Exception {
		validate(file(this.SNIPSET_662)).assertNoErrors();
	}

	private String SNIPSET_663 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_663() throws Exception {
		validate(file(this.SNIPSET_663)).assertNoErrors();
	}

	private String SNIPSET_664 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_664() throws Exception {
		validate(file(this.SNIPSET_664)).assertNoErrors();
	}

	private String SNIPSET_665 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_665() throws Exception {
		validate(file(this.SNIPSET_665)).assertNoErrors();
	}

	private String SNIPSET_666 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : int) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_666() throws Exception {
		validate(file(this.SNIPSET_666)).assertNoErrors();
	}

	private String SNIPSET_667 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_667() throws Exception {
		validate(file(this.SNIPSET_667)).assertNoErrors();
	}

	private String SNIPSET_668 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_668() throws Exception {
		validate(file(this.SNIPSET_668)).assertNoErrors();
	}

	private String SNIPSET_669 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_669() throws Exception {
		validate(file(this.SNIPSET_669)).assertNoErrors();
	}

	private String SNIPSET_670 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Integer) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_670() throws Exception {
		validate(file(this.SNIPSET_670)).assertNoErrors();
	}

	private String SNIPSET_671 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_671() throws Exception {
		validate(file(this.SNIPSET_671)).assertNoErrors();
	}

	private String SNIPSET_672 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_672() throws Exception {
		validate(file(this.SNIPSET_672)).assertNoErrors();
	}

	private String SNIPSET_673 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : Short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_673() throws Exception {
		validate(file(this.SNIPSET_673)).assertNoErrors();
	}

	private String SNIPSET_674 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_674() throws Exception {
		validate(file(this.SNIPSET_674)).assertNoErrors();
	}

	private String SNIPSET_675 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : short, b : AtomicInteger) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_675() throws Exception {
		validate(file(this.SNIPSET_675)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
