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
public class PrimitiveByteExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_0 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte) : int {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_0() throws Exception {
		validate(file(this.SNIPSET_0)).assertNoErrors();
	}

	private String SNIPSET_1 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1() throws Exception {
		validate(file(this.SNIPSET_1)).assertNoErrors();
	}

	private String SNIPSET_2 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_2() throws Exception {
		validate(file(this.SNIPSET_2)).assertNoErrors();
	}

	private String SNIPSET_3 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_3() throws Exception {
		validate(file(this.SNIPSET_3)).assertNoErrors();
	}

	private String SNIPSET_4 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_4() throws Exception {
		validate(file(this.SNIPSET_4)).assertNoErrors();
	}

	private String SNIPSET_5 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_5() throws Exception {
		validate(file(this.SNIPSET_5)).assertNoErrors();
	}

	private String SNIPSET_6 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_6() throws Exception {
		validate(file(this.SNIPSET_6)).assertNoErrors();
	}

	private String SNIPSET_7 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_7() throws Exception {
		validate(file(this.SNIPSET_7)).assertNoErrors();
	}

	private String SNIPSET_8 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_8() throws Exception {
		validate(file(this.SNIPSET_8)).assertNoErrors();
	}

	private String SNIPSET_9 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_9() throws Exception {
		validate(file(this.SNIPSET_9)).assertNoErrors();
	}

	private String SNIPSET_10 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_10() throws Exception {
		validate(file(this.SNIPSET_10)).assertNoErrors();
	}

	private String SNIPSET_11 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_11() throws Exception {
		validate(file(this.SNIPSET_11)).assertNoErrors();
	}

	private String SNIPSET_12 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_12() throws Exception {
		validate(file(this.SNIPSET_12)).assertNoErrors();
	}

	private String SNIPSET_13 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_13() throws Exception {
		validate(file(this.SNIPSET_13)).assertNoErrors();
	}

	private String SNIPSET_14 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_14() throws Exception {
		validate(file(this.SNIPSET_14)).assertNoErrors();
	}

	private String SNIPSET_15 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_15() throws Exception {
		validate(file(this.SNIPSET_15)).assertNoErrors();
	}

	private String SNIPSET_16 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_16() throws Exception {
		validate(file(this.SNIPSET_16)).assertNoErrors();
	}

	private String SNIPSET_17 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_17() throws Exception {
		validate(file(this.SNIPSET_17)).assertNoErrors();
	}

	private String SNIPSET_18 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_18() throws Exception {
		validate(file(this.SNIPSET_18)).assertNoErrors();
	}

	private String SNIPSET_19 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_19() throws Exception {
		validate(file(this.SNIPSET_19)).assertNoErrors();
	}

	private String SNIPSET_20 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_20() throws Exception {
		validate(file(this.SNIPSET_20)).assertNoErrors();
	}

	private String SNIPSET_21 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_21() throws Exception {
		validate(file(this.SNIPSET_21)).assertNoErrors();
	}

	private String SNIPSET_22 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_22() throws Exception {
		validate(file(this.SNIPSET_22)).assertNoErrors();
	}

	private String SNIPSET_23 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_23() throws Exception {
		validate(file(this.SNIPSET_23)).assertNoErrors();
	}

	private String SNIPSET_24 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_24() throws Exception {
		validate(file(this.SNIPSET_24)).assertNoErrors();
	}

	private String SNIPSET_25 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_25() throws Exception {
		validate(file(this.SNIPSET_25)).assertNoErrors();
	}

	private String SNIPSET_26 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_26() throws Exception {
		validate(file(this.SNIPSET_26)).assertNoErrors();
	}

	private String SNIPSET_27 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_27() throws Exception {
		validate(file(this.SNIPSET_27)).assertNoErrors();
	}

	private String SNIPSET_28 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_28() throws Exception {
		validate(file(this.SNIPSET_28)).assertNoErrors();
	}

	private String SNIPSET_29 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_29() throws Exception {
		validate(file(this.SNIPSET_29)).assertNoErrors();
	}

	private String SNIPSET_30 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_30() throws Exception {
		validate(file(this.SNIPSET_30)).assertNoErrors();
	}

	private String SNIPSET_31 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_31() throws Exception {
		validate(file(this.SNIPSET_31)).assertNoErrors();
	}

	private String SNIPSET_32 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_32() throws Exception {
		validate(file(this.SNIPSET_32)).assertNoErrors();
	}

	private String SNIPSET_33 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_33() throws Exception {
		validate(file(this.SNIPSET_33)).assertNoErrors();
	}

	private String SNIPSET_34 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_34() throws Exception {
		validate(file(this.SNIPSET_34)).assertNoErrors();
	}

	private String SNIPSET_35 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_35() throws Exception {
		validate(file(this.SNIPSET_35)).assertNoErrors();
	}

	private String SNIPSET_36 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_36() throws Exception {
		validate(file(this.SNIPSET_36)).assertNoErrors();
	}

	private String SNIPSET_37 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_37() throws Exception {
		validate(file(this.SNIPSET_37)).assertNoErrors();
	}

	private String SNIPSET_38 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_38() throws Exception {
		validate(file(this.SNIPSET_38)).assertNoErrors();
	}

	private String SNIPSET_39 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_39() throws Exception {
		validate(file(this.SNIPSET_39)).assertNoErrors();
	}

	private String SNIPSET_40 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_40() throws Exception {
		validate(file(this.SNIPSET_40)).assertNoErrors();
	}

	private String SNIPSET_41 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_41() throws Exception {
		validate(file(this.SNIPSET_41)).assertNoErrors();
	}

	private String SNIPSET_42 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_42() throws Exception {
		validate(file(this.SNIPSET_42)).assertNoErrors();
	}

	private String SNIPSET_43 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_43() throws Exception {
		validate(file(this.SNIPSET_43)).assertNoErrors();
	}

	private String SNIPSET_44 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_44() throws Exception {
		validate(file(this.SNIPSET_44)).assertNoErrors();
	}

	private String SNIPSET_45 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_45() throws Exception {
		validate(file(this.SNIPSET_45)).assertNoErrors();
	}

	private String SNIPSET_46 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_46() throws Exception {
		validate(file(this.SNIPSET_46)).assertNoErrors();
	}

	private String SNIPSET_47 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_47() throws Exception {
		validate(file(this.SNIPSET_47)).assertNoErrors();
	}

	private String SNIPSET_48 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_48() throws Exception {
		validate(file(this.SNIPSET_48)).assertNoErrors();
	}

	private String SNIPSET_49 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_49() throws Exception {
		validate(file(this.SNIPSET_49)).assertNoErrors();
	}

	private String SNIPSET_50 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_50() throws Exception {
		validate(file(this.SNIPSET_50)).assertNoErrors();
	}

	private String SNIPSET_51 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_51() throws Exception {
		validate(file(this.SNIPSET_51)).assertNoErrors();
	}

	private String SNIPSET_52 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_52() throws Exception {
		validate(file(this.SNIPSET_52)).assertNoErrors();
	}

	private String SNIPSET_53 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_53() throws Exception {
		validate(file(this.SNIPSET_53)).assertNoErrors();
	}

	private String SNIPSET_54 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_54() throws Exception {
		validate(file(this.SNIPSET_54)).assertNoErrors();
	}

	private String SNIPSET_55 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_55() throws Exception {
		validate(file(this.SNIPSET_55)).assertNoErrors();
	}

	private String SNIPSET_56 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_56() throws Exception {
		validate(file(this.SNIPSET_56)).assertNoErrors();
	}

	private String SNIPSET_57 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_57() throws Exception {
		validate(file(this.SNIPSET_57)).assertNoErrors();
	}

	private String SNIPSET_58 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_58() throws Exception {
		validate(file(this.SNIPSET_58)).assertNoErrors();
	}

	private String SNIPSET_59 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_59() throws Exception {
		validate(file(this.SNIPSET_59)).assertNoErrors();
	}

	private String SNIPSET_60 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_60() throws Exception {
		validate(file(this.SNIPSET_60)).assertNoErrors();
	}

	private String SNIPSET_61 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_61() throws Exception {
		validate(file(this.SNIPSET_61)).assertNoErrors();
	}

	private String SNIPSET_62 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_62() throws Exception {
		validate(file(this.SNIPSET_62)).assertNoErrors();
	}

	private String SNIPSET_63 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_63() throws Exception {
		validate(file(this.SNIPSET_63)).assertNoErrors();
	}

	private String SNIPSET_64 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_64() throws Exception {
		validate(file(this.SNIPSET_64)).assertNoErrors();
	}

	private String SNIPSET_65 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_65() throws Exception {
		validate(file(this.SNIPSET_65)).assertNoErrors();
	}

	private String SNIPSET_66 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_66() throws Exception {
		validate(file(this.SNIPSET_66)).assertNoErrors();
	}

	private String SNIPSET_67 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_67() throws Exception {
		validate(file(this.SNIPSET_67)).assertNoErrors();
	}

	private String SNIPSET_68 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_68() throws Exception {
		validate(file(this.SNIPSET_68)).assertNoErrors();
	}

	private String SNIPSET_69 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_69() throws Exception {
		validate(file(this.SNIPSET_69)).assertNoErrors();
	}

	private String SNIPSET_70 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_70() throws Exception {
		validate(file(this.SNIPSET_70)).assertNoErrors();
	}

	private String SNIPSET_71 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_71() throws Exception {
		validate(file(this.SNIPSET_71)).assertNoErrors();
	}

	private String SNIPSET_72 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_72() throws Exception {
		validate(file(this.SNIPSET_72)).assertNoErrors();
	}

	private String SNIPSET_73 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_73() throws Exception {
		validate(file(this.SNIPSET_73)).assertNoErrors();
	}

	private String SNIPSET_74 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_74() throws Exception {
		validate(file(this.SNIPSET_74)).assertNoErrors();
	}

	private String SNIPSET_75 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_75() throws Exception {
		validate(file(this.SNIPSET_75)).assertNoErrors();
	}

	private String SNIPSET_76 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_76() throws Exception {
		validate(file(this.SNIPSET_76)).assertNoErrors();
	}

	private String SNIPSET_77 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_77() throws Exception {
		validate(file(this.SNIPSET_77)).assertNoErrors();
	}

	private String SNIPSET_78 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_78() throws Exception {
		validate(file(this.SNIPSET_78)).assertNoErrors();
	}

	private String SNIPSET_79 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_79() throws Exception {
		validate(file(this.SNIPSET_79)).assertNoErrors();
	}

	private String SNIPSET_80 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_80() throws Exception {
		validate(file(this.SNIPSET_80)).assertNoErrors();
	}

	private String SNIPSET_81 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_81() throws Exception {
		validate(file(this.SNIPSET_81)).assertNoErrors();
	}

	private String SNIPSET_82 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_82() throws Exception {
		validate(file(this.SNIPSET_82)).assertNoErrors();
	}

	private String SNIPSET_83 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_83() throws Exception {
		validate(file(this.SNIPSET_83)).assertNoErrors();
	}

	private String SNIPSET_84 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_84() throws Exception {
		validate(file(this.SNIPSET_84)).assertNoErrors();
	}

	private String SNIPSET_85 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_85() throws Exception {
		validate(file(this.SNIPSET_85)).assertNoErrors();
	}

	private String SNIPSET_86 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_86() throws Exception {
		validate(file(this.SNIPSET_86)).assertNoErrors();
	}

	private String SNIPSET_87 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_87() throws Exception {
		validate(file(this.SNIPSET_87)).assertNoErrors();
	}

	private String SNIPSET_88 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_88() throws Exception {
		validate(file(this.SNIPSET_88)).assertNoErrors();
	}

	private String SNIPSET_89 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_89() throws Exception {
		validate(file(this.SNIPSET_89)).assertNoErrors();
	}

	private String SNIPSET_90 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_90() throws Exception {
		validate(file(this.SNIPSET_90)).assertNoErrors();
	}

	private String SNIPSET_91 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_91() throws Exception {
		validate(file(this.SNIPSET_91)).assertNoErrors();
	}

	private String SNIPSET_92 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_92() throws Exception {
		validate(file(this.SNIPSET_92)).assertNoErrors();
	}

	private String SNIPSET_93 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_93() throws Exception {
		validate(file(this.SNIPSET_93)).assertNoErrors();
	}

	private String SNIPSET_94 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_94() throws Exception {
		validate(file(this.SNIPSET_94)).assertNoErrors();
	}

	private String SNIPSET_95 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_95() throws Exception {
		validate(file(this.SNIPSET_95)).assertNoErrors();
	}

	private String SNIPSET_96 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_96() throws Exception {
		validate(file(this.SNIPSET_96)).assertNoErrors();
	}

	private String SNIPSET_97 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_97() throws Exception {
		validate(file(this.SNIPSET_97)).assertNoErrors();
	}

	private String SNIPSET_98 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_98() throws Exception {
		validate(file(this.SNIPSET_98)).assertNoErrors();
	}

	private String SNIPSET_99 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_99() throws Exception {
		validate(file(this.SNIPSET_99)).assertNoErrors();
	}

	private String SNIPSET_100 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_100() throws Exception {
		validate(file(this.SNIPSET_100)).assertNoErrors();
	}

	private String SNIPSET_101 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_101() throws Exception {
		validate(file(this.SNIPSET_101)).assertNoErrors();
	}

	private String SNIPSET_102 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_102() throws Exception {
		validate(file(this.SNIPSET_102)).assertNoErrors();
	}

	private String SNIPSET_103 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_103() throws Exception {
		validate(file(this.SNIPSET_103)).assertNoErrors();
	}

	private String SNIPSET_104 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_104() throws Exception {
		validate(file(this.SNIPSET_104)).assertNoErrors();
	}

	private String SNIPSET_105 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_105() throws Exception {
		validate(file(this.SNIPSET_105)).assertNoErrors();
	}

	private String SNIPSET_106 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_106() throws Exception {
		validate(file(this.SNIPSET_106)).assertNoErrors();
	}

	private String SNIPSET_107 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_107() throws Exception {
		validate(file(this.SNIPSET_107)).assertNoErrors();
	}

	private String SNIPSET_108 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_108() throws Exception {
		validate(file(this.SNIPSET_108)).assertNoErrors();
	}

	private String SNIPSET_109 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_109() throws Exception {
		validate(file(this.SNIPSET_109)).assertNoErrors();
	}

	private String SNIPSET_110 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_110() throws Exception {
		validate(file(this.SNIPSET_110)).assertNoErrors();
	}

	private String SNIPSET_111 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_111() throws Exception {
		validate(file(this.SNIPSET_111)).assertNoErrors();
	}

	private String SNIPSET_112 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_112() throws Exception {
		validate(file(this.SNIPSET_112)).assertNoErrors();
	}

	private String SNIPSET_113 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_113() throws Exception {
		validate(file(this.SNIPSET_113)).assertNoErrors();
	}

	private String SNIPSET_114 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_114() throws Exception {
		validate(file(this.SNIPSET_114)).assertNoErrors();
	}

	private String SNIPSET_115 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_115() throws Exception {
		validate(file(this.SNIPSET_115)).assertNoErrors();
	}

	private String SNIPSET_116 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_116() throws Exception {
		validate(file(this.SNIPSET_116)).assertNoErrors();
	}

	private String SNIPSET_117 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_117() throws Exception {
		validate(file(this.SNIPSET_117)).assertNoErrors();
	}

	private String SNIPSET_118 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_118() throws Exception {
		validate(file(this.SNIPSET_118)).assertNoErrors();
	}

	private String SNIPSET_119 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_119() throws Exception {
		validate(file(this.SNIPSET_119)).assertNoErrors();
	}

	private String SNIPSET_120 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_120() throws Exception {
		validate(file(this.SNIPSET_120)).assertNoErrors();
	}

	private String SNIPSET_121 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_121() throws Exception {
		validate(file(this.SNIPSET_121)).assertNoErrors();
	}

	private String SNIPSET_122 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_122() throws Exception {
		validate(file(this.SNIPSET_122)).assertNoErrors();
	}

	private String SNIPSET_123 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_123() throws Exception {
		validate(file(this.SNIPSET_123)).assertNoErrors();
	}

	private String SNIPSET_124 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_124() throws Exception {
		validate(file(this.SNIPSET_124)).assertNoErrors();
	}

	private String SNIPSET_125 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_125() throws Exception {
		validate(file(this.SNIPSET_125)).assertNoErrors();
	}

	private String SNIPSET_126 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_126() throws Exception {
		validate(file(this.SNIPSET_126)).assertNoErrors();
	}

	private String SNIPSET_127 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_127() throws Exception {
		validate(file(this.SNIPSET_127)).assertNoErrors();
	}

	private String SNIPSET_128 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_128() throws Exception {
		validate(file(this.SNIPSET_128)).assertNoErrors();
	}

	private String SNIPSET_129 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_129() throws Exception {
		validate(file(this.SNIPSET_129)).assertNoErrors();
	}

	private String SNIPSET_130 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_130() throws Exception {
		validate(file(this.SNIPSET_130)).assertNoErrors();
	}

	private String SNIPSET_131 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_131() throws Exception {
		validate(file(this.SNIPSET_131)).assertNoErrors();
	}

	private String SNIPSET_132 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_132() throws Exception {
		validate(file(this.SNIPSET_132)).assertNoErrors();
	}

	private String SNIPSET_133 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_133() throws Exception {
		validate(file(this.SNIPSET_133)).assertNoErrors();
	}

	private String SNIPSET_134 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_134() throws Exception {
		validate(file(this.SNIPSET_134)).assertNoErrors();
	}

	private String SNIPSET_135 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_135() throws Exception {
		validate(file(this.SNIPSET_135)).assertNoErrors();
	}

	private String SNIPSET_136 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_136() throws Exception {
		validate(file(this.SNIPSET_136)).assertNoErrors();
	}

	private String SNIPSET_137 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_137() throws Exception {
		validate(file(this.SNIPSET_137)).assertNoErrors();
	}

	private String SNIPSET_138 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_138() throws Exception {
		validate(file(this.SNIPSET_138)).assertNoErrors();
	}

	private String SNIPSET_139 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_139() throws Exception {
		validate(file(this.SNIPSET_139)).assertNoErrors();
	}

	private String SNIPSET_140 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_140() throws Exception {
		validate(file(this.SNIPSET_140)).assertNoErrors();
	}

	private String SNIPSET_141 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_141() throws Exception {
		validate(file(this.SNIPSET_141)).assertNoErrors();
	}

	private String SNIPSET_142 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_142() throws Exception {
		validate(file(this.SNIPSET_142)).assertNoErrors();
	}

	private String SNIPSET_143 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_143() throws Exception {
		validate(file(this.SNIPSET_143)).assertNoErrors();
	}

	private String SNIPSET_144 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_144() throws Exception {
		validate(file(this.SNIPSET_144)).assertNoErrors();
	}

	private String SNIPSET_145 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_145() throws Exception {
		validate(file(this.SNIPSET_145)).assertNoErrors();
	}

	private String SNIPSET_146 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_146() throws Exception {
		validate(file(this.SNIPSET_146)).assertNoErrors();
	}

	private String SNIPSET_147 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_147() throws Exception {
		validate(file(this.SNIPSET_147)).assertNoErrors();
	}

	private String SNIPSET_148 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_148() throws Exception {
		validate(file(this.SNIPSET_148)).assertNoErrors();
	}

	private String SNIPSET_149 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_149() throws Exception {
		validate(file(this.SNIPSET_149)).assertNoErrors();
	}

	private String SNIPSET_150 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_150() throws Exception {
		validate(file(this.SNIPSET_150)).assertNoErrors();
	}

	private String SNIPSET_151 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_151() throws Exception {
		validate(file(this.SNIPSET_151)).assertNoErrors();
	}

	private String SNIPSET_152 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_152() throws Exception {
		validate(file(this.SNIPSET_152)).assertNoErrors();
	}

	private String SNIPSET_153 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_153() throws Exception {
		validate(file(this.SNIPSET_153)).assertNoErrors();
	}

	private String SNIPSET_154 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_154() throws Exception {
		validate(file(this.SNIPSET_154)).assertNoErrors();
	}

	private String SNIPSET_155 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_155() throws Exception {
		validate(file(this.SNIPSET_155)).assertNoErrors();
	}

	private String SNIPSET_156 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_156() throws Exception {
		validate(file(this.SNIPSET_156)).assertNoErrors();
	}

	private String SNIPSET_157 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_157() throws Exception {
		validate(file(this.SNIPSET_157)).assertNoErrors();
	}

	private String SNIPSET_158 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_158() throws Exception {
		validate(file(this.SNIPSET_158)).assertNoErrors();
	}

	private String SNIPSET_159 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : int) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_159() throws Exception {
		validate(file(this.SNIPSET_159)).assertNoErrors();
	}

	private String SNIPSET_160 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_160() throws Exception {
		validate(file(this.SNIPSET_160)).assertNoErrors();
	}

	private String SNIPSET_161 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_161() throws Exception {
		validate(file(this.SNIPSET_161)).assertNoErrors();
	}

	private String SNIPSET_162 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_162() throws Exception {
		validate(file(this.SNIPSET_162)).assertNoErrors();
	}

	private String SNIPSET_163 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Integer) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_163() throws Exception {
		validate(file(this.SNIPSET_163)).assertNoErrors();
	}

	private String SNIPSET_164 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_164() throws Exception {
		validate(file(this.SNIPSET_164)).assertNoErrors();
	}

	private String SNIPSET_165 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_165() throws Exception {
		validate(file(this.SNIPSET_165)).assertNoErrors();
	}

	private String SNIPSET_166 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : Short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_166() throws Exception {
		validate(file(this.SNIPSET_166)).assertNoErrors();
	}

	private String SNIPSET_167 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_167() throws Exception {
		validate(file(this.SNIPSET_167)).assertNoErrors();
	}

	private String SNIPSET_168 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : byte, b : AtomicInteger) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_168() throws Exception {
		validate(file(this.SNIPSET_168)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
