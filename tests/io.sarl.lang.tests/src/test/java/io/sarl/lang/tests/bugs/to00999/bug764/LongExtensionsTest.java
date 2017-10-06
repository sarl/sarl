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
public class LongExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_1014 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long) : long {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_1014() throws Exception {
		validate(file(this.SNIPSET_1014)).assertNoErrors();
	}

	private String SNIPSET_1015 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1015() throws Exception {
		validate(file(this.SNIPSET_1015)).assertNoErrors();
	}

	private String SNIPSET_1016 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1016() throws Exception {
		validate(file(this.SNIPSET_1016)).assertNoErrors();
	}

	private String SNIPSET_1017 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1017() throws Exception {
		validate(file(this.SNIPSET_1017)).assertNoErrors();
	}

	private String SNIPSET_1018 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1018() throws Exception {
		validate(file(this.SNIPSET_1018)).assertNoErrors();
	}

	private String SNIPSET_1019 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1019() throws Exception {
		validate(file(this.SNIPSET_1019)).assertNoErrors();
	}

	private String SNIPSET_1020 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1020() throws Exception {
		validate(file(this.SNIPSET_1020)).assertNoErrors();
	}

	private String SNIPSET_1021 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1021() throws Exception {
		validate(file(this.SNIPSET_1021)).assertNoErrors();
	}

	private String SNIPSET_1022 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1022() throws Exception {
		validate(file(this.SNIPSET_1022)).assertNoErrors();
	}

	private String SNIPSET_1023 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1023() throws Exception {
		validate(file(this.SNIPSET_1023)).assertNoErrors();
	}

	private String SNIPSET_1024 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1024() throws Exception {
		validate(file(this.SNIPSET_1024)).assertNoErrors();
	}

	private String SNIPSET_1025 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1025() throws Exception {
		validate(file(this.SNIPSET_1025)).assertNoErrors();
	}

	private String SNIPSET_1026 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1026() throws Exception {
		validate(file(this.SNIPSET_1026)).assertNoErrors();
	}

	private String SNIPSET_1027 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1027() throws Exception {
		validate(file(this.SNIPSET_1027)).assertNoErrors();
	}

	private String SNIPSET_1028 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1028() throws Exception {
		validate(file(this.SNIPSET_1028)).assertNoErrors();
	}

	private String SNIPSET_1029 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1029() throws Exception {
		validate(file(this.SNIPSET_1029)).assertNoErrors();
	}

	private String SNIPSET_1030 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1030() throws Exception {
		validate(file(this.SNIPSET_1030)).assertNoErrors();
	}

	private String SNIPSET_1031 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1031() throws Exception {
		validate(file(this.SNIPSET_1031)).assertNoErrors();
	}

	private String SNIPSET_1032 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1032() throws Exception {
		validate(file(this.SNIPSET_1032)).assertNoErrors();
	}

	private String SNIPSET_1033 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1033() throws Exception {
		validate(file(this.SNIPSET_1033)).assertNoErrors();
	}

	private String SNIPSET_1034 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1034() throws Exception {
		validate(file(this.SNIPSET_1034)).assertNoErrors();
	}

	private String SNIPSET_1035 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1035() throws Exception {
		validate(file(this.SNIPSET_1035)).assertNoErrors();
	}

	private String SNIPSET_1036 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1036() throws Exception {
		validate(file(this.SNIPSET_1036)).assertNoErrors();
	}

	private String SNIPSET_1037 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1037() throws Exception {
		validate(file(this.SNIPSET_1037)).assertNoErrors();
	}

	private String SNIPSET_1038 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1038() throws Exception {
		validate(file(this.SNIPSET_1038)).assertNoErrors();
	}

	private String SNIPSET_1039 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1039() throws Exception {
		validate(file(this.SNIPSET_1039)).assertNoErrors();
	}

	private String SNIPSET_1040 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1040() throws Exception {
		validate(file(this.SNIPSET_1040)).assertNoErrors();
	}

	private String SNIPSET_1041 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1041() throws Exception {
		validate(file(this.SNIPSET_1041)).assertNoErrors();
	}

	private String SNIPSET_1042 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1042() throws Exception {
		validate(file(this.SNIPSET_1042)).assertNoErrors();
	}

	private String SNIPSET_1043 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1043() throws Exception {
		validate(file(this.SNIPSET_1043)).assertNoErrors();
	}

	private String SNIPSET_1044 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1044() throws Exception {
		validate(file(this.SNIPSET_1044)).assertNoErrors();
	}

	private String SNIPSET_1045 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1045() throws Exception {
		validate(file(this.SNIPSET_1045)).assertNoErrors();
	}

	private String SNIPSET_1046 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1046() throws Exception {
		validate(file(this.SNIPSET_1046)).assertNoErrors();
	}

	private String SNIPSET_1047 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1047() throws Exception {
		validate(file(this.SNIPSET_1047)).assertNoErrors();
	}

	private String SNIPSET_1048 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1048() throws Exception {
		validate(file(this.SNIPSET_1048)).assertNoErrors();
	}

	private String SNIPSET_1049 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1049() throws Exception {
		validate(file(this.SNIPSET_1049)).assertNoErrors();
	}

	private String SNIPSET_1050 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1050() throws Exception {
		validate(file(this.SNIPSET_1050)).assertNoErrors();
	}

	private String SNIPSET_1051 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1051() throws Exception {
		validate(file(this.SNIPSET_1051)).assertNoErrors();
	}

	private String SNIPSET_1052 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1052() throws Exception {
		validate(file(this.SNIPSET_1052)).assertNoErrors();
	}

	private String SNIPSET_1053 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1053() throws Exception {
		validate(file(this.SNIPSET_1053)).assertNoErrors();
	}

	private String SNIPSET_1054 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1054() throws Exception {
		validate(file(this.SNIPSET_1054)).assertNoErrors();
	}

	private String SNIPSET_1055 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1055() throws Exception {
		validate(file(this.SNIPSET_1055)).assertNoErrors();
	}

	private String SNIPSET_1056 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1056() throws Exception {
		validate(file(this.SNIPSET_1056)).assertNoErrors();
	}

	private String SNIPSET_1057 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1057() throws Exception {
		validate(file(this.SNIPSET_1057)).assertNoErrors();
	}

	private String SNIPSET_1058 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1058() throws Exception {
		validate(file(this.SNIPSET_1058)).assertNoErrors();
	}

	private String SNIPSET_1059 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1059() throws Exception {
		validate(file(this.SNIPSET_1059)).assertNoErrors();
	}

	private String SNIPSET_1060 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1060() throws Exception {
		validate(file(this.SNIPSET_1060)).assertNoErrors();
	}

	private String SNIPSET_1061 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1061() throws Exception {
		validate(file(this.SNIPSET_1061)).assertNoErrors();
	}

	private String SNIPSET_1062 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1062() throws Exception {
		validate(file(this.SNIPSET_1062)).assertNoErrors();
	}

	private String SNIPSET_1063 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1063() throws Exception {
		validate(file(this.SNIPSET_1063)).assertNoErrors();
	}

	private String SNIPSET_1064 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1064() throws Exception {
		validate(file(this.SNIPSET_1064)).assertNoErrors();
	}

	private String SNIPSET_1065 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1065() throws Exception {
		validate(file(this.SNIPSET_1065)).assertNoErrors();
	}

	private String SNIPSET_1066 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1066() throws Exception {
		validate(file(this.SNIPSET_1066)).assertNoErrors();
	}

	private String SNIPSET_1067 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1067() throws Exception {
		validate(file(this.SNIPSET_1067)).assertNoErrors();
	}

	private String SNIPSET_1068 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1068() throws Exception {
		validate(file(this.SNIPSET_1068)).assertNoErrors();
	}

	private String SNIPSET_1069 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1069() throws Exception {
		validate(file(this.SNIPSET_1069)).assertNoErrors();
	}

	private String SNIPSET_1070 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1070() throws Exception {
		validate(file(this.SNIPSET_1070)).assertNoErrors();
	}

	private String SNIPSET_1071 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1071() throws Exception {
		validate(file(this.SNIPSET_1071)).assertNoErrors();
	}

	private String SNIPSET_1072 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1072() throws Exception {
		validate(file(this.SNIPSET_1072)).assertNoErrors();
	}

	private String SNIPSET_1073 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1073() throws Exception {
		validate(file(this.SNIPSET_1073)).assertNoErrors();
	}

	private String SNIPSET_1074 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1074() throws Exception {
		validate(file(this.SNIPSET_1074)).assertNoErrors();
	}

	private String SNIPSET_1075 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1075() throws Exception {
		validate(file(this.SNIPSET_1075)).assertNoErrors();
	}

	private String SNIPSET_1076 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1076() throws Exception {
		validate(file(this.SNIPSET_1076)).assertNoErrors();
	}

	private String SNIPSET_1077 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1077() throws Exception {
		validate(file(this.SNIPSET_1077)).assertNoErrors();
	}

	private String SNIPSET_1078 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1078() throws Exception {
		validate(file(this.SNIPSET_1078)).assertNoErrors();
	}

	private String SNIPSET_1079 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1079() throws Exception {
		validate(file(this.SNIPSET_1079)).assertNoErrors();
	}

	private String SNIPSET_1080 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1080() throws Exception {
		validate(file(this.SNIPSET_1080)).assertNoErrors();
	}

	private String SNIPSET_1081 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1081() throws Exception {
		validate(file(this.SNIPSET_1081)).assertNoErrors();
	}

	private String SNIPSET_1082 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1082() throws Exception {
		validate(file(this.SNIPSET_1082)).assertNoErrors();
	}

	private String SNIPSET_1083 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1083() throws Exception {
		validate(file(this.SNIPSET_1083)).assertNoErrors();
	}

	private String SNIPSET_1084 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1084() throws Exception {
		validate(file(this.SNIPSET_1084)).assertNoErrors();
	}

	private String SNIPSET_1085 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1085() throws Exception {
		validate(file(this.SNIPSET_1085)).assertNoErrors();
	}

	private String SNIPSET_1086 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1086() throws Exception {
		validate(file(this.SNIPSET_1086)).assertNoErrors();
	}

	private String SNIPSET_1087 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1087() throws Exception {
		validate(file(this.SNIPSET_1087)).assertNoErrors();
	}

	private String SNIPSET_1088 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1088() throws Exception {
		validate(file(this.SNIPSET_1088)).assertNoErrors();
	}

	private String SNIPSET_1089 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1089() throws Exception {
		validate(file(this.SNIPSET_1089)).assertNoErrors();
	}

	private String SNIPSET_1090 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1090() throws Exception {
		validate(file(this.SNIPSET_1090)).assertNoErrors();
	}

	private String SNIPSET_1091 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1091() throws Exception {
		validate(file(this.SNIPSET_1091)).assertNoErrors();
	}

	private String SNIPSET_1092 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1092() throws Exception {
		validate(file(this.SNIPSET_1092)).assertNoErrors();
	}

	private String SNIPSET_1093 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1093() throws Exception {
		validate(file(this.SNIPSET_1093)).assertNoErrors();
	}

	private String SNIPSET_1094 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1094() throws Exception {
		validate(file(this.SNIPSET_1094)).assertNoErrors();
	}

	private String SNIPSET_1095 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1095() throws Exception {
		validate(file(this.SNIPSET_1095)).assertNoErrors();
	}

	private String SNIPSET_1096 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1096() throws Exception {
		validate(file(this.SNIPSET_1096)).assertNoErrors();
	}

	private String SNIPSET_1097 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1097() throws Exception {
		validate(file(this.SNIPSET_1097)).assertNoErrors();
	}

	private String SNIPSET_1098 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1098() throws Exception {
		validate(file(this.SNIPSET_1098)).assertNoErrors();
	}

	private String SNIPSET_1099 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1099() throws Exception {
		validate(file(this.SNIPSET_1099)).assertNoErrors();
	}

	private String SNIPSET_1100 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1100() throws Exception {
		validate(file(this.SNIPSET_1100)).assertNoErrors();
	}

	private String SNIPSET_1101 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1101() throws Exception {
		validate(file(this.SNIPSET_1101)).assertNoErrors();
	}

	private String SNIPSET_1102 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1102() throws Exception {
		validate(file(this.SNIPSET_1102)).assertNoErrors();
	}

	private String SNIPSET_1103 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1103() throws Exception {
		validate(file(this.SNIPSET_1103)).assertNoErrors();
	}

	private String SNIPSET_1104 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1104() throws Exception {
		validate(file(this.SNIPSET_1104)).assertNoErrors();
	}

	private String SNIPSET_1105 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1105() throws Exception {
		validate(file(this.SNIPSET_1105)).assertNoErrors();
	}

	private String SNIPSET_1106 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1106() throws Exception {
		validate(file(this.SNIPSET_1106)).assertNoErrors();
	}

	private String SNIPSET_1107 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1107() throws Exception {
		validate(file(this.SNIPSET_1107)).assertNoErrors();
	}

	private String SNIPSET_1108 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1108() throws Exception {
		validate(file(this.SNIPSET_1108)).assertNoErrors();
	}

	private String SNIPSET_1109 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1109() throws Exception {
		validate(file(this.SNIPSET_1109)).assertNoErrors();
	}

	private String SNIPSET_1110 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1110() throws Exception {
		validate(file(this.SNIPSET_1110)).assertNoErrors();
	}

	private String SNIPSET_1111 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1111() throws Exception {
		validate(file(this.SNIPSET_1111)).assertNoErrors();
	}

	private String SNIPSET_1112 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1112() throws Exception {
		validate(file(this.SNIPSET_1112)).assertNoErrors();
	}

	private String SNIPSET_1113 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1113() throws Exception {
		validate(file(this.SNIPSET_1113)).assertNoErrors();
	}

	private String SNIPSET_1114 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1114() throws Exception {
		validate(file(this.SNIPSET_1114)).assertNoErrors();
	}

	private String SNIPSET_1115 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1115() throws Exception {
		validate(file(this.SNIPSET_1115)).assertNoErrors();
	}

	private String SNIPSET_1116 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1116() throws Exception {
		validate(file(this.SNIPSET_1116)).assertNoErrors();
	}

	private String SNIPSET_1117 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1117() throws Exception {
		validate(file(this.SNIPSET_1117)).assertNoErrors();
	}

	private String SNIPSET_1118 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1118() throws Exception {
		validate(file(this.SNIPSET_1118)).assertNoErrors();
	}

	private String SNIPSET_1119 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1119() throws Exception {
		validate(file(this.SNIPSET_1119)).assertNoErrors();
	}

	private String SNIPSET_1120 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1120() throws Exception {
		validate(file(this.SNIPSET_1120)).assertNoErrors();
	}

	private String SNIPSET_1121 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1121() throws Exception {
		validate(file(this.SNIPSET_1121)).assertNoErrors();
	}

	private String SNIPSET_1122 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1122() throws Exception {
		validate(file(this.SNIPSET_1122)).assertNoErrors();
	}

	private String SNIPSET_1123 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1123() throws Exception {
		validate(file(this.SNIPSET_1123)).assertNoErrors();
	}

	private String SNIPSET_1124 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1124() throws Exception {
		validate(file(this.SNIPSET_1124)).assertNoErrors();
	}

	private String SNIPSET_1125 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1125() throws Exception {
		validate(file(this.SNIPSET_1125)).assertNoErrors();
	}

	private String SNIPSET_1126 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1126() throws Exception {
		validate(file(this.SNIPSET_1126)).assertNoErrors();
	}

	private String SNIPSET_1127 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1127() throws Exception {
		validate(file(this.SNIPSET_1127)).assertNoErrors();
	}

	private String SNIPSET_1128 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1128() throws Exception {
		validate(file(this.SNIPSET_1128)).assertNoErrors();
	}

	private String SNIPSET_1129 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1129() throws Exception {
		validate(file(this.SNIPSET_1129)).assertNoErrors();
	}

	private String SNIPSET_1130 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1130() throws Exception {
		validate(file(this.SNIPSET_1130)).assertNoErrors();
	}

	private String SNIPSET_1131 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1131() throws Exception {
		validate(file(this.SNIPSET_1131)).assertNoErrors();
	}

	private String SNIPSET_1132 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1132() throws Exception {
		validate(file(this.SNIPSET_1132)).assertNoErrors();
	}

	private String SNIPSET_1133 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1133() throws Exception {
		validate(file(this.SNIPSET_1133)).assertNoErrors();
	}

	private String SNIPSET_1134 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1134() throws Exception {
		validate(file(this.SNIPSET_1134)).assertNoErrors();
	}

	private String SNIPSET_1135 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1135() throws Exception {
		validate(file(this.SNIPSET_1135)).assertNoErrors();
	}

	private String SNIPSET_1136 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1136() throws Exception {
		validate(file(this.SNIPSET_1136)).assertNoErrors();
	}

	private String SNIPSET_1137 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1137() throws Exception {
		validate(file(this.SNIPSET_1137)).assertNoErrors();
	}

	private String SNIPSET_1138 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1138() throws Exception {
		validate(file(this.SNIPSET_1138)).assertNoErrors();
	}

	private String SNIPSET_1139 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1139() throws Exception {
		validate(file(this.SNIPSET_1139)).assertNoErrors();
	}

	private String SNIPSET_1140 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1140() throws Exception {
		validate(file(this.SNIPSET_1140)).assertNoErrors();
	}

	private String SNIPSET_1141 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1141() throws Exception {
		validate(file(this.SNIPSET_1141)).assertNoErrors();
	}

	private String SNIPSET_1142 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1142() throws Exception {
		validate(file(this.SNIPSET_1142)).assertNoErrors();
	}

	private String SNIPSET_1143 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1143() throws Exception {
		validate(file(this.SNIPSET_1143)).assertNoErrors();
	}

	private String SNIPSET_1144 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1144() throws Exception {
		validate(file(this.SNIPSET_1144)).assertNoErrors();
	}

	private String SNIPSET_1145 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1145() throws Exception {
		validate(file(this.SNIPSET_1145)).assertNoErrors();
	}

	private String SNIPSET_1146 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1146() throws Exception {
		validate(file(this.SNIPSET_1146)).assertNoErrors();
	}

	private String SNIPSET_1147 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1147() throws Exception {
		validate(file(this.SNIPSET_1147)).assertNoErrors();
	}

	private String SNIPSET_1148 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1148() throws Exception {
		validate(file(this.SNIPSET_1148)).assertNoErrors();
	}

	private String SNIPSET_1149 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1149() throws Exception {
		validate(file(this.SNIPSET_1149)).assertNoErrors();
	}

	private String SNIPSET_1150 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1150() throws Exception {
		validate(file(this.SNIPSET_1150)).assertNoErrors();
	}

	private String SNIPSET_1151 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1151() throws Exception {
		validate(file(this.SNIPSET_1151)).assertNoErrors();
	}

	private String SNIPSET_1152 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1152() throws Exception {
		validate(file(this.SNIPSET_1152)).assertNoErrors();
	}

	private String SNIPSET_1153 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1153() throws Exception {
		validate(file(this.SNIPSET_1153)).assertNoErrors();
	}

	private String SNIPSET_1154 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1154() throws Exception {
		validate(file(this.SNIPSET_1154)).assertNoErrors();
	}

	private String SNIPSET_1155 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1155() throws Exception {
		validate(file(this.SNIPSET_1155)).assertNoErrors();
	}

	private String SNIPSET_1156 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1156() throws Exception {
		validate(file(this.SNIPSET_1156)).assertNoErrors();
	}

	private String SNIPSET_1157 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1157() throws Exception {
		validate(file(this.SNIPSET_1157)).assertNoErrors();
	}

	private String SNIPSET_1158 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1158() throws Exception {
		validate(file(this.SNIPSET_1158)).assertNoErrors();
	}

	private String SNIPSET_1159 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1159() throws Exception {
		validate(file(this.SNIPSET_1159)).assertNoErrors();
	}

	private String SNIPSET_1160 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1160() throws Exception {
		validate(file(this.SNIPSET_1160)).assertNoErrors();
	}

	private String SNIPSET_1161 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1161() throws Exception {
		validate(file(this.SNIPSET_1161)).assertNoErrors();
	}

	private String SNIPSET_1162 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1162() throws Exception {
		validate(file(this.SNIPSET_1162)).assertNoErrors();
	}

	private String SNIPSET_1163 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1163() throws Exception {
		validate(file(this.SNIPSET_1163)).assertNoErrors();
	}

	private String SNIPSET_1164 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1164() throws Exception {
		validate(file(this.SNIPSET_1164)).assertNoErrors();
	}

	private String SNIPSET_1165 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1165() throws Exception {
		validate(file(this.SNIPSET_1165)).assertNoErrors();
	}

	private String SNIPSET_1166 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1166() throws Exception {
		validate(file(this.SNIPSET_1166)).assertNoErrors();
	}

	private String SNIPSET_1167 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1167() throws Exception {
		validate(file(this.SNIPSET_1167)).assertNoErrors();
	}

	private String SNIPSET_1168 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1168() throws Exception {
		validate(file(this.SNIPSET_1168)).assertNoErrors();
	}

	private String SNIPSET_1169 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : byte) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1169() throws Exception {
		validate(file(this.SNIPSET_1169)).assertNoErrors();
	}

	private String SNIPSET_1170 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1170() throws Exception {
		validate(file(this.SNIPSET_1170)).assertNoErrors();
	}

	private String SNIPSET_1171 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1171() throws Exception {
		validate(file(this.SNIPSET_1171)).assertNoErrors();
	}

	private String SNIPSET_1172 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : short) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1172() throws Exception {
		validate(file(this.SNIPSET_1172)).assertNoErrors();
	}

	private String SNIPSET_1173 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : int) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1173() throws Exception {
		validate(file(this.SNIPSET_1173)).assertNoErrors();
	}

	private String SNIPSET_1174 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1174() throws Exception {
		validate(file(this.SNIPSET_1174)).assertNoErrors();
	}

	private String SNIPSET_1175 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1175() throws Exception {
		validate(file(this.SNIPSET_1175)).assertNoErrors();
	}

	private String SNIPSET_1176 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1176() throws Exception {
		validate(file(this.SNIPSET_1176)).assertNoErrors();
	}

	private String SNIPSET_1177 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Integer) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1177() throws Exception {
		validate(file(this.SNIPSET_1177)).assertNoErrors();
	}

	private String SNIPSET_1178 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1178() throws Exception {
		validate(file(this.SNIPSET_1178)).assertNoErrors();
	}

	private String SNIPSET_1179 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Byte) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1179() throws Exception {
		validate(file(this.SNIPSET_1179)).assertNoErrors();
	}

	private String SNIPSET_1180 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : Short) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1180() throws Exception {
		validate(file(this.SNIPSET_1180)).assertNoErrors();
	}

	private String SNIPSET_1181 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1181() throws Exception {
		validate(file(this.SNIPSET_1181)).assertNoErrors();
	}

	private String SNIPSET_1182 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Long, b : AtomicInteger) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1182() throws Exception {
		validate(file(this.SNIPSET_1182)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
