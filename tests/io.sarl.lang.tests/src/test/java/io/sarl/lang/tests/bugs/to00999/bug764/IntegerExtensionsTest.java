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
public class IntegerExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_1352 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer) : int {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_1352() throws Exception {
		validate(file(this.SNIPSET_1352)).assertNoErrors();
	}

	private String SNIPSET_1353 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1353() throws Exception {
		validate(file(this.SNIPSET_1353)).assertNoErrors();
	}

	private String SNIPSET_1354 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1354() throws Exception {
		validate(file(this.SNIPSET_1354)).assertNoErrors();
	}

	private String SNIPSET_1355 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1355() throws Exception {
		validate(file(this.SNIPSET_1355)).assertNoErrors();
	}

	private String SNIPSET_1356 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1356() throws Exception {
		validate(file(this.SNIPSET_1356)).assertNoErrors();
	}

	private String SNIPSET_1357 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1357() throws Exception {
		validate(file(this.SNIPSET_1357)).assertNoErrors();
	}

	private String SNIPSET_1358 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1358() throws Exception {
		validate(file(this.SNIPSET_1358)).assertNoErrors();
	}

	private String SNIPSET_1359 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1359() throws Exception {
		validate(file(this.SNIPSET_1359)).assertNoErrors();
	}

	private String SNIPSET_1360 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1360() throws Exception {
		validate(file(this.SNIPSET_1360)).assertNoErrors();
	}

	private String SNIPSET_1361 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1361() throws Exception {
		validate(file(this.SNIPSET_1361)).assertNoErrors();
	}

	private String SNIPSET_1362 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1362() throws Exception {
		validate(file(this.SNIPSET_1362)).assertNoErrors();
	}

	private String SNIPSET_1363 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1363() throws Exception {
		validate(file(this.SNIPSET_1363)).assertNoErrors();
	}

	private String SNIPSET_1364 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1364() throws Exception {
		validate(file(this.SNIPSET_1364)).assertNoErrors();
	}

	private String SNIPSET_1365 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1365() throws Exception {
		validate(file(this.SNIPSET_1365)).assertNoErrors();
	}

	private String SNIPSET_1366 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1366() throws Exception {
		validate(file(this.SNIPSET_1366)).assertNoErrors();
	}

	private String SNIPSET_1367 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1367() throws Exception {
		validate(file(this.SNIPSET_1367)).assertNoErrors();
	}

	private String SNIPSET_1368 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1368() throws Exception {
		validate(file(this.SNIPSET_1368)).assertNoErrors();
	}

	private String SNIPSET_1369 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1369() throws Exception {
		validate(file(this.SNIPSET_1369)).assertNoErrors();
	}

	private String SNIPSET_1370 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1370() throws Exception {
		validate(file(this.SNIPSET_1370)).assertNoErrors();
	}

	private String SNIPSET_1371 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1371() throws Exception {
		validate(file(this.SNIPSET_1371)).assertNoErrors();
	}

	private String SNIPSET_1372 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1372() throws Exception {
		validate(file(this.SNIPSET_1372)).assertNoErrors();
	}

	private String SNIPSET_1373 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1373() throws Exception {
		validate(file(this.SNIPSET_1373)).assertNoErrors();
	}

	private String SNIPSET_1374 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1374() throws Exception {
		validate(file(this.SNIPSET_1374)).assertNoErrors();
	}

	private String SNIPSET_1375 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1375() throws Exception {
		validate(file(this.SNIPSET_1375)).assertNoErrors();
	}

	private String SNIPSET_1376 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1376() throws Exception {
		validate(file(this.SNIPSET_1376)).assertNoErrors();
	}

	private String SNIPSET_1377 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1377() throws Exception {
		validate(file(this.SNIPSET_1377)).assertNoErrors();
	}

	private String SNIPSET_1378 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1378() throws Exception {
		validate(file(this.SNIPSET_1378)).assertNoErrors();
	}

	private String SNIPSET_1379 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1379() throws Exception {
		validate(file(this.SNIPSET_1379)).assertNoErrors();
	}

	private String SNIPSET_1380 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1380() throws Exception {
		validate(file(this.SNIPSET_1380)).assertNoErrors();
	}

	private String SNIPSET_1381 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1381() throws Exception {
		validate(file(this.SNIPSET_1381)).assertNoErrors();
	}

	private String SNIPSET_1382 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1382() throws Exception {
		validate(file(this.SNIPSET_1382)).assertNoErrors();
	}

	private String SNIPSET_1383 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1383() throws Exception {
		validate(file(this.SNIPSET_1383)).assertNoErrors();
	}

	private String SNIPSET_1384 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1384() throws Exception {
		validate(file(this.SNIPSET_1384)).assertNoErrors();
	}

	private String SNIPSET_1385 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1385() throws Exception {
		validate(file(this.SNIPSET_1385)).assertNoErrors();
	}

	private String SNIPSET_1386 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1386() throws Exception {
		validate(file(this.SNIPSET_1386)).assertNoErrors();
	}

	private String SNIPSET_1387 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1387() throws Exception {
		validate(file(this.SNIPSET_1387)).assertNoErrors();
	}

	private String SNIPSET_1388 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1388() throws Exception {
		validate(file(this.SNIPSET_1388)).assertNoErrors();
	}

	private String SNIPSET_1389 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1389() throws Exception {
		validate(file(this.SNIPSET_1389)).assertNoErrors();
	}

	private String SNIPSET_1390 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1390() throws Exception {
		validate(file(this.SNIPSET_1390)).assertNoErrors();
	}

	private String SNIPSET_1391 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1391() throws Exception {
		validate(file(this.SNIPSET_1391)).assertNoErrors();
	}

	private String SNIPSET_1392 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1392() throws Exception {
		validate(file(this.SNIPSET_1392)).assertNoErrors();
	}

	private String SNIPSET_1393 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1393() throws Exception {
		validate(file(this.SNIPSET_1393)).assertNoErrors();
	}

	private String SNIPSET_1394 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1394() throws Exception {
		validate(file(this.SNIPSET_1394)).assertNoErrors();
	}

	private String SNIPSET_1395 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1395() throws Exception {
		validate(file(this.SNIPSET_1395)).assertNoErrors();
	}

	private String SNIPSET_1396 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1396() throws Exception {
		validate(file(this.SNIPSET_1396)).assertNoErrors();
	}

	private String SNIPSET_1397 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1397() throws Exception {
		validate(file(this.SNIPSET_1397)).assertNoErrors();
	}

	private String SNIPSET_1398 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1398() throws Exception {
		validate(file(this.SNIPSET_1398)).assertNoErrors();
	}

	private String SNIPSET_1399 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1399() throws Exception {
		validate(file(this.SNIPSET_1399)).assertNoErrors();
	}

	private String SNIPSET_1400 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1400() throws Exception {
		validate(file(this.SNIPSET_1400)).assertNoErrors();
	}

	private String SNIPSET_1401 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1401() throws Exception {
		validate(file(this.SNIPSET_1401)).assertNoErrors();
	}

	private String SNIPSET_1402 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1402() throws Exception {
		validate(file(this.SNIPSET_1402)).assertNoErrors();
	}

	private String SNIPSET_1403 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1403() throws Exception {
		validate(file(this.SNIPSET_1403)).assertNoErrors();
	}

	private String SNIPSET_1404 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1404() throws Exception {
		validate(file(this.SNIPSET_1404)).assertNoErrors();
	}

	private String SNIPSET_1405 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1405() throws Exception {
		validate(file(this.SNIPSET_1405)).assertNoErrors();
	}

	private String SNIPSET_1406 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1406() throws Exception {
		validate(file(this.SNIPSET_1406)).assertNoErrors();
	}

	private String SNIPSET_1407 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1407() throws Exception {
		validate(file(this.SNIPSET_1407)).assertNoErrors();
	}

	private String SNIPSET_1408 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1408() throws Exception {
		validate(file(this.SNIPSET_1408)).assertNoErrors();
	}

	private String SNIPSET_1409 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1409() throws Exception {
		validate(file(this.SNIPSET_1409)).assertNoErrors();
	}

	private String SNIPSET_1410 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1410() throws Exception {
		validate(file(this.SNIPSET_1410)).assertNoErrors();
	}

	private String SNIPSET_1411 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1411() throws Exception {
		validate(file(this.SNIPSET_1411)).assertNoErrors();
	}

	private String SNIPSET_1412 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1412() throws Exception {
		validate(file(this.SNIPSET_1412)).assertNoErrors();
	}

	private String SNIPSET_1413 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1413() throws Exception {
		validate(file(this.SNIPSET_1413)).assertNoErrors();
	}

	private String SNIPSET_1414 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1414() throws Exception {
		validate(file(this.SNIPSET_1414)).assertNoErrors();
	}

	private String SNIPSET_1415 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1415() throws Exception {
		validate(file(this.SNIPSET_1415)).assertNoErrors();
	}

	private String SNIPSET_1416 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1416() throws Exception {
		validate(file(this.SNIPSET_1416)).assertNoErrors();
	}

	private String SNIPSET_1417 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1417() throws Exception {
		validate(file(this.SNIPSET_1417)).assertNoErrors();
	}

	private String SNIPSET_1418 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1418() throws Exception {
		validate(file(this.SNIPSET_1418)).assertNoErrors();
	}

	private String SNIPSET_1419 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1419() throws Exception {
		validate(file(this.SNIPSET_1419)).assertNoErrors();
	}

	private String SNIPSET_1420 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1420() throws Exception {
		validate(file(this.SNIPSET_1420)).assertNoErrors();
	}

	private String SNIPSET_1421 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1421() throws Exception {
		validate(file(this.SNIPSET_1421)).assertNoErrors();
	}

	private String SNIPSET_1422 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1422() throws Exception {
		validate(file(this.SNIPSET_1422)).assertNoErrors();
	}

	private String SNIPSET_1423 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1423() throws Exception {
		validate(file(this.SNIPSET_1423)).assertNoErrors();
	}

	private String SNIPSET_1424 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1424() throws Exception {
		validate(file(this.SNIPSET_1424)).assertNoErrors();
	}

	private String SNIPSET_1425 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1425() throws Exception {
		validate(file(this.SNIPSET_1425)).assertNoErrors();
	}

	private String SNIPSET_1426 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1426() throws Exception {
		validate(file(this.SNIPSET_1426)).assertNoErrors();
	}

	private String SNIPSET_1427 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1427() throws Exception {
		validate(file(this.SNIPSET_1427)).assertNoErrors();
	}

	private String SNIPSET_1428 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1428() throws Exception {
		validate(file(this.SNIPSET_1428)).assertNoErrors();
	}

	private String SNIPSET_1429 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1429() throws Exception {
		validate(file(this.SNIPSET_1429)).assertNoErrors();
	}

	private String SNIPSET_1430 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1430() throws Exception {
		validate(file(this.SNIPSET_1430)).assertNoErrors();
	}

	private String SNIPSET_1431 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1431() throws Exception {
		validate(file(this.SNIPSET_1431)).assertNoErrors();
	}

	private String SNIPSET_1432 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1432() throws Exception {
		validate(file(this.SNIPSET_1432)).assertNoErrors();
	}

	private String SNIPSET_1433 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1433() throws Exception {
		validate(file(this.SNIPSET_1433)).assertNoErrors();
	}

	private String SNIPSET_1434 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1434() throws Exception {
		validate(file(this.SNIPSET_1434)).assertNoErrors();
	}

	private String SNIPSET_1435 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1435() throws Exception {
		validate(file(this.SNIPSET_1435)).assertNoErrors();
	}

	private String SNIPSET_1436 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1436() throws Exception {
		validate(file(this.SNIPSET_1436)).assertNoErrors();
	}

	private String SNIPSET_1437 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1437() throws Exception {
		validate(file(this.SNIPSET_1437)).assertNoErrors();
	}

	private String SNIPSET_1438 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1438() throws Exception {
		validate(file(this.SNIPSET_1438)).assertNoErrors();
	}

	private String SNIPSET_1439 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1439() throws Exception {
		validate(file(this.SNIPSET_1439)).assertNoErrors();
	}

	private String SNIPSET_1440 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1440() throws Exception {
		validate(file(this.SNIPSET_1440)).assertNoErrors();
	}

	private String SNIPSET_1441 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1441() throws Exception {
		validate(file(this.SNIPSET_1441)).assertNoErrors();
	}

	private String SNIPSET_1442 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1442() throws Exception {
		validate(file(this.SNIPSET_1442)).assertNoErrors();
	}

	private String SNIPSET_1443 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1443() throws Exception {
		validate(file(this.SNIPSET_1443)).assertNoErrors();
	}

	private String SNIPSET_1444 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1444() throws Exception {
		validate(file(this.SNIPSET_1444)).assertNoErrors();
	}

	private String SNIPSET_1445 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1445() throws Exception {
		validate(file(this.SNIPSET_1445)).assertNoErrors();
	}

	private String SNIPSET_1446 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1446() throws Exception {
		validate(file(this.SNIPSET_1446)).assertNoErrors();
	}

	private String SNIPSET_1447 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1447() throws Exception {
		validate(file(this.SNIPSET_1447)).assertNoErrors();
	}

	private String SNIPSET_1448 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1448() throws Exception {
		validate(file(this.SNIPSET_1448)).assertNoErrors();
	}

	private String SNIPSET_1449 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1449() throws Exception {
		validate(file(this.SNIPSET_1449)).assertNoErrors();
	}

	private String SNIPSET_1450 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1450() throws Exception {
		validate(file(this.SNIPSET_1450)).assertNoErrors();
	}

	private String SNIPSET_1451 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1451() throws Exception {
		validate(file(this.SNIPSET_1451)).assertNoErrors();
	}

	private String SNIPSET_1452 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1452() throws Exception {
		validate(file(this.SNIPSET_1452)).assertNoErrors();
	}

	private String SNIPSET_1453 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1453() throws Exception {
		validate(file(this.SNIPSET_1453)).assertNoErrors();
	}

	private String SNIPSET_1454 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1454() throws Exception {
		validate(file(this.SNIPSET_1454)).assertNoErrors();
	}

	private String SNIPSET_1455 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1455() throws Exception {
		validate(file(this.SNIPSET_1455)).assertNoErrors();
	}

	private String SNIPSET_1456 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1456() throws Exception {
		validate(file(this.SNIPSET_1456)).assertNoErrors();
	}

	private String SNIPSET_1457 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1457() throws Exception {
		validate(file(this.SNIPSET_1457)).assertNoErrors();
	}

	private String SNIPSET_1458 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1458() throws Exception {
		validate(file(this.SNIPSET_1458)).assertNoErrors();
	}

	private String SNIPSET_1459 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1459() throws Exception {
		validate(file(this.SNIPSET_1459)).assertNoErrors();
	}

	private String SNIPSET_1460 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1460() throws Exception {
		validate(file(this.SNIPSET_1460)).assertNoErrors();
	}

	private String SNIPSET_1461 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1461() throws Exception {
		validate(file(this.SNIPSET_1461)).assertNoErrors();
	}

	private String SNIPSET_1462 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1462() throws Exception {
		validate(file(this.SNIPSET_1462)).assertNoErrors();
	}

	private String SNIPSET_1463 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1463() throws Exception {
		validate(file(this.SNIPSET_1463)).assertNoErrors();
	}

	private String SNIPSET_1464 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1464() throws Exception {
		validate(file(this.SNIPSET_1464)).assertNoErrors();
	}

	private String SNIPSET_1465 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1465() throws Exception {
		validate(file(this.SNIPSET_1465)).assertNoErrors();
	}

	private String SNIPSET_1466 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1466() throws Exception {
		validate(file(this.SNIPSET_1466)).assertNoErrors();
	}

	private String SNIPSET_1467 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1467() throws Exception {
		validate(file(this.SNIPSET_1467)).assertNoErrors();
	}

	private String SNIPSET_1468 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1468() throws Exception {
		validate(file(this.SNIPSET_1468)).assertNoErrors();
	}

	private String SNIPSET_1469 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1469() throws Exception {
		validate(file(this.SNIPSET_1469)).assertNoErrors();
	}

	private String SNIPSET_1470 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1470() throws Exception {
		validate(file(this.SNIPSET_1470)).assertNoErrors();
	}

	private String SNIPSET_1471 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1471() throws Exception {
		validate(file(this.SNIPSET_1471)).assertNoErrors();
	}

	private String SNIPSET_1472 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1472() throws Exception {
		validate(file(this.SNIPSET_1472)).assertNoErrors();
	}

	private String SNIPSET_1473 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1473() throws Exception {
		validate(file(this.SNIPSET_1473)).assertNoErrors();
	}

	private String SNIPSET_1474 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1474() throws Exception {
		validate(file(this.SNIPSET_1474)).assertNoErrors();
	}

	private String SNIPSET_1475 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1475() throws Exception {
		validate(file(this.SNIPSET_1475)).assertNoErrors();
	}

	private String SNIPSET_1476 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1476() throws Exception {
		validate(file(this.SNIPSET_1476)).assertNoErrors();
	}

	private String SNIPSET_1477 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1477() throws Exception {
		validate(file(this.SNIPSET_1477)).assertNoErrors();
	}

	private String SNIPSET_1478 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1478() throws Exception {
		validate(file(this.SNIPSET_1478)).assertNoErrors();
	}

	private String SNIPSET_1479 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1479() throws Exception {
		validate(file(this.SNIPSET_1479)).assertNoErrors();
	}

	private String SNIPSET_1480 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1480() throws Exception {
		validate(file(this.SNIPSET_1480)).assertNoErrors();
	}

	private String SNIPSET_1481 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1481() throws Exception {
		validate(file(this.SNIPSET_1481)).assertNoErrors();
	}

	private String SNIPSET_1482 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1482() throws Exception {
		validate(file(this.SNIPSET_1482)).assertNoErrors();
	}

	private String SNIPSET_1483 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1483() throws Exception {
		validate(file(this.SNIPSET_1483)).assertNoErrors();
	}

	private String SNIPSET_1484 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1484() throws Exception {
		validate(file(this.SNIPSET_1484)).assertNoErrors();
	}

	private String SNIPSET_1485 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1485() throws Exception {
		validate(file(this.SNIPSET_1485)).assertNoErrors();
	}

	private String SNIPSET_1486 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1486() throws Exception {
		validate(file(this.SNIPSET_1486)).assertNoErrors();
	}

	private String SNIPSET_1487 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1487() throws Exception {
		validate(file(this.SNIPSET_1487)).assertNoErrors();
	}

	private String SNIPSET_1488 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1488() throws Exception {
		validate(file(this.SNIPSET_1488)).assertNoErrors();
	}

	private String SNIPSET_1489 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1489() throws Exception {
		validate(file(this.SNIPSET_1489)).assertNoErrors();
	}

	private String SNIPSET_1490 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1490() throws Exception {
		validate(file(this.SNIPSET_1490)).assertNoErrors();
	}

	private String SNIPSET_1491 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1491() throws Exception {
		validate(file(this.SNIPSET_1491)).assertNoErrors();
	}

	private String SNIPSET_1492 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1492() throws Exception {
		validate(file(this.SNIPSET_1492)).assertNoErrors();
	}

	private String SNIPSET_1493 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1493() throws Exception {
		validate(file(this.SNIPSET_1493)).assertNoErrors();
	}

	private String SNIPSET_1494 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1494() throws Exception {
		validate(file(this.SNIPSET_1494)).assertNoErrors();
	}

	private String SNIPSET_1495 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1495() throws Exception {
		validate(file(this.SNIPSET_1495)).assertNoErrors();
	}

	private String SNIPSET_1496 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1496() throws Exception {
		validate(file(this.SNIPSET_1496)).assertNoErrors();
	}

	private String SNIPSET_1497 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1497() throws Exception {
		validate(file(this.SNIPSET_1497)).assertNoErrors();
	}

	private String SNIPSET_1498 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1498() throws Exception {
		validate(file(this.SNIPSET_1498)).assertNoErrors();
	}

	private String SNIPSET_1499 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1499() throws Exception {
		validate(file(this.SNIPSET_1499)).assertNoErrors();
	}

	private String SNIPSET_1500 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1500() throws Exception {
		validate(file(this.SNIPSET_1500)).assertNoErrors();
	}

	private String SNIPSET_1501 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1501() throws Exception {
		validate(file(this.SNIPSET_1501)).assertNoErrors();
	}

	private String SNIPSET_1502 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1502() throws Exception {
		validate(file(this.SNIPSET_1502)).assertNoErrors();
	}

	private String SNIPSET_1503 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1503() throws Exception {
		validate(file(this.SNIPSET_1503)).assertNoErrors();
	}

	private String SNIPSET_1504 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1504() throws Exception {
		validate(file(this.SNIPSET_1504)).assertNoErrors();
	}

	private String SNIPSET_1505 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1505() throws Exception {
		validate(file(this.SNIPSET_1505)).assertNoErrors();
	}

	private String SNIPSET_1506 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1506() throws Exception {
		validate(file(this.SNIPSET_1506)).assertNoErrors();
	}

	private String SNIPSET_1507 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1507() throws Exception {
		validate(file(this.SNIPSET_1507)).assertNoErrors();
	}

	private String SNIPSET_1508 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1508() throws Exception {
		validate(file(this.SNIPSET_1508)).assertNoErrors();
	}

	private String SNIPSET_1509 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1509() throws Exception {
		validate(file(this.SNIPSET_1509)).assertNoErrors();
	}

	private String SNIPSET_1510 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1510() throws Exception {
		validate(file(this.SNIPSET_1510)).assertNoErrors();
	}

	private String SNIPSET_1511 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : int) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1511() throws Exception {
		validate(file(this.SNIPSET_1511)).assertNoErrors();
	}

	private String SNIPSET_1512 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1512() throws Exception {
		validate(file(this.SNIPSET_1512)).assertNoErrors();
	}

	private String SNIPSET_1513 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1513() throws Exception {
		validate(file(this.SNIPSET_1513)).assertNoErrors();
	}

	private String SNIPSET_1514 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1514() throws Exception {
		validate(file(this.SNIPSET_1514)).assertNoErrors();
	}

	private String SNIPSET_1515 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Integer) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1515() throws Exception {
		validate(file(this.SNIPSET_1515)).assertNoErrors();
	}

	private String SNIPSET_1516 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1516() throws Exception {
		validate(file(this.SNIPSET_1516)).assertNoErrors();
	}

	private String SNIPSET_1517 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1517() throws Exception {
		validate(file(this.SNIPSET_1517)).assertNoErrors();
	}

	private String SNIPSET_1518 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : Short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1518() throws Exception {
		validate(file(this.SNIPSET_1518)).assertNoErrors();
	}

	private String SNIPSET_1519 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1519() throws Exception {
		validate(file(this.SNIPSET_1519)).assertNoErrors();
	}

	private String SNIPSET_1520 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Integer, b : AtomicInteger) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1520() throws Exception {
		validate(file(this.SNIPSET_1520)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
