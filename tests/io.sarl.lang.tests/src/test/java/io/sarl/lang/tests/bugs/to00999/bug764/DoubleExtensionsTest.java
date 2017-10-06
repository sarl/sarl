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
public class DoubleExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_1521 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double) : double {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_1521() throws Exception {
		validate(file(this.SNIPSET_1521)).assertNoErrors();
	}

	private String SNIPSET_1522 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1522() throws Exception {
		validate(file(this.SNIPSET_1522)).assertNoErrors();
	}

	private String SNIPSET_1523 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1523() throws Exception {
		validate(file(this.SNIPSET_1523)).assertNoErrors();
	}

	private String SNIPSET_1524 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1524() throws Exception {
		validate(file(this.SNIPSET_1524)).assertNoErrors();
	}

	private String SNIPSET_1525 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1525() throws Exception {
		validate(file(this.SNIPSET_1525)).assertNoErrors();
	}

	private String SNIPSET_1526 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1526() throws Exception {
		validate(file(this.SNIPSET_1526)).assertNoErrors();
	}

	private String SNIPSET_1527 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1527() throws Exception {
		validate(file(this.SNIPSET_1527)).assertNoErrors();
	}

	private String SNIPSET_1528 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1528() throws Exception {
		validate(file(this.SNIPSET_1528)).assertNoErrors();
	}

	private String SNIPSET_1529 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1529() throws Exception {
		validate(file(this.SNIPSET_1529)).assertNoErrors();
	}

	private String SNIPSET_1530 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1530() throws Exception {
		validate(file(this.SNIPSET_1530)).assertNoErrors();
	}

	private String SNIPSET_1531 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1531() throws Exception {
		validate(file(this.SNIPSET_1531)).assertNoErrors();
	}

	private String SNIPSET_1532 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1532() throws Exception {
		validate(file(this.SNIPSET_1532)).assertNoErrors();
	}

	private String SNIPSET_1533 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1533() throws Exception {
		validate(file(this.SNIPSET_1533)).assertNoErrors();
	}

	private String SNIPSET_1534 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1534() throws Exception {
		validate(file(this.SNIPSET_1534)).assertNoErrors();
	}

	private String SNIPSET_1535 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1535() throws Exception {
		validate(file(this.SNIPSET_1535)).assertNoErrors();
	}

	private String SNIPSET_1536 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1536() throws Exception {
		validate(file(this.SNIPSET_1536)).assertNoErrors();
	}

	private String SNIPSET_1537 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1537() throws Exception {
		validate(file(this.SNIPSET_1537)).assertNoErrors();
	}

	private String SNIPSET_1538 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1538() throws Exception {
		validate(file(this.SNIPSET_1538)).assertNoErrors();
	}

	private String SNIPSET_1539 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1539() throws Exception {
		validate(file(this.SNIPSET_1539)).assertNoErrors();
	}

	private String SNIPSET_1540 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1540() throws Exception {
		validate(file(this.SNIPSET_1540)).assertNoErrors();
	}

	private String SNIPSET_1541 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1541() throws Exception {
		validate(file(this.SNIPSET_1541)).assertNoErrors();
	}

	private String SNIPSET_1542 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1542() throws Exception {
		validate(file(this.SNIPSET_1542)).assertNoErrors();
	}

	private String SNIPSET_1543 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1543() throws Exception {
		validate(file(this.SNIPSET_1543)).assertNoErrors();
	}

	private String SNIPSET_1544 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1544() throws Exception {
		validate(file(this.SNIPSET_1544)).assertNoErrors();
	}

	private String SNIPSET_1545 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1545() throws Exception {
		validate(file(this.SNIPSET_1545)).assertNoErrors();
	}

	private String SNIPSET_1546 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1546() throws Exception {
		validate(file(this.SNIPSET_1546)).assertNoErrors();
	}

	private String SNIPSET_1547 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1547() throws Exception {
		validate(file(this.SNIPSET_1547)).assertNoErrors();
	}

	private String SNIPSET_1548 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1548() throws Exception {
		validate(file(this.SNIPSET_1548)).assertNoErrors();
	}

	private String SNIPSET_1549 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1549() throws Exception {
		validate(file(this.SNIPSET_1549)).assertNoErrors();
	}

	private String SNIPSET_1550 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1550() throws Exception {
		validate(file(this.SNIPSET_1550)).assertNoErrors();
	}

	private String SNIPSET_1551 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1551() throws Exception {
		validate(file(this.SNIPSET_1551)).assertNoErrors();
	}

	private String SNIPSET_1552 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1552() throws Exception {
		validate(file(this.SNIPSET_1552)).assertNoErrors();
	}

	private String SNIPSET_1553 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1553() throws Exception {
		validate(file(this.SNIPSET_1553)).assertNoErrors();
	}

	private String SNIPSET_1554 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1554() throws Exception {
		validate(file(this.SNIPSET_1554)).assertNoErrors();
	}

	private String SNIPSET_1555 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1555() throws Exception {
		validate(file(this.SNIPSET_1555)).assertNoErrors();
	}

	private String SNIPSET_1556 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1556() throws Exception {
		validate(file(this.SNIPSET_1556)).assertNoErrors();
	}

	private String SNIPSET_1557 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1557() throws Exception {
		validate(file(this.SNIPSET_1557)).assertNoErrors();
	}

	private String SNIPSET_1558 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1558() throws Exception {
		validate(file(this.SNIPSET_1558)).assertNoErrors();
	}

	private String SNIPSET_1559 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1559() throws Exception {
		validate(file(this.SNIPSET_1559)).assertNoErrors();
	}

	private String SNIPSET_1560 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1560() throws Exception {
		validate(file(this.SNIPSET_1560)).assertNoErrors();
	}

	private String SNIPSET_1561 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1561() throws Exception {
		validate(file(this.SNIPSET_1561)).assertNoErrors();
	}

	private String SNIPSET_1562 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1562() throws Exception {
		validate(file(this.SNIPSET_1562)).assertNoErrors();
	}

	private String SNIPSET_1563 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1563() throws Exception {
		validate(file(this.SNIPSET_1563)).assertNoErrors();
	}

	private String SNIPSET_1564 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1564() throws Exception {
		validate(file(this.SNIPSET_1564)).assertNoErrors();
	}

	private String SNIPSET_1565 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1565() throws Exception {
		validate(file(this.SNIPSET_1565)).assertNoErrors();
	}

	private String SNIPSET_1566 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1566() throws Exception {
		validate(file(this.SNIPSET_1566)).assertNoErrors();
	}

	private String SNIPSET_1567 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1567() throws Exception {
		validate(file(this.SNIPSET_1567)).assertNoErrors();
	}

	private String SNIPSET_1568 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1568() throws Exception {
		validate(file(this.SNIPSET_1568)).assertNoErrors();
	}

	private String SNIPSET_1569 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1569() throws Exception {
		validate(file(this.SNIPSET_1569)).assertNoErrors();
	}

	private String SNIPSET_1570 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1570() throws Exception {
		validate(file(this.SNIPSET_1570)).assertNoErrors();
	}

	private String SNIPSET_1571 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1571() throws Exception {
		validate(file(this.SNIPSET_1571)).assertNoErrors();
	}

	private String SNIPSET_1572 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1572() throws Exception {
		validate(file(this.SNIPSET_1572)).assertNoErrors();
	}

	private String SNIPSET_1573 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1573() throws Exception {
		validate(file(this.SNIPSET_1573)).assertNoErrors();
	}

	private String SNIPSET_1574 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1574() throws Exception {
		validate(file(this.SNIPSET_1574)).assertNoErrors();
	}

	private String SNIPSET_1575 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1575() throws Exception {
		validate(file(this.SNIPSET_1575)).assertNoErrors();
	}

	private String SNIPSET_1576 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1576() throws Exception {
		validate(file(this.SNIPSET_1576)).assertNoErrors();
	}

	private String SNIPSET_1577 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1577() throws Exception {
		validate(file(this.SNIPSET_1577)).assertNoErrors();
	}

	private String SNIPSET_1578 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1578() throws Exception {
		validate(file(this.SNIPSET_1578)).assertNoErrors();
	}

	private String SNIPSET_1579 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1579() throws Exception {
		validate(file(this.SNIPSET_1579)).assertNoErrors();
	}

	private String SNIPSET_1580 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1580() throws Exception {
		validate(file(this.SNIPSET_1580)).assertNoErrors();
	}

	private String SNIPSET_1581 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1581() throws Exception {
		validate(file(this.SNIPSET_1581)).assertNoErrors();
	}

	private String SNIPSET_1582 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1582() throws Exception {
		validate(file(this.SNIPSET_1582)).assertNoErrors();
	}

	private String SNIPSET_1583 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1583() throws Exception {
		validate(file(this.SNIPSET_1583)).assertNoErrors();
	}

	private String SNIPSET_1584 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1584() throws Exception {
		validate(file(this.SNIPSET_1584)).assertNoErrors();
	}

	private String SNIPSET_1585 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1585() throws Exception {
		validate(file(this.SNIPSET_1585)).assertNoErrors();
	}

	private String SNIPSET_1586 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1586() throws Exception {
		validate(file(this.SNIPSET_1586)).assertNoErrors();
	}

	private String SNIPSET_1587 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1587() throws Exception {
		validate(file(this.SNIPSET_1587)).assertNoErrors();
	}

	private String SNIPSET_1588 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1588() throws Exception {
		validate(file(this.SNIPSET_1588)).assertNoErrors();
	}

	private String SNIPSET_1589 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1589() throws Exception {
		validate(file(this.SNIPSET_1589)).assertNoErrors();
	}

	private String SNIPSET_1590 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1590() throws Exception {
		validate(file(this.SNIPSET_1590)).assertNoErrors();
	}

	private String SNIPSET_1591 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1591() throws Exception {
		validate(file(this.SNIPSET_1591)).assertNoErrors();
	}

	private String SNIPSET_1592 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1592() throws Exception {
		validate(file(this.SNIPSET_1592)).assertNoErrors();
	}

	private String SNIPSET_1593 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1593() throws Exception {
		validate(file(this.SNIPSET_1593)).assertNoErrors();
	}

	private String SNIPSET_1594 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1594() throws Exception {
		validate(file(this.SNIPSET_1594)).assertNoErrors();
	}

	private String SNIPSET_1595 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1595() throws Exception {
		validate(file(this.SNIPSET_1595)).assertNoErrors();
	}

	private String SNIPSET_1596 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1596() throws Exception {
		validate(file(this.SNIPSET_1596)).assertNoErrors();
	}

	private String SNIPSET_1597 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1597() throws Exception {
		validate(file(this.SNIPSET_1597)).assertNoErrors();
	}

	private String SNIPSET_1598 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1598() throws Exception {
		validate(file(this.SNIPSET_1598)).assertNoErrors();
	}

	private String SNIPSET_1599 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1599() throws Exception {
		validate(file(this.SNIPSET_1599)).assertNoErrors();
	}

	private String SNIPSET_1600 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1600() throws Exception {
		validate(file(this.SNIPSET_1600)).assertNoErrors();
	}

	private String SNIPSET_1601 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1601() throws Exception {
		validate(file(this.SNIPSET_1601)).assertNoErrors();
	}

	private String SNIPSET_1602 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1602() throws Exception {
		validate(file(this.SNIPSET_1602)).assertNoErrors();
	}

	private String SNIPSET_1603 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1603() throws Exception {
		validate(file(this.SNIPSET_1603)).assertNoErrors();
	}

	private String SNIPSET_1604 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1604() throws Exception {
		validate(file(this.SNIPSET_1604)).assertNoErrors();
	}

	private String SNIPSET_1605 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1605() throws Exception {
		validate(file(this.SNIPSET_1605)).assertNoErrors();
	}

	private String SNIPSET_1606 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1606() throws Exception {
		validate(file(this.SNIPSET_1606)).assertNoErrors();
	}

	private String SNIPSET_1607 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1607() throws Exception {
		validate(file(this.SNIPSET_1607)).assertNoErrors();
	}

	private String SNIPSET_1608 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1608() throws Exception {
		validate(file(this.SNIPSET_1608)).assertNoErrors();
	}

	private String SNIPSET_1609 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1609() throws Exception {
		validate(file(this.SNIPSET_1609)).assertNoErrors();
	}

	private String SNIPSET_1610 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1610() throws Exception {
		validate(file(this.SNIPSET_1610)).assertNoErrors();
	}

	private String SNIPSET_1611 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1611() throws Exception {
		validate(file(this.SNIPSET_1611)).assertNoErrors();
	}

	private String SNIPSET_1612 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1612() throws Exception {
		validate(file(this.SNIPSET_1612)).assertNoErrors();
	}

	private String SNIPSET_1613 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1613() throws Exception {
		validate(file(this.SNIPSET_1613)).assertNoErrors();
	}

	private String SNIPSET_1614 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1614() throws Exception {
		validate(file(this.SNIPSET_1614)).assertNoErrors();
	}

	private String SNIPSET_1615 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1615() throws Exception {
		validate(file(this.SNIPSET_1615)).assertNoErrors();
	}

	private String SNIPSET_1616 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1616() throws Exception {
		validate(file(this.SNIPSET_1616)).assertNoErrors();
	}

	private String SNIPSET_1617 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1617() throws Exception {
		validate(file(this.SNIPSET_1617)).assertNoErrors();
	}

	private String SNIPSET_1618 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1618() throws Exception {
		validate(file(this.SNIPSET_1618)).assertNoErrors();
	}

	private String SNIPSET_1619 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1619() throws Exception {
		validate(file(this.SNIPSET_1619)).assertNoErrors();
	}

	private String SNIPSET_1620 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1620() throws Exception {
		validate(file(this.SNIPSET_1620)).assertNoErrors();
	}

	private String SNIPSET_1621 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1621() throws Exception {
		validate(file(this.SNIPSET_1621)).assertNoErrors();
	}

	private String SNIPSET_1622 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1622() throws Exception {
		validate(file(this.SNIPSET_1622)).assertNoErrors();
	}

	private String SNIPSET_1623 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1623() throws Exception {
		validate(file(this.SNIPSET_1623)).assertNoErrors();
	}

	private String SNIPSET_1624 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1624() throws Exception {
		validate(file(this.SNIPSET_1624)).assertNoErrors();
	}

	private String SNIPSET_1625 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1625() throws Exception {
		validate(file(this.SNIPSET_1625)).assertNoErrors();
	}

	private String SNIPSET_1626 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1626() throws Exception {
		validate(file(this.SNIPSET_1626)).assertNoErrors();
	}

	private String SNIPSET_1627 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1627() throws Exception {
		validate(file(this.SNIPSET_1627)).assertNoErrors();
	}

	private String SNIPSET_1628 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1628() throws Exception {
		validate(file(this.SNIPSET_1628)).assertNoErrors();
	}

	private String SNIPSET_1629 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1629() throws Exception {
		validate(file(this.SNIPSET_1629)).assertNoErrors();
	}

	private String SNIPSET_1630 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1630() throws Exception {
		validate(file(this.SNIPSET_1630)).assertNoErrors();
	}

	private String SNIPSET_1631 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1631() throws Exception {
		validate(file(this.SNIPSET_1631)).assertNoErrors();
	}

	private String SNIPSET_1632 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1632() throws Exception {
		validate(file(this.SNIPSET_1632)).assertNoErrors();
	}

	private String SNIPSET_1633 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1633() throws Exception {
		validate(file(this.SNIPSET_1633)).assertNoErrors();
	}

	private String SNIPSET_1634 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1634() throws Exception {
		validate(file(this.SNIPSET_1634)).assertNoErrors();
	}

	private String SNIPSET_1635 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1635() throws Exception {
		validate(file(this.SNIPSET_1635)).assertNoErrors();
	}

	private String SNIPSET_1636 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1636() throws Exception {
		validate(file(this.SNIPSET_1636)).assertNoErrors();
	}

	private String SNIPSET_1637 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1637() throws Exception {
		validate(file(this.SNIPSET_1637)).assertNoErrors();
	}

	private String SNIPSET_1638 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1638() throws Exception {
		validate(file(this.SNIPSET_1638)).assertNoErrors();
	}

	private String SNIPSET_1639 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1639() throws Exception {
		validate(file(this.SNIPSET_1639)).assertNoErrors();
	}

	private String SNIPSET_1640 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1640() throws Exception {
		validate(file(this.SNIPSET_1640)).assertNoErrors();
	}

	private String SNIPSET_1641 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1641() throws Exception {
		validate(file(this.SNIPSET_1641)).assertNoErrors();
	}

	private String SNIPSET_1642 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1642() throws Exception {
		validate(file(this.SNIPSET_1642)).assertNoErrors();
	}

	private String SNIPSET_1643 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1643() throws Exception {
		validate(file(this.SNIPSET_1643)).assertNoErrors();
	}

	private String SNIPSET_1644 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1644() throws Exception {
		validate(file(this.SNIPSET_1644)).assertNoErrors();
	}

	private String SNIPSET_1645 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1645() throws Exception {
		validate(file(this.SNIPSET_1645)).assertNoErrors();
	}

	private String SNIPSET_1646 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1646() throws Exception {
		validate(file(this.SNIPSET_1646)).assertNoErrors();
	}

	private String SNIPSET_1647 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1647() throws Exception {
		validate(file(this.SNIPSET_1647)).assertNoErrors();
	}

	private String SNIPSET_1648 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1648() throws Exception {
		validate(file(this.SNIPSET_1648)).assertNoErrors();
	}

	private String SNIPSET_1649 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1649() throws Exception {
		validate(file(this.SNIPSET_1649)).assertNoErrors();
	}

	private String SNIPSET_1650 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1650() throws Exception {
		validate(file(this.SNIPSET_1650)).assertNoErrors();
	}

	private String SNIPSET_1651 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1651() throws Exception {
		validate(file(this.SNIPSET_1651)).assertNoErrors();
	}

	private String SNIPSET_1652 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1652() throws Exception {
		validate(file(this.SNIPSET_1652)).assertNoErrors();
	}

	private String SNIPSET_1653 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1653() throws Exception {
		validate(file(this.SNIPSET_1653)).assertNoErrors();
	}

	private String SNIPSET_1654 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1654() throws Exception {
		validate(file(this.SNIPSET_1654)).assertNoErrors();
	}

	private String SNIPSET_1655 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1655() throws Exception {
		validate(file(this.SNIPSET_1655)).assertNoErrors();
	}

	private String SNIPSET_1656 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1656() throws Exception {
		validate(file(this.SNIPSET_1656)).assertNoErrors();
	}

	private String SNIPSET_1657 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1657() throws Exception {
		validate(file(this.SNIPSET_1657)).assertNoErrors();
	}

	private String SNIPSET_1658 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1658() throws Exception {
		validate(file(this.SNIPSET_1658)).assertNoErrors();
	}

	private String SNIPSET_1659 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1659() throws Exception {
		validate(file(this.SNIPSET_1659)).assertNoErrors();
	}

	private String SNIPSET_1660 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1660() throws Exception {
		validate(file(this.SNIPSET_1660)).assertNoErrors();
	}

	private String SNIPSET_1661 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1661() throws Exception {
		validate(file(this.SNIPSET_1661)).assertNoErrors();
	}

	private String SNIPSET_1662 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1662() throws Exception {
		validate(file(this.SNIPSET_1662)).assertNoErrors();
	}

	private String SNIPSET_1663 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1663() throws Exception {
		validate(file(this.SNIPSET_1663)).assertNoErrors();
	}

	private String SNIPSET_1664 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1664() throws Exception {
		validate(file(this.SNIPSET_1664)).assertNoErrors();
	}

	private String SNIPSET_1665 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1665() throws Exception {
		validate(file(this.SNIPSET_1665)).assertNoErrors();
	}

	private String SNIPSET_1666 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1666() throws Exception {
		validate(file(this.SNIPSET_1666)).assertNoErrors();
	}

	private String SNIPSET_1667 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1667() throws Exception {
		validate(file(this.SNIPSET_1667)).assertNoErrors();
	}

	private String SNIPSET_1668 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1668() throws Exception {
		validate(file(this.SNIPSET_1668)).assertNoErrors();
	}

	private String SNIPSET_1669 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1669() throws Exception {
		validate(file(this.SNIPSET_1669)).assertNoErrors();
	}

	private String SNIPSET_1670 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1670() throws Exception {
		validate(file(this.SNIPSET_1670)).assertNoErrors();
	}

	private String SNIPSET_1671 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1671() throws Exception {
		validate(file(this.SNIPSET_1671)).assertNoErrors();
	}

	private String SNIPSET_1672 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1672() throws Exception {
		validate(file(this.SNIPSET_1672)).assertNoErrors();
	}

	private String SNIPSET_1673 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1673() throws Exception {
		validate(file(this.SNIPSET_1673)).assertNoErrors();
	}

	private String SNIPSET_1674 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1674() throws Exception {
		validate(file(this.SNIPSET_1674)).assertNoErrors();
	}

	private String SNIPSET_1675 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1675() throws Exception {
		validate(file(this.SNIPSET_1675)).assertNoErrors();
	}

	private String SNIPSET_1676 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : byte) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1676() throws Exception {
		validate(file(this.SNIPSET_1676)).assertNoErrors();
	}

	private String SNIPSET_1677 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : long) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1677() throws Exception {
		validate(file(this.SNIPSET_1677)).assertNoErrors();
	}

	private String SNIPSET_1678 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1678() throws Exception {
		validate(file(this.SNIPSET_1678)).assertNoErrors();
	}

	private String SNIPSET_1679 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : short) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1679() throws Exception {
		validate(file(this.SNIPSET_1679)).assertNoErrors();
	}

	private String SNIPSET_1680 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : int) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1680() throws Exception {
		validate(file(this.SNIPSET_1680)).assertNoErrors();
	}

	private String SNIPSET_1681 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : float) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1681() throws Exception {
		validate(file(this.SNIPSET_1681)).assertNoErrors();
	}

	private String SNIPSET_1682 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Long) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1682() throws Exception {
		validate(file(this.SNIPSET_1682)).assertNoErrors();
	}

	private String SNIPSET_1683 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Float) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1683() throws Exception {
		validate(file(this.SNIPSET_1683)).assertNoErrors();
	}

	private String SNIPSET_1684 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Integer) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1684() throws Exception {
		validate(file(this.SNIPSET_1684)).assertNoErrors();
	}

	private String SNIPSET_1685 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1685() throws Exception {
		validate(file(this.SNIPSET_1685)).assertNoErrors();
	}

	private String SNIPSET_1686 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Byte) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1686() throws Exception {
		validate(file(this.SNIPSET_1686)).assertNoErrors();
	}

	private String SNIPSET_1687 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : Short) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1687() throws Exception {
		validate(file(this.SNIPSET_1687)).assertNoErrors();
	}

	private String SNIPSET_1688 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicLong) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1688() throws Exception {
		validate(file(this.SNIPSET_1688)).assertNoErrors();
	}

	private String SNIPSET_1689 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Double, b : AtomicInteger) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1689() throws Exception {
		validate(file(this.SNIPSET_1689)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
