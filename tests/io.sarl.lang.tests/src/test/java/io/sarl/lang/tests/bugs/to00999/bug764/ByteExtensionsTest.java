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
public class ByteExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_1690 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte) : int {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_1690() throws Exception {
		validate(file(this.SNIPSET_1690)).assertNoErrors();
	}

	private String SNIPSET_1691 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1691() throws Exception {
		validate(file(this.SNIPSET_1691)).assertNoErrors();
	}

	private String SNIPSET_1692 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1692() throws Exception {
		validate(file(this.SNIPSET_1692)).assertNoErrors();
	}

	private String SNIPSET_1693 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1693() throws Exception {
		validate(file(this.SNIPSET_1693)).assertNoErrors();
	}

	private String SNIPSET_1694 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1694() throws Exception {
		validate(file(this.SNIPSET_1694)).assertNoErrors();
	}

	private String SNIPSET_1695 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1695() throws Exception {
		validate(file(this.SNIPSET_1695)).assertNoErrors();
	}

	private String SNIPSET_1696 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1696() throws Exception {
		validate(file(this.SNIPSET_1696)).assertNoErrors();
	}

	private String SNIPSET_1697 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1697() throws Exception {
		validate(file(this.SNIPSET_1697)).assertNoErrors();
	}

	private String SNIPSET_1698 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1698() throws Exception {
		validate(file(this.SNIPSET_1698)).assertNoErrors();
	}

	private String SNIPSET_1699 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1699() throws Exception {
		validate(file(this.SNIPSET_1699)).assertNoErrors();
	}

	private String SNIPSET_1700 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1700() throws Exception {
		validate(file(this.SNIPSET_1700)).assertNoErrors();
	}

	private String SNIPSET_1701 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1701() throws Exception {
		validate(file(this.SNIPSET_1701)).assertNoErrors();
	}

	private String SNIPSET_1702 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1702() throws Exception {
		validate(file(this.SNIPSET_1702)).assertNoErrors();
	}

	private String SNIPSET_1703 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1703() throws Exception {
		validate(file(this.SNIPSET_1703)).assertNoErrors();
	}

	private String SNIPSET_1704 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1704() throws Exception {
		validate(file(this.SNIPSET_1704)).assertNoErrors();
	}

	private String SNIPSET_1705 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1705() throws Exception {
		validate(file(this.SNIPSET_1705)).assertNoErrors();
	}

	private String SNIPSET_1706 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1706() throws Exception {
		validate(file(this.SNIPSET_1706)).assertNoErrors();
	}

	private String SNIPSET_1707 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1707() throws Exception {
		validate(file(this.SNIPSET_1707)).assertNoErrors();
	}

	private String SNIPSET_1708 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1708() throws Exception {
		validate(file(this.SNIPSET_1708)).assertNoErrors();
	}

	private String SNIPSET_1709 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1709() throws Exception {
		validate(file(this.SNIPSET_1709)).assertNoErrors();
	}

	private String SNIPSET_1710 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1710() throws Exception {
		validate(file(this.SNIPSET_1710)).assertNoErrors();
	}

	private String SNIPSET_1711 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1711() throws Exception {
		validate(file(this.SNIPSET_1711)).assertNoErrors();
	}

	private String SNIPSET_1712 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1712() throws Exception {
		validate(file(this.SNIPSET_1712)).assertNoErrors();
	}

	private String SNIPSET_1713 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1713() throws Exception {
		validate(file(this.SNIPSET_1713)).assertNoErrors();
	}

	private String SNIPSET_1714 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1714() throws Exception {
		validate(file(this.SNIPSET_1714)).assertNoErrors();
	}

	private String SNIPSET_1715 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1715() throws Exception {
		validate(file(this.SNIPSET_1715)).assertNoErrors();
	}

	private String SNIPSET_1716 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1716() throws Exception {
		validate(file(this.SNIPSET_1716)).assertNoErrors();
	}

	private String SNIPSET_1717 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1717() throws Exception {
		validate(file(this.SNIPSET_1717)).assertNoErrors();
	}

	private String SNIPSET_1718 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1718() throws Exception {
		validate(file(this.SNIPSET_1718)).assertNoErrors();
	}

	private String SNIPSET_1719 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1719() throws Exception {
		validate(file(this.SNIPSET_1719)).assertNoErrors();
	}

	private String SNIPSET_1720 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1720() throws Exception {
		validate(file(this.SNIPSET_1720)).assertNoErrors();
	}

	private String SNIPSET_1721 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1721() throws Exception {
		validate(file(this.SNIPSET_1721)).assertNoErrors();
	}

	private String SNIPSET_1722 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1722() throws Exception {
		validate(file(this.SNIPSET_1722)).assertNoErrors();
	}

	private String SNIPSET_1723 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1723() throws Exception {
		validate(file(this.SNIPSET_1723)).assertNoErrors();
	}

	private String SNIPSET_1724 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1724() throws Exception {
		validate(file(this.SNIPSET_1724)).assertNoErrors();
	}

	private String SNIPSET_1725 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1725() throws Exception {
		validate(file(this.SNIPSET_1725)).assertNoErrors();
	}

	private String SNIPSET_1726 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1726() throws Exception {
		validate(file(this.SNIPSET_1726)).assertNoErrors();
	}

	private String SNIPSET_1727 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1727() throws Exception {
		validate(file(this.SNIPSET_1727)).assertNoErrors();
	}

	private String SNIPSET_1728 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1728() throws Exception {
		validate(file(this.SNIPSET_1728)).assertNoErrors();
	}

	private String SNIPSET_1729 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1729() throws Exception {
		validate(file(this.SNIPSET_1729)).assertNoErrors();
	}

	private String SNIPSET_1730 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1730() throws Exception {
		validate(file(this.SNIPSET_1730)).assertNoErrors();
	}

	private String SNIPSET_1731 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1731() throws Exception {
		validate(file(this.SNIPSET_1731)).assertNoErrors();
	}

	private String SNIPSET_1732 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1732() throws Exception {
		validate(file(this.SNIPSET_1732)).assertNoErrors();
	}

	private String SNIPSET_1733 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1733() throws Exception {
		validate(file(this.SNIPSET_1733)).assertNoErrors();
	}

	private String SNIPSET_1734 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1734() throws Exception {
		validate(file(this.SNIPSET_1734)).assertNoErrors();
	}

	private String SNIPSET_1735 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1735() throws Exception {
		validate(file(this.SNIPSET_1735)).assertNoErrors();
	}

	private String SNIPSET_1736 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1736() throws Exception {
		validate(file(this.SNIPSET_1736)).assertNoErrors();
	}

	private String SNIPSET_1737 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1737() throws Exception {
		validate(file(this.SNIPSET_1737)).assertNoErrors();
	}

	private String SNIPSET_1738 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1738() throws Exception {
		validate(file(this.SNIPSET_1738)).assertNoErrors();
	}

	private String SNIPSET_1739 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1739() throws Exception {
		validate(file(this.SNIPSET_1739)).assertNoErrors();
	}

	private String SNIPSET_1740 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1740() throws Exception {
		validate(file(this.SNIPSET_1740)).assertNoErrors();
	}

	private String SNIPSET_1741 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1741() throws Exception {
		validate(file(this.SNIPSET_1741)).assertNoErrors();
	}

	private String SNIPSET_1742 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1742() throws Exception {
		validate(file(this.SNIPSET_1742)).assertNoErrors();
	}

	private String SNIPSET_1743 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1743() throws Exception {
		validate(file(this.SNIPSET_1743)).assertNoErrors();
	}

	private String SNIPSET_1744 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1744() throws Exception {
		validate(file(this.SNIPSET_1744)).assertNoErrors();
	}

	private String SNIPSET_1745 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1745() throws Exception {
		validate(file(this.SNIPSET_1745)).assertNoErrors();
	}

	private String SNIPSET_1746 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1746() throws Exception {
		validate(file(this.SNIPSET_1746)).assertNoErrors();
	}

	private String SNIPSET_1747 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1747() throws Exception {
		validate(file(this.SNIPSET_1747)).assertNoErrors();
	}

	private String SNIPSET_1748 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1748() throws Exception {
		validate(file(this.SNIPSET_1748)).assertNoErrors();
	}

	private String SNIPSET_1749 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1749() throws Exception {
		validate(file(this.SNIPSET_1749)).assertNoErrors();
	}

	private String SNIPSET_1750 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1750() throws Exception {
		validate(file(this.SNIPSET_1750)).assertNoErrors();
	}

	private String SNIPSET_1751 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1751() throws Exception {
		validate(file(this.SNIPSET_1751)).assertNoErrors();
	}

	private String SNIPSET_1752 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1752() throws Exception {
		validate(file(this.SNIPSET_1752)).assertNoErrors();
	}

	private String SNIPSET_1753 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1753() throws Exception {
		validate(file(this.SNIPSET_1753)).assertNoErrors();
	}

	private String SNIPSET_1754 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1754() throws Exception {
		validate(file(this.SNIPSET_1754)).assertNoErrors();
	}

	private String SNIPSET_1755 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1755() throws Exception {
		validate(file(this.SNIPSET_1755)).assertNoErrors();
	}

	private String SNIPSET_1756 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1756() throws Exception {
		validate(file(this.SNIPSET_1756)).assertNoErrors();
	}

	private String SNIPSET_1757 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1757() throws Exception {
		validate(file(this.SNIPSET_1757)).assertNoErrors();
	}

	private String SNIPSET_1758 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1758() throws Exception {
		validate(file(this.SNIPSET_1758)).assertNoErrors();
	}

	private String SNIPSET_1759 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1759() throws Exception {
		validate(file(this.SNIPSET_1759)).assertNoErrors();
	}

	private String SNIPSET_1760 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1760() throws Exception {
		validate(file(this.SNIPSET_1760)).assertNoErrors();
	}

	private String SNIPSET_1761 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1761() throws Exception {
		validate(file(this.SNIPSET_1761)).assertNoErrors();
	}

	private String SNIPSET_1762 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1762() throws Exception {
		validate(file(this.SNIPSET_1762)).assertNoErrors();
	}

	private String SNIPSET_1763 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1763() throws Exception {
		validate(file(this.SNIPSET_1763)).assertNoErrors();
	}

	private String SNIPSET_1764 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1764() throws Exception {
		validate(file(this.SNIPSET_1764)).assertNoErrors();
	}

	private String SNIPSET_1765 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1765() throws Exception {
		validate(file(this.SNIPSET_1765)).assertNoErrors();
	}

	private String SNIPSET_1766 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1766() throws Exception {
		validate(file(this.SNIPSET_1766)).assertNoErrors();
	}

	private String SNIPSET_1767 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1767() throws Exception {
		validate(file(this.SNIPSET_1767)).assertNoErrors();
	}

	private String SNIPSET_1768 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1768() throws Exception {
		validate(file(this.SNIPSET_1768)).assertNoErrors();
	}

	private String SNIPSET_1769 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1769() throws Exception {
		validate(file(this.SNIPSET_1769)).assertNoErrors();
	}

	private String SNIPSET_1770 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1770() throws Exception {
		validate(file(this.SNIPSET_1770)).assertNoErrors();
	}

	private String SNIPSET_1771 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1771() throws Exception {
		validate(file(this.SNIPSET_1771)).assertNoErrors();
	}

	private String SNIPSET_1772 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1772() throws Exception {
		validate(file(this.SNIPSET_1772)).assertNoErrors();
	}

	private String SNIPSET_1773 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1773() throws Exception {
		validate(file(this.SNIPSET_1773)).assertNoErrors();
	}

	private String SNIPSET_1774 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1774() throws Exception {
		validate(file(this.SNIPSET_1774)).assertNoErrors();
	}

	private String SNIPSET_1775 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1775() throws Exception {
		validate(file(this.SNIPSET_1775)).assertNoErrors();
	}

	private String SNIPSET_1776 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1776() throws Exception {
		validate(file(this.SNIPSET_1776)).assertNoErrors();
	}

	private String SNIPSET_1777 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1777() throws Exception {
		validate(file(this.SNIPSET_1777)).assertNoErrors();
	}

	private String SNIPSET_1778 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1778() throws Exception {
		validate(file(this.SNIPSET_1778)).assertNoErrors();
	}

	private String SNIPSET_1779 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1779() throws Exception {
		validate(file(this.SNIPSET_1779)).assertNoErrors();
	}

	private String SNIPSET_1780 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1780() throws Exception {
		validate(file(this.SNIPSET_1780)).assertNoErrors();
	}

	private String SNIPSET_1781 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1781() throws Exception {
		validate(file(this.SNIPSET_1781)).assertNoErrors();
	}

	private String SNIPSET_1782 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1782() throws Exception {
		validate(file(this.SNIPSET_1782)).assertNoErrors();
	}

	private String SNIPSET_1783 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1783() throws Exception {
		validate(file(this.SNIPSET_1783)).assertNoErrors();
	}

	private String SNIPSET_1784 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1784() throws Exception {
		validate(file(this.SNIPSET_1784)).assertNoErrors();
	}

	private String SNIPSET_1785 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1785() throws Exception {
		validate(file(this.SNIPSET_1785)).assertNoErrors();
	}

	private String SNIPSET_1786 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1786() throws Exception {
		validate(file(this.SNIPSET_1786)).assertNoErrors();
	}

	private String SNIPSET_1787 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1787() throws Exception {
		validate(file(this.SNIPSET_1787)).assertNoErrors();
	}

	private String SNIPSET_1788 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1788() throws Exception {
		validate(file(this.SNIPSET_1788)).assertNoErrors();
	}

	private String SNIPSET_1789 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1789() throws Exception {
		validate(file(this.SNIPSET_1789)).assertNoErrors();
	}

	private String SNIPSET_1790 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1790() throws Exception {
		validate(file(this.SNIPSET_1790)).assertNoErrors();
	}

	private String SNIPSET_1791 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1791() throws Exception {
		validate(file(this.SNIPSET_1791)).assertNoErrors();
	}

	private String SNIPSET_1792 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1792() throws Exception {
		validate(file(this.SNIPSET_1792)).assertNoErrors();
	}

	private String SNIPSET_1793 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1793() throws Exception {
		validate(file(this.SNIPSET_1793)).assertNoErrors();
	}

	private String SNIPSET_1794 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1794() throws Exception {
		validate(file(this.SNIPSET_1794)).assertNoErrors();
	}

	private String SNIPSET_1795 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1795() throws Exception {
		validate(file(this.SNIPSET_1795)).assertNoErrors();
	}

	private String SNIPSET_1796 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1796() throws Exception {
		validate(file(this.SNIPSET_1796)).assertNoErrors();
	}

	private String SNIPSET_1797 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1797() throws Exception {
		validate(file(this.SNIPSET_1797)).assertNoErrors();
	}

	private String SNIPSET_1798 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1798() throws Exception {
		validate(file(this.SNIPSET_1798)).assertNoErrors();
	}

	private String SNIPSET_1799 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1799() throws Exception {
		validate(file(this.SNIPSET_1799)).assertNoErrors();
	}

	private String SNIPSET_1800 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1800() throws Exception {
		validate(file(this.SNIPSET_1800)).assertNoErrors();
	}

	private String SNIPSET_1801 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1801() throws Exception {
		validate(file(this.SNIPSET_1801)).assertNoErrors();
	}

	private String SNIPSET_1802 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1802() throws Exception {
		validate(file(this.SNIPSET_1802)).assertNoErrors();
	}

	private String SNIPSET_1803 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1803() throws Exception {
		validate(file(this.SNIPSET_1803)).assertNoErrors();
	}

	private String SNIPSET_1804 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1804() throws Exception {
		validate(file(this.SNIPSET_1804)).assertNoErrors();
	}

	private String SNIPSET_1805 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1805() throws Exception {
		validate(file(this.SNIPSET_1805)).assertNoErrors();
	}

	private String SNIPSET_1806 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1806() throws Exception {
		validate(file(this.SNIPSET_1806)).assertNoErrors();
	}

	private String SNIPSET_1807 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1807() throws Exception {
		validate(file(this.SNIPSET_1807)).assertNoErrors();
	}

	private String SNIPSET_1808 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1808() throws Exception {
		validate(file(this.SNIPSET_1808)).assertNoErrors();
	}

	private String SNIPSET_1809 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1809() throws Exception {
		validate(file(this.SNIPSET_1809)).assertNoErrors();
	}

	private String SNIPSET_1810 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1810() throws Exception {
		validate(file(this.SNIPSET_1810)).assertNoErrors();
	}

	private String SNIPSET_1811 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1811() throws Exception {
		validate(file(this.SNIPSET_1811)).assertNoErrors();
	}

	private String SNIPSET_1812 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1812() throws Exception {
		validate(file(this.SNIPSET_1812)).assertNoErrors();
	}

	private String SNIPSET_1813 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1813() throws Exception {
		validate(file(this.SNIPSET_1813)).assertNoErrors();
	}

	private String SNIPSET_1814 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1814() throws Exception {
		validate(file(this.SNIPSET_1814)).assertNoErrors();
	}

	private String SNIPSET_1815 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1815() throws Exception {
		validate(file(this.SNIPSET_1815)).assertNoErrors();
	}

	private String SNIPSET_1816 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1816() throws Exception {
		validate(file(this.SNIPSET_1816)).assertNoErrors();
	}

	private String SNIPSET_1817 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1817() throws Exception {
		validate(file(this.SNIPSET_1817)).assertNoErrors();
	}

	private String SNIPSET_1818 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1818() throws Exception {
		validate(file(this.SNIPSET_1818)).assertNoErrors();
	}

	private String SNIPSET_1819 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1819() throws Exception {
		validate(file(this.SNIPSET_1819)).assertNoErrors();
	}

	private String SNIPSET_1820 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1820() throws Exception {
		validate(file(this.SNIPSET_1820)).assertNoErrors();
	}

	private String SNIPSET_1821 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1821() throws Exception {
		validate(file(this.SNIPSET_1821)).assertNoErrors();
	}

	private String SNIPSET_1822 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1822() throws Exception {
		validate(file(this.SNIPSET_1822)).assertNoErrors();
	}

	private String SNIPSET_1823 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1823() throws Exception {
		validate(file(this.SNIPSET_1823)).assertNoErrors();
	}

	private String SNIPSET_1824 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1824() throws Exception {
		validate(file(this.SNIPSET_1824)).assertNoErrors();
	}

	private String SNIPSET_1825 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1825() throws Exception {
		validate(file(this.SNIPSET_1825)).assertNoErrors();
	}

	private String SNIPSET_1826 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1826() throws Exception {
		validate(file(this.SNIPSET_1826)).assertNoErrors();
	}

	private String SNIPSET_1827 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1827() throws Exception {
		validate(file(this.SNIPSET_1827)).assertNoErrors();
	}

	private String SNIPSET_1828 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1828() throws Exception {
		validate(file(this.SNIPSET_1828)).assertNoErrors();
	}

	private String SNIPSET_1829 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1829() throws Exception {
		validate(file(this.SNIPSET_1829)).assertNoErrors();
	}

	private String SNIPSET_1830 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1830() throws Exception {
		validate(file(this.SNIPSET_1830)).assertNoErrors();
	}

	private String SNIPSET_1831 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1831() throws Exception {
		validate(file(this.SNIPSET_1831)).assertNoErrors();
	}

	private String SNIPSET_1832 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1832() throws Exception {
		validate(file(this.SNIPSET_1832)).assertNoErrors();
	}

	private String SNIPSET_1833 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1833() throws Exception {
		validate(file(this.SNIPSET_1833)).assertNoErrors();
	}

	private String SNIPSET_1834 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1834() throws Exception {
		validate(file(this.SNIPSET_1834)).assertNoErrors();
	}

	private String SNIPSET_1835 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1835() throws Exception {
		validate(file(this.SNIPSET_1835)).assertNoErrors();
	}

	private String SNIPSET_1836 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1836() throws Exception {
		validate(file(this.SNIPSET_1836)).assertNoErrors();
	}

	private String SNIPSET_1837 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1837() throws Exception {
		validate(file(this.SNIPSET_1837)).assertNoErrors();
	}

	private String SNIPSET_1838 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1838() throws Exception {
		validate(file(this.SNIPSET_1838)).assertNoErrors();
	}

	private String SNIPSET_1839 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1839() throws Exception {
		validate(file(this.SNIPSET_1839)).assertNoErrors();
	}

	private String SNIPSET_1840 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1840() throws Exception {
		validate(file(this.SNIPSET_1840)).assertNoErrors();
	}

	private String SNIPSET_1841 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1841() throws Exception {
		validate(file(this.SNIPSET_1841)).assertNoErrors();
	}

	private String SNIPSET_1842 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1842() throws Exception {
		validate(file(this.SNIPSET_1842)).assertNoErrors();
	}

	private String SNIPSET_1843 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1843() throws Exception {
		validate(file(this.SNIPSET_1843)).assertNoErrors();
	}

	private String SNIPSET_1844 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1844() throws Exception {
		validate(file(this.SNIPSET_1844)).assertNoErrors();
	}

	private String SNIPSET_1845 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1845() throws Exception {
		validate(file(this.SNIPSET_1845)).assertNoErrors();
	}

	private String SNIPSET_1846 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1846() throws Exception {
		validate(file(this.SNIPSET_1846)).assertNoErrors();
	}

	private String SNIPSET_1847 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1847() throws Exception {
		validate(file(this.SNIPSET_1847)).assertNoErrors();
	}

	private String SNIPSET_1848 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1848() throws Exception {
		validate(file(this.SNIPSET_1848)).assertNoErrors();
	}

	private String SNIPSET_1849 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : int) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1849() throws Exception {
		validate(file(this.SNIPSET_1849)).assertNoErrors();
	}

	private String SNIPSET_1850 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1850() throws Exception {
		validate(file(this.SNIPSET_1850)).assertNoErrors();
	}

	private String SNIPSET_1851 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1851() throws Exception {
		validate(file(this.SNIPSET_1851)).assertNoErrors();
	}

	private String SNIPSET_1852 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1852() throws Exception {
		validate(file(this.SNIPSET_1852)).assertNoErrors();
	}

	private String SNIPSET_1853 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Integer) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1853() throws Exception {
		validate(file(this.SNIPSET_1853)).assertNoErrors();
	}

	private String SNIPSET_1854 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1854() throws Exception {
		validate(file(this.SNIPSET_1854)).assertNoErrors();
	}

	private String SNIPSET_1855 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1855() throws Exception {
		validate(file(this.SNIPSET_1855)).assertNoErrors();
	}

	private String SNIPSET_1856 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : Short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1856() throws Exception {
		validate(file(this.SNIPSET_1856)).assertNoErrors();
	}

	private String SNIPSET_1857 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1857() throws Exception {
		validate(file(this.SNIPSET_1857)).assertNoErrors();
	}

	private String SNIPSET_1858 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Byte, b : AtomicInteger) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1858() throws Exception {
		validate(file(this.SNIPSET_1858)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
