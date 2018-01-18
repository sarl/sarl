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
public class PrimitiveIntExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_676 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int) : int {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_676() throws Exception {
		validate(file(this.SNIPSET_676)).assertNoErrors();
	}

	private String SNIPSET_677 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_677() throws Exception {
		validate(file(this.SNIPSET_677)).assertNoErrors();
	}

	private String SNIPSET_678 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_678() throws Exception {
		validate(file(this.SNIPSET_678)).assertNoErrors();
	}

	private String SNIPSET_679 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_679() throws Exception {
		validate(file(this.SNIPSET_679)).assertNoErrors();
	}

	private String SNIPSET_680 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_680() throws Exception {
		validate(file(this.SNIPSET_680)).assertNoErrors();
	}

	private String SNIPSET_681 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_681() throws Exception {
		validate(file(this.SNIPSET_681)).assertNoErrors();
	}

	private String SNIPSET_682 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_682() throws Exception {
		validate(file(this.SNIPSET_682)).assertNoErrors();
	}

	private String SNIPSET_683 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_683() throws Exception {
		validate(file(this.SNIPSET_683)).assertNoErrors();
	}

	private String SNIPSET_684 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_684() throws Exception {
		validate(file(this.SNIPSET_684)).assertNoErrors();
	}

	private String SNIPSET_685 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_685() throws Exception {
		validate(file(this.SNIPSET_685)).assertNoErrors();
	}

	private String SNIPSET_686 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_686() throws Exception {
		validate(file(this.SNIPSET_686)).assertNoErrors();
	}

	private String SNIPSET_687 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_687() throws Exception {
		validate(file(this.SNIPSET_687)).assertNoErrors();
	}

	private String SNIPSET_688 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_688() throws Exception {
		validate(file(this.SNIPSET_688)).assertNoErrors();
	}

	private String SNIPSET_689 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_689() throws Exception {
		validate(file(this.SNIPSET_689)).assertNoErrors();
	}

	private String SNIPSET_690 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_690() throws Exception {
		validate(file(this.SNIPSET_690)).assertNoErrors();
	}

	private String SNIPSET_691 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_691() throws Exception {
		validate(file(this.SNIPSET_691)).assertNoErrors();
	}

	private String SNIPSET_692 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_692() throws Exception {
		validate(file(this.SNIPSET_692)).assertNoErrors();
	}

	private String SNIPSET_693 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_693() throws Exception {
		validate(file(this.SNIPSET_693)).assertNoErrors();
	}

	private String SNIPSET_694 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_694() throws Exception {
		validate(file(this.SNIPSET_694)).assertNoErrors();
	}

	private String SNIPSET_695 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_695() throws Exception {
		validate(file(this.SNIPSET_695)).assertNoErrors();
	}

	private String SNIPSET_696 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_696() throws Exception {
		validate(file(this.SNIPSET_696)).assertNoErrors();
	}

	private String SNIPSET_697 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_697() throws Exception {
		validate(file(this.SNIPSET_697)).assertNoErrors();
	}

	private String SNIPSET_698 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_698() throws Exception {
		validate(file(this.SNIPSET_698)).assertNoErrors();
	}

	private String SNIPSET_699 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_699() throws Exception {
		validate(file(this.SNIPSET_699)).assertNoErrors();
	}

	private String SNIPSET_700 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_700() throws Exception {
		validate(file(this.SNIPSET_700)).assertNoErrors();
	}

	private String SNIPSET_701 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_701() throws Exception {
		validate(file(this.SNIPSET_701)).assertNoErrors();
	}

	private String SNIPSET_702 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_702() throws Exception {
		validate(file(this.SNIPSET_702)).assertNoErrors();
	}

	private String SNIPSET_703 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_703() throws Exception {
		validate(file(this.SNIPSET_703)).assertNoErrors();
	}

	private String SNIPSET_704 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_704() throws Exception {
		validate(file(this.SNIPSET_704)).assertNoErrors();
	}

	private String SNIPSET_705 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_705() throws Exception {
		validate(file(this.SNIPSET_705)).assertNoErrors();
	}

	private String SNIPSET_706 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_706() throws Exception {
		validate(file(this.SNIPSET_706)).assertNoErrors();
	}

	private String SNIPSET_707 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_707() throws Exception {
		validate(file(this.SNIPSET_707)).assertNoErrors();
	}

	private String SNIPSET_708 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_708() throws Exception {
		validate(file(this.SNIPSET_708)).assertNoErrors();
	}

	private String SNIPSET_709 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_709() throws Exception {
		validate(file(this.SNIPSET_709)).assertNoErrors();
	}

	private String SNIPSET_710 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_710() throws Exception {
		validate(file(this.SNIPSET_710)).assertNoErrors();
	}

	private String SNIPSET_711 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_711() throws Exception {
		validate(file(this.SNIPSET_711)).assertNoErrors();
	}

	private String SNIPSET_712 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_712() throws Exception {
		validate(file(this.SNIPSET_712)).assertNoErrors();
	}

	private String SNIPSET_713 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_713() throws Exception {
		validate(file(this.SNIPSET_713)).assertNoErrors();
	}

	private String SNIPSET_714 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_714() throws Exception {
		validate(file(this.SNIPSET_714)).assertNoErrors();
	}

	private String SNIPSET_715 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_715() throws Exception {
		validate(file(this.SNIPSET_715)).assertNoErrors();
	}

	private String SNIPSET_716 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_716() throws Exception {
		validate(file(this.SNIPSET_716)).assertNoErrors();
	}

	private String SNIPSET_717 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_717() throws Exception {
		validate(file(this.SNIPSET_717)).assertNoErrors();
	}

	private String SNIPSET_718 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_718() throws Exception {
		validate(file(this.SNIPSET_718)).assertNoErrors();
	}

	private String SNIPSET_719 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_719() throws Exception {
		validate(file(this.SNIPSET_719)).assertNoErrors();
	}

	private String SNIPSET_720 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_720() throws Exception {
		validate(file(this.SNIPSET_720)).assertNoErrors();
	}

	private String SNIPSET_721 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_721() throws Exception {
		validate(file(this.SNIPSET_721)).assertNoErrors();
	}

	private String SNIPSET_722 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_722() throws Exception {
		validate(file(this.SNIPSET_722)).assertNoErrors();
	}

	private String SNIPSET_723 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_723() throws Exception {
		validate(file(this.SNIPSET_723)).assertNoErrors();
	}

	private String SNIPSET_724 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_724() throws Exception {
		validate(file(this.SNIPSET_724)).assertNoErrors();
	}

	private String SNIPSET_725 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_725() throws Exception {
		validate(file(this.SNIPSET_725)).assertNoErrors();
	}

	private String SNIPSET_726 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_726() throws Exception {
		validate(file(this.SNIPSET_726)).assertNoErrors();
	}

	private String SNIPSET_727 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_727() throws Exception {
		validate(file(this.SNIPSET_727)).assertNoErrors();
	}

	private String SNIPSET_728 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_728() throws Exception {
		validate(file(this.SNIPSET_728)).assertNoErrors();
	}

	private String SNIPSET_729 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_729() throws Exception {
		validate(file(this.SNIPSET_729)).assertNoErrors();
	}

	private String SNIPSET_730 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_730() throws Exception {
		validate(file(this.SNIPSET_730)).assertNoErrors();
	}

	private String SNIPSET_731 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_731() throws Exception {
		validate(file(this.SNIPSET_731)).assertNoErrors();
	}

	private String SNIPSET_732 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_732() throws Exception {
		validate(file(this.SNIPSET_732)).assertNoErrors();
	}

	private String SNIPSET_733 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_733() throws Exception {
		validate(file(this.SNIPSET_733)).assertNoErrors();
	}

	private String SNIPSET_734 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_734() throws Exception {
		validate(file(this.SNIPSET_734)).assertNoErrors();
	}

	private String SNIPSET_735 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_735() throws Exception {
		validate(file(this.SNIPSET_735)).assertNoErrors();
	}

	private String SNIPSET_736 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_736() throws Exception {
		validate(file(this.SNIPSET_736)).assertNoErrors();
	}

	private String SNIPSET_737 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_737() throws Exception {
		validate(file(this.SNIPSET_737)).assertNoErrors();
	}

	private String SNIPSET_738 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_738() throws Exception {
		validate(file(this.SNIPSET_738)).assertNoErrors();
	}

	private String SNIPSET_739 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_739() throws Exception {
		validate(file(this.SNIPSET_739)).assertNoErrors();
	}

	private String SNIPSET_740 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_740() throws Exception {
		validate(file(this.SNIPSET_740)).assertNoErrors();
	}

	private String SNIPSET_741 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_741() throws Exception {
		validate(file(this.SNIPSET_741)).assertNoErrors();
	}

	private String SNIPSET_742 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_742() throws Exception {
		validate(file(this.SNIPSET_742)).assertNoErrors();
	}

	private String SNIPSET_743 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_743() throws Exception {
		validate(file(this.SNIPSET_743)).assertNoErrors();
	}

	private String SNIPSET_744 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_744() throws Exception {
		validate(file(this.SNIPSET_744)).assertNoErrors();
	}

	private String SNIPSET_745 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_745() throws Exception {
		validate(file(this.SNIPSET_745)).assertNoErrors();
	}

	private String SNIPSET_746 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_746() throws Exception {
		validate(file(this.SNIPSET_746)).assertNoErrors();
	}

	private String SNIPSET_747 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_747() throws Exception {
		validate(file(this.SNIPSET_747)).assertNoErrors();
	}

	private String SNIPSET_748 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_748() throws Exception {
		validate(file(this.SNIPSET_748)).assertNoErrors();
	}

	private String SNIPSET_749 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_749() throws Exception {
		validate(file(this.SNIPSET_749)).assertNoErrors();
	}

	private String SNIPSET_750 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_750() throws Exception {
		validate(file(this.SNIPSET_750)).assertNoErrors();
	}

	private String SNIPSET_751 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_751() throws Exception {
		validate(file(this.SNIPSET_751)).assertNoErrors();
	}

	private String SNIPSET_752 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_752() throws Exception {
		validate(file(this.SNIPSET_752)).assertNoErrors();
	}

	private String SNIPSET_753 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_753() throws Exception {
		validate(file(this.SNIPSET_753)).assertNoErrors();
	}

	private String SNIPSET_754 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_754() throws Exception {
		validate(file(this.SNIPSET_754)).assertNoErrors();
	}

	private String SNIPSET_755 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_755() throws Exception {
		validate(file(this.SNIPSET_755)).assertNoErrors();
	}

	private String SNIPSET_756 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_756() throws Exception {
		validate(file(this.SNIPSET_756)).assertNoErrors();
	}

	private String SNIPSET_757 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_757() throws Exception {
		validate(file(this.SNIPSET_757)).assertNoErrors();
	}

	private String SNIPSET_758 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_758() throws Exception {
		validate(file(this.SNIPSET_758)).assertNoErrors();
	}

	private String SNIPSET_759 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_759() throws Exception {
		validate(file(this.SNIPSET_759)).assertNoErrors();
	}

	private String SNIPSET_760 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_760() throws Exception {
		validate(file(this.SNIPSET_760)).assertNoErrors();
	}

	private String SNIPSET_761 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_761() throws Exception {
		validate(file(this.SNIPSET_761)).assertNoErrors();
	}

	private String SNIPSET_762 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_762() throws Exception {
		validate(file(this.SNIPSET_762)).assertNoErrors();
	}

	private String SNIPSET_763 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_763() throws Exception {
		validate(file(this.SNIPSET_763)).assertNoErrors();
	}

	private String SNIPSET_764 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_764() throws Exception {
		validate(file(this.SNIPSET_764)).assertNoErrors();
	}

	private String SNIPSET_765 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_765() throws Exception {
		validate(file(this.SNIPSET_765)).assertNoErrors();
	}

	private String SNIPSET_766 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_766() throws Exception {
		validate(file(this.SNIPSET_766)).assertNoErrors();
	}

	private String SNIPSET_767 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_767() throws Exception {
		validate(file(this.SNIPSET_767)).assertNoErrors();
	}

	private String SNIPSET_768 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_768() throws Exception {
		validate(file(this.SNIPSET_768)).assertNoErrors();
	}

	private String SNIPSET_769 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_769() throws Exception {
		validate(file(this.SNIPSET_769)).assertNoErrors();
	}

	private String SNIPSET_770 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_770() throws Exception {
		validate(file(this.SNIPSET_770)).assertNoErrors();
	}

	private String SNIPSET_771 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_771() throws Exception {
		validate(file(this.SNIPSET_771)).assertNoErrors();
	}

	private String SNIPSET_772 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_772() throws Exception {
		validate(file(this.SNIPSET_772)).assertNoErrors();
	}

	private String SNIPSET_773 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_773() throws Exception {
		validate(file(this.SNIPSET_773)).assertNoErrors();
	}

	private String SNIPSET_774 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_774() throws Exception {
		validate(file(this.SNIPSET_774)).assertNoErrors();
	}

	private String SNIPSET_775 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_775() throws Exception {
		validate(file(this.SNIPSET_775)).assertNoErrors();
	}

	private String SNIPSET_776 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_776() throws Exception {
		validate(file(this.SNIPSET_776)).assertNoErrors();
	}

	private String SNIPSET_777 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_777() throws Exception {
		validate(file(this.SNIPSET_777)).assertNoErrors();
	}

	private String SNIPSET_778 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_778() throws Exception {
		validate(file(this.SNIPSET_778)).assertNoErrors();
	}

	private String SNIPSET_779 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_779() throws Exception {
		validate(file(this.SNIPSET_779)).assertNoErrors();
	}

	private String SNIPSET_780 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_780() throws Exception {
		validate(file(this.SNIPSET_780)).assertNoErrors();
	}

	private String SNIPSET_781 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_781() throws Exception {
		validate(file(this.SNIPSET_781)).assertNoErrors();
	}

	private String SNIPSET_782 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_782() throws Exception {
		validate(file(this.SNIPSET_782)).assertNoErrors();
	}

	private String SNIPSET_783 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_783() throws Exception {
		validate(file(this.SNIPSET_783)).assertNoErrors();
	}

	private String SNIPSET_784 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_784() throws Exception {
		validate(file(this.SNIPSET_784)).assertNoErrors();
	}

	private String SNIPSET_785 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_785() throws Exception {
		validate(file(this.SNIPSET_785)).assertNoErrors();
	}

	private String SNIPSET_786 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_786() throws Exception {
		validate(file(this.SNIPSET_786)).assertNoErrors();
	}

	private String SNIPSET_787 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_787() throws Exception {
		validate(file(this.SNIPSET_787)).assertNoErrors();
	}

	private String SNIPSET_788 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_788() throws Exception {
		validate(file(this.SNIPSET_788)).assertNoErrors();
	}

	private String SNIPSET_789 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_789() throws Exception {
		validate(file(this.SNIPSET_789)).assertNoErrors();
	}

	private String SNIPSET_790 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_790() throws Exception {
		validate(file(this.SNIPSET_790)).assertNoErrors();
	}

	private String SNIPSET_791 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_791() throws Exception {
		validate(file(this.SNIPSET_791)).assertNoErrors();
	}

	private String SNIPSET_792 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_792() throws Exception {
		validate(file(this.SNIPSET_792)).assertNoErrors();
	}

	private String SNIPSET_793 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_793() throws Exception {
		validate(file(this.SNIPSET_793)).assertNoErrors();
	}

	private String SNIPSET_794 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_794() throws Exception {
		validate(file(this.SNIPSET_794)).assertNoErrors();
	}

	private String SNIPSET_795 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_795() throws Exception {
		validate(file(this.SNIPSET_795)).assertNoErrors();
	}

	private String SNIPSET_796 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_796() throws Exception {
		validate(file(this.SNIPSET_796)).assertNoErrors();
	}

	private String SNIPSET_797 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_797() throws Exception {
		validate(file(this.SNIPSET_797)).assertNoErrors();
	}

	private String SNIPSET_798 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_798() throws Exception {
		validate(file(this.SNIPSET_798)).assertNoErrors();
	}

	private String SNIPSET_799 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_799() throws Exception {
		validate(file(this.SNIPSET_799)).assertNoErrors();
	}

	private String SNIPSET_800 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_800() throws Exception {
		validate(file(this.SNIPSET_800)).assertNoErrors();
	}

	private String SNIPSET_801 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_801() throws Exception {
		validate(file(this.SNIPSET_801)).assertNoErrors();
	}

	private String SNIPSET_802 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_802() throws Exception {
		validate(file(this.SNIPSET_802)).assertNoErrors();
	}

	private String SNIPSET_803 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_803() throws Exception {
		validate(file(this.SNIPSET_803)).assertNoErrors();
	}

	private String SNIPSET_804 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_804() throws Exception {
		validate(file(this.SNIPSET_804)).assertNoErrors();
	}

	private String SNIPSET_805 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_805() throws Exception {
		validate(file(this.SNIPSET_805)).assertNoErrors();
	}

	private String SNIPSET_806 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_806() throws Exception {
		validate(file(this.SNIPSET_806)).assertNoErrors();
	}

	private String SNIPSET_807 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_807() throws Exception {
		validate(file(this.SNIPSET_807)).assertNoErrors();
	}

	private String SNIPSET_808 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_808() throws Exception {
		validate(file(this.SNIPSET_808)).assertNoErrors();
	}

	private String SNIPSET_809 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_809() throws Exception {
		validate(file(this.SNIPSET_809)).assertNoErrors();
	}

	private String SNIPSET_810 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_810() throws Exception {
		validate(file(this.SNIPSET_810)).assertNoErrors();
	}

	private String SNIPSET_811 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_811() throws Exception {
		validate(file(this.SNIPSET_811)).assertNoErrors();
	}

	private String SNIPSET_812 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_812() throws Exception {
		validate(file(this.SNIPSET_812)).assertNoErrors();
	}

	private String SNIPSET_813 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_813() throws Exception {
		validate(file(this.SNIPSET_813)).assertNoErrors();
	}

	private String SNIPSET_814 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_814() throws Exception {
		validate(file(this.SNIPSET_814)).assertNoErrors();
	}

	private String SNIPSET_815 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_815() throws Exception {
		validate(file(this.SNIPSET_815)).assertNoErrors();
	}

	private String SNIPSET_816 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_816() throws Exception {
		validate(file(this.SNIPSET_816)).assertNoErrors();
	}

	private String SNIPSET_817 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_817() throws Exception {
		validate(file(this.SNIPSET_817)).assertNoErrors();
	}

	private String SNIPSET_818 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_818() throws Exception {
		validate(file(this.SNIPSET_818)).assertNoErrors();
	}

	private String SNIPSET_819 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_819() throws Exception {
		validate(file(this.SNIPSET_819)).assertNoErrors();
	}

	private String SNIPSET_820 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_820() throws Exception {
		validate(file(this.SNIPSET_820)).assertNoErrors();
	}

	private String SNIPSET_821 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_821() throws Exception {
		validate(file(this.SNIPSET_821)).assertNoErrors();
	}

	private String SNIPSET_822 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_822() throws Exception {
		validate(file(this.SNIPSET_822)).assertNoErrors();
	}

	private String SNIPSET_823 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_823() throws Exception {
		validate(file(this.SNIPSET_823)).assertNoErrors();
	}

	private String SNIPSET_824 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_824() throws Exception {
		validate(file(this.SNIPSET_824)).assertNoErrors();
	}

	private String SNIPSET_825 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_825() throws Exception {
		validate(file(this.SNIPSET_825)).assertNoErrors();
	}

	private String SNIPSET_826 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_826() throws Exception {
		validate(file(this.SNIPSET_826)).assertNoErrors();
	}

	private String SNIPSET_827 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_827() throws Exception {
		validate(file(this.SNIPSET_827)).assertNoErrors();
	}

	private String SNIPSET_828 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_828() throws Exception {
		validate(file(this.SNIPSET_828)).assertNoErrors();
	}

	private String SNIPSET_829 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_829() throws Exception {
		validate(file(this.SNIPSET_829)).assertNoErrors();
	}

	private String SNIPSET_830 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_830() throws Exception {
		validate(file(this.SNIPSET_830)).assertNoErrors();
	}

	private String SNIPSET_831 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_831() throws Exception {
		validate(file(this.SNIPSET_831)).assertNoErrors();
	}

	private String SNIPSET_832 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_832() throws Exception {
		validate(file(this.SNIPSET_832)).assertNoErrors();
	}

	private String SNIPSET_833 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_833() throws Exception {
		validate(file(this.SNIPSET_833)).assertNoErrors();
	}

	private String SNIPSET_834 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_834() throws Exception {
		validate(file(this.SNIPSET_834)).assertNoErrors();
	}

	private String SNIPSET_835 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : int) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_835() throws Exception {
		validate(file(this.SNIPSET_835)).assertNoErrors();
	}

	private String SNIPSET_836 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_836() throws Exception {
		validate(file(this.SNIPSET_836)).assertNoErrors();
	}

	private String SNIPSET_837 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_837() throws Exception {
		validate(file(this.SNIPSET_837)).assertNoErrors();
	}

	private String SNIPSET_838 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_838() throws Exception {
		validate(file(this.SNIPSET_838)).assertNoErrors();
	}

	private String SNIPSET_839 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Integer) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_839() throws Exception {
		validate(file(this.SNIPSET_839)).assertNoErrors();
	}

	private String SNIPSET_840 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_840() throws Exception {
		validate(file(this.SNIPSET_840)).assertNoErrors();
	}

	private String SNIPSET_841 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_841() throws Exception {
		validate(file(this.SNIPSET_841)).assertNoErrors();
	}

	private String SNIPSET_842 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : Short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_842() throws Exception {
		validate(file(this.SNIPSET_842)).assertNoErrors();
	}

	private String SNIPSET_843 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_843() throws Exception {
		validate(file(this.SNIPSET_843)).assertNoErrors();
	}

	private String SNIPSET_844 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : int, b : AtomicInteger) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_844() throws Exception {
		validate(file(this.SNIPSET_844)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
