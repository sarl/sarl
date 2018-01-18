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
public class PrimitiveFloatExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_845 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float) : float {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_845() throws Exception {
		validate(file(this.SNIPSET_845)).assertNoErrors();
	}

	private String SNIPSET_846 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_846() throws Exception {
		validate(file(this.SNIPSET_846)).assertNoErrors();
	}

	private String SNIPSET_847 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_847() throws Exception {
		validate(file(this.SNIPSET_847)).assertNoErrors();
	}

	private String SNIPSET_848 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_848() throws Exception {
		validate(file(this.SNIPSET_848)).assertNoErrors();
	}

	private String SNIPSET_849 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_849() throws Exception {
		validate(file(this.SNIPSET_849)).assertNoErrors();
	}

	private String SNIPSET_850 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_850() throws Exception {
		validate(file(this.SNIPSET_850)).assertNoErrors();
	}

	private String SNIPSET_851 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_851() throws Exception {
		validate(file(this.SNIPSET_851)).assertNoErrors();
	}

	private String SNIPSET_852 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_852() throws Exception {
		validate(file(this.SNIPSET_852)).assertNoErrors();
	}

	private String SNIPSET_853 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_853() throws Exception {
		validate(file(this.SNIPSET_853)).assertNoErrors();
	}

	private String SNIPSET_854 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_854() throws Exception {
		validate(file(this.SNIPSET_854)).assertNoErrors();
	}

	private String SNIPSET_855 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_855() throws Exception {
		validate(file(this.SNIPSET_855)).assertNoErrors();
	}

	private String SNIPSET_856 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_856() throws Exception {
		validate(file(this.SNIPSET_856)).assertNoErrors();
	}

	private String SNIPSET_857 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_857() throws Exception {
		validate(file(this.SNIPSET_857)).assertNoErrors();
	}

	private String SNIPSET_858 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_858() throws Exception {
		validate(file(this.SNIPSET_858)).assertNoErrors();
	}

	private String SNIPSET_859 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_859() throws Exception {
		validate(file(this.SNIPSET_859)).assertNoErrors();
	}

	private String SNIPSET_860 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_860() throws Exception {
		validate(file(this.SNIPSET_860)).assertNoErrors();
	}

	private String SNIPSET_861 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_861() throws Exception {
		validate(file(this.SNIPSET_861)).assertNoErrors();
	}

	private String SNIPSET_862 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_862() throws Exception {
		validate(file(this.SNIPSET_862)).assertNoErrors();
	}

	private String SNIPSET_863 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_863() throws Exception {
		validate(file(this.SNIPSET_863)).assertNoErrors();
	}

	private String SNIPSET_864 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_864() throws Exception {
		validate(file(this.SNIPSET_864)).assertNoErrors();
	}

	private String SNIPSET_865 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_865() throws Exception {
		validate(file(this.SNIPSET_865)).assertNoErrors();
	}

	private String SNIPSET_866 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_866() throws Exception {
		validate(file(this.SNIPSET_866)).assertNoErrors();
	}

	private String SNIPSET_867 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_867() throws Exception {
		validate(file(this.SNIPSET_867)).assertNoErrors();
	}

	private String SNIPSET_868 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_868() throws Exception {
		validate(file(this.SNIPSET_868)).assertNoErrors();
	}

	private String SNIPSET_869 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_869() throws Exception {
		validate(file(this.SNIPSET_869)).assertNoErrors();
	}

	private String SNIPSET_870 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_870() throws Exception {
		validate(file(this.SNIPSET_870)).assertNoErrors();
	}

	private String SNIPSET_871 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_871() throws Exception {
		validate(file(this.SNIPSET_871)).assertNoErrors();
	}

	private String SNIPSET_872 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_872() throws Exception {
		validate(file(this.SNIPSET_872)).assertNoErrors();
	}

	private String SNIPSET_873 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_873() throws Exception {
		validate(file(this.SNIPSET_873)).assertNoErrors();
	}

	private String SNIPSET_874 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_874() throws Exception {
		validate(file(this.SNIPSET_874)).assertNoErrors();
	}

	private String SNIPSET_875 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_875() throws Exception {
		validate(file(this.SNIPSET_875)).assertNoErrors();
	}

	private String SNIPSET_876 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_876() throws Exception {
		validate(file(this.SNIPSET_876)).assertNoErrors();
	}

	private String SNIPSET_877 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_877() throws Exception {
		validate(file(this.SNIPSET_877)).assertNoErrors();
	}

	private String SNIPSET_878 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_878() throws Exception {
		validate(file(this.SNIPSET_878)).assertNoErrors();
	}

	private String SNIPSET_879 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_879() throws Exception {
		validate(file(this.SNIPSET_879)).assertNoErrors();
	}

	private String SNIPSET_880 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_880() throws Exception {
		validate(file(this.SNIPSET_880)).assertNoErrors();
	}

	private String SNIPSET_881 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_881() throws Exception {
		validate(file(this.SNIPSET_881)).assertNoErrors();
	}

	private String SNIPSET_882 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_882() throws Exception {
		validate(file(this.SNIPSET_882)).assertNoErrors();
	}

	private String SNIPSET_883 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_883() throws Exception {
		validate(file(this.SNIPSET_883)).assertNoErrors();
	}

	private String SNIPSET_884 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_884() throws Exception {
		validate(file(this.SNIPSET_884)).assertNoErrors();
	}

	private String SNIPSET_885 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_885() throws Exception {
		validate(file(this.SNIPSET_885)).assertNoErrors();
	}

	private String SNIPSET_886 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_886() throws Exception {
		validate(file(this.SNIPSET_886)).assertNoErrors();
	}

	private String SNIPSET_887 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_887() throws Exception {
		validate(file(this.SNIPSET_887)).assertNoErrors();
	}

	private String SNIPSET_888 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_888() throws Exception {
		validate(file(this.SNIPSET_888)).assertNoErrors();
	}

	private String SNIPSET_889 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_889() throws Exception {
		validate(file(this.SNIPSET_889)).assertNoErrors();
	}

	private String SNIPSET_890 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_890() throws Exception {
		validate(file(this.SNIPSET_890)).assertNoErrors();
	}

	private String SNIPSET_891 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_891() throws Exception {
		validate(file(this.SNIPSET_891)).assertNoErrors();
	}

	private String SNIPSET_892 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_892() throws Exception {
		validate(file(this.SNIPSET_892)).assertNoErrors();
	}

	private String SNIPSET_893 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_893() throws Exception {
		validate(file(this.SNIPSET_893)).assertNoErrors();
	}

	private String SNIPSET_894 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_894() throws Exception {
		validate(file(this.SNIPSET_894)).assertNoErrors();
	}

	private String SNIPSET_895 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_895() throws Exception {
		validate(file(this.SNIPSET_895)).assertNoErrors();
	}

	private String SNIPSET_896 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_896() throws Exception {
		validate(file(this.SNIPSET_896)).assertNoErrors();
	}

	private String SNIPSET_897 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_897() throws Exception {
		validate(file(this.SNIPSET_897)).assertNoErrors();
	}

	private String SNIPSET_898 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_898() throws Exception {
		validate(file(this.SNIPSET_898)).assertNoErrors();
	}

	private String SNIPSET_899 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_899() throws Exception {
		validate(file(this.SNIPSET_899)).assertNoErrors();
	}

	private String SNIPSET_900 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_900() throws Exception {
		validate(file(this.SNIPSET_900)).assertNoErrors();
	}

	private String SNIPSET_901 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_901() throws Exception {
		validate(file(this.SNIPSET_901)).assertNoErrors();
	}

	private String SNIPSET_902 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_902() throws Exception {
		validate(file(this.SNIPSET_902)).assertNoErrors();
	}

	private String SNIPSET_903 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_903() throws Exception {
		validate(file(this.SNIPSET_903)).assertNoErrors();
	}

	private String SNIPSET_904 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_904() throws Exception {
		validate(file(this.SNIPSET_904)).assertNoErrors();
	}

	private String SNIPSET_905 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_905() throws Exception {
		validate(file(this.SNIPSET_905)).assertNoErrors();
	}

	private String SNIPSET_906 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_906() throws Exception {
		validate(file(this.SNIPSET_906)).assertNoErrors();
	}

	private String SNIPSET_907 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_907() throws Exception {
		validate(file(this.SNIPSET_907)).assertNoErrors();
	}

	private String SNIPSET_908 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_908() throws Exception {
		validate(file(this.SNIPSET_908)).assertNoErrors();
	}

	private String SNIPSET_909 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_909() throws Exception {
		validate(file(this.SNIPSET_909)).assertNoErrors();
	}

	private String SNIPSET_910 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_910() throws Exception {
		validate(file(this.SNIPSET_910)).assertNoErrors();
	}

	private String SNIPSET_911 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_911() throws Exception {
		validate(file(this.SNIPSET_911)).assertNoErrors();
	}

	private String SNIPSET_912 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_912() throws Exception {
		validate(file(this.SNIPSET_912)).assertNoErrors();
	}

	private String SNIPSET_913 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_913() throws Exception {
		validate(file(this.SNIPSET_913)).assertNoErrors();
	}

	private String SNIPSET_914 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_914() throws Exception {
		validate(file(this.SNIPSET_914)).assertNoErrors();
	}

	private String SNIPSET_915 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_915() throws Exception {
		validate(file(this.SNIPSET_915)).assertNoErrors();
	}

	private String SNIPSET_916 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_916() throws Exception {
		validate(file(this.SNIPSET_916)).assertNoErrors();
	}

	private String SNIPSET_917 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_917() throws Exception {
		validate(file(this.SNIPSET_917)).assertNoErrors();
	}

	private String SNIPSET_918 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_918() throws Exception {
		validate(file(this.SNIPSET_918)).assertNoErrors();
	}

	private String SNIPSET_919 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_919() throws Exception {
		validate(file(this.SNIPSET_919)).assertNoErrors();
	}

	private String SNIPSET_920 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_920() throws Exception {
		validate(file(this.SNIPSET_920)).assertNoErrors();
	}

	private String SNIPSET_921 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_921() throws Exception {
		validate(file(this.SNIPSET_921)).assertNoErrors();
	}

	private String SNIPSET_922 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_922() throws Exception {
		validate(file(this.SNIPSET_922)).assertNoErrors();
	}

	private String SNIPSET_923 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_923() throws Exception {
		validate(file(this.SNIPSET_923)).assertNoErrors();
	}

	private String SNIPSET_924 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_924() throws Exception {
		validate(file(this.SNIPSET_924)).assertNoErrors();
	}

	private String SNIPSET_925 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_925() throws Exception {
		validate(file(this.SNIPSET_925)).assertNoErrors();
	}

	private String SNIPSET_926 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_926() throws Exception {
		validate(file(this.SNIPSET_926)).assertNoErrors();
	}

	private String SNIPSET_927 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_927() throws Exception {
		validate(file(this.SNIPSET_927)).assertNoErrors();
	}

	private String SNIPSET_928 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_928() throws Exception {
		validate(file(this.SNIPSET_928)).assertNoErrors();
	}

	private String SNIPSET_929 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_929() throws Exception {
		validate(file(this.SNIPSET_929)).assertNoErrors();
	}

	private String SNIPSET_930 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_930() throws Exception {
		validate(file(this.SNIPSET_930)).assertNoErrors();
	}

	private String SNIPSET_931 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_931() throws Exception {
		validate(file(this.SNIPSET_931)).assertNoErrors();
	}

	private String SNIPSET_932 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_932() throws Exception {
		validate(file(this.SNIPSET_932)).assertNoErrors();
	}

	private String SNIPSET_933 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_933() throws Exception {
		validate(file(this.SNIPSET_933)).assertNoErrors();
	}

	private String SNIPSET_934 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_934() throws Exception {
		validate(file(this.SNIPSET_934)).assertNoErrors();
	}

	private String SNIPSET_935 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_935() throws Exception {
		validate(file(this.SNIPSET_935)).assertNoErrors();
	}

	private String SNIPSET_936 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_936() throws Exception {
		validate(file(this.SNIPSET_936)).assertNoErrors();
	}

	private String SNIPSET_937 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_937() throws Exception {
		validate(file(this.SNIPSET_937)).assertNoErrors();
	}

	private String SNIPSET_938 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_938() throws Exception {
		validate(file(this.SNIPSET_938)).assertNoErrors();
	}

	private String SNIPSET_939 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_939() throws Exception {
		validate(file(this.SNIPSET_939)).assertNoErrors();
	}

	private String SNIPSET_940 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_940() throws Exception {
		validate(file(this.SNIPSET_940)).assertNoErrors();
	}

	private String SNIPSET_941 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_941() throws Exception {
		validate(file(this.SNIPSET_941)).assertNoErrors();
	}

	private String SNIPSET_942 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_942() throws Exception {
		validate(file(this.SNIPSET_942)).assertNoErrors();
	}

	private String SNIPSET_943 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_943() throws Exception {
		validate(file(this.SNIPSET_943)).assertNoErrors();
	}

	private String SNIPSET_944 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_944() throws Exception {
		validate(file(this.SNIPSET_944)).assertNoErrors();
	}

	private String SNIPSET_945 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_945() throws Exception {
		validate(file(this.SNIPSET_945)).assertNoErrors();
	}

	private String SNIPSET_946 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_946() throws Exception {
		validate(file(this.SNIPSET_946)).assertNoErrors();
	}

	private String SNIPSET_947 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_947() throws Exception {
		validate(file(this.SNIPSET_947)).assertNoErrors();
	}

	private String SNIPSET_948 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_948() throws Exception {
		validate(file(this.SNIPSET_948)).assertNoErrors();
	}

	private String SNIPSET_949 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_949() throws Exception {
		validate(file(this.SNIPSET_949)).assertNoErrors();
	}

	private String SNIPSET_950 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_950() throws Exception {
		validate(file(this.SNIPSET_950)).assertNoErrors();
	}

	private String SNIPSET_951 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_951() throws Exception {
		validate(file(this.SNIPSET_951)).assertNoErrors();
	}

	private String SNIPSET_952 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_952() throws Exception {
		validate(file(this.SNIPSET_952)).assertNoErrors();
	}

	private String SNIPSET_953 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_953() throws Exception {
		validate(file(this.SNIPSET_953)).assertNoErrors();
	}

	private String SNIPSET_954 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_954() throws Exception {
		validate(file(this.SNIPSET_954)).assertNoErrors();
	}

	private String SNIPSET_955 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_955() throws Exception {
		validate(file(this.SNIPSET_955)).assertNoErrors();
	}

	private String SNIPSET_956 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_956() throws Exception {
		validate(file(this.SNIPSET_956)).assertNoErrors();
	}

	private String SNIPSET_957 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_957() throws Exception {
		validate(file(this.SNIPSET_957)).assertNoErrors();
	}

	private String SNIPSET_958 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_958() throws Exception {
		validate(file(this.SNIPSET_958)).assertNoErrors();
	}

	private String SNIPSET_959 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_959() throws Exception {
		validate(file(this.SNIPSET_959)).assertNoErrors();
	}

	private String SNIPSET_960 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_960() throws Exception {
		validate(file(this.SNIPSET_960)).assertNoErrors();
	}

	private String SNIPSET_961 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_961() throws Exception {
		validate(file(this.SNIPSET_961)).assertNoErrors();
	}

	private String SNIPSET_962 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_962() throws Exception {
		validate(file(this.SNIPSET_962)).assertNoErrors();
	}

	private String SNIPSET_963 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_963() throws Exception {
		validate(file(this.SNIPSET_963)).assertNoErrors();
	}

	private String SNIPSET_964 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_964() throws Exception {
		validate(file(this.SNIPSET_964)).assertNoErrors();
	}

	private String SNIPSET_965 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_965() throws Exception {
		validate(file(this.SNIPSET_965)).assertNoErrors();
	}

	private String SNIPSET_966 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_966() throws Exception {
		validate(file(this.SNIPSET_966)).assertNoErrors();
	}

	private String SNIPSET_967 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_967() throws Exception {
		validate(file(this.SNIPSET_967)).assertNoErrors();
	}

	private String SNIPSET_968 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_968() throws Exception {
		validate(file(this.SNIPSET_968)).assertNoErrors();
	}

	private String SNIPSET_969 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_969() throws Exception {
		validate(file(this.SNIPSET_969)).assertNoErrors();
	}

	private String SNIPSET_970 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_970() throws Exception {
		validate(file(this.SNIPSET_970)).assertNoErrors();
	}

	private String SNIPSET_971 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_971() throws Exception {
		validate(file(this.SNIPSET_971)).assertNoErrors();
	}

	private String SNIPSET_972 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_972() throws Exception {
		validate(file(this.SNIPSET_972)).assertNoErrors();
	}

	private String SNIPSET_973 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_973() throws Exception {
		validate(file(this.SNIPSET_973)).assertNoErrors();
	}

	private String SNIPSET_974 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_974() throws Exception {
		validate(file(this.SNIPSET_974)).assertNoErrors();
	}

	private String SNIPSET_975 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_975() throws Exception {
		validate(file(this.SNIPSET_975)).assertNoErrors();
	}

	private String SNIPSET_976 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_976() throws Exception {
		validate(file(this.SNIPSET_976)).assertNoErrors();
	}

	private String SNIPSET_977 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_977() throws Exception {
		validate(file(this.SNIPSET_977)).assertNoErrors();
	}

	private String SNIPSET_978 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_978() throws Exception {
		validate(file(this.SNIPSET_978)).assertNoErrors();
	}

	private String SNIPSET_979 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_979() throws Exception {
		validate(file(this.SNIPSET_979)).assertNoErrors();
	}

	private String SNIPSET_980 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_980() throws Exception {
		validate(file(this.SNIPSET_980)).assertNoErrors();
	}

	private String SNIPSET_981 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_981() throws Exception {
		validate(file(this.SNIPSET_981)).assertNoErrors();
	}

	private String SNIPSET_982 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_982() throws Exception {
		validate(file(this.SNIPSET_982)).assertNoErrors();
	}

	private String SNIPSET_983 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_983() throws Exception {
		validate(file(this.SNIPSET_983)).assertNoErrors();
	}

	private String SNIPSET_984 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_984() throws Exception {
		validate(file(this.SNIPSET_984)).assertNoErrors();
	}

	private String SNIPSET_985 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_985() throws Exception {
		validate(file(this.SNIPSET_985)).assertNoErrors();
	}

	private String SNIPSET_986 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_986() throws Exception {
		validate(file(this.SNIPSET_986)).assertNoErrors();
	}

	private String SNIPSET_987 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_987() throws Exception {
		validate(file(this.SNIPSET_987)).assertNoErrors();
	}

	private String SNIPSET_988 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_988() throws Exception {
		validate(file(this.SNIPSET_988)).assertNoErrors();
	}

	private String SNIPSET_989 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_989() throws Exception {
		validate(file(this.SNIPSET_989)).assertNoErrors();
	}

	private String SNIPSET_990 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_990() throws Exception {
		validate(file(this.SNIPSET_990)).assertNoErrors();
	}

	private String SNIPSET_991 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_991() throws Exception {
		validate(file(this.SNIPSET_991)).assertNoErrors();
	}

	private String SNIPSET_992 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_992() throws Exception {
		validate(file(this.SNIPSET_992)).assertNoErrors();
	}

	private String SNIPSET_993 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_993() throws Exception {
		validate(file(this.SNIPSET_993)).assertNoErrors();
	}

	private String SNIPSET_994 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_994() throws Exception {
		validate(file(this.SNIPSET_994)).assertNoErrors();
	}

	private String SNIPSET_995 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_995() throws Exception {
		validate(file(this.SNIPSET_995)).assertNoErrors();
	}

	private String SNIPSET_996 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_996() throws Exception {
		validate(file(this.SNIPSET_996)).assertNoErrors();
	}

	private String SNIPSET_997 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_997() throws Exception {
		validate(file(this.SNIPSET_997)).assertNoErrors();
	}

	private String SNIPSET_998 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_998() throws Exception {
		validate(file(this.SNIPSET_998)).assertNoErrors();
	}

	private String SNIPSET_999 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_999() throws Exception {
		validate(file(this.SNIPSET_999)).assertNoErrors();
	}

	private String SNIPSET_1000 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : byte) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1000() throws Exception {
		validate(file(this.SNIPSET_1000)).assertNoErrors();
	}

	private String SNIPSET_1001 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : long) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1001() throws Exception {
		validate(file(this.SNIPSET_1001)).assertNoErrors();
	}

	private String SNIPSET_1002 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1002() throws Exception {
		validate(file(this.SNIPSET_1002)).assertNoErrors();
	}

	private String SNIPSET_1003 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : short) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1003() throws Exception {
		validate(file(this.SNIPSET_1003)).assertNoErrors();
	}

	private String SNIPSET_1004 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : int) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1004() throws Exception {
		validate(file(this.SNIPSET_1004)).assertNoErrors();
	}

	private String SNIPSET_1005 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1005() throws Exception {
		validate(file(this.SNIPSET_1005)).assertNoErrors();
	}

	private String SNIPSET_1006 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Long) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1006() throws Exception {
		validate(file(this.SNIPSET_1006)).assertNoErrors();
	}

	private String SNIPSET_1007 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1007() throws Exception {
		validate(file(this.SNIPSET_1007)).assertNoErrors();
	}

	private String SNIPSET_1008 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Integer) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1008() throws Exception {
		validate(file(this.SNIPSET_1008)).assertNoErrors();
	}

	private String SNIPSET_1009 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1009() throws Exception {
		validate(file(this.SNIPSET_1009)).assertNoErrors();
	}

	private String SNIPSET_1010 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Byte) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1010() throws Exception {
		validate(file(this.SNIPSET_1010)).assertNoErrors();
	}

	private String SNIPSET_1011 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : Short) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1011() throws Exception {
		validate(file(this.SNIPSET_1011)).assertNoErrors();
	}

	private String SNIPSET_1012 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicLong) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1012() throws Exception {
		validate(file(this.SNIPSET_1012)).assertNoErrors();
	}

	private String SNIPSET_1013 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : float, b : AtomicInteger) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1013() throws Exception {
		validate(file(this.SNIPSET_1013)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
