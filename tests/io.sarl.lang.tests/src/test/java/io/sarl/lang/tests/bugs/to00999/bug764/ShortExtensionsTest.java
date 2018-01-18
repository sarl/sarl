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
public class ShortExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_1859 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short) : int {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_1859() throws Exception {
		validate(file(this.SNIPSET_1859)).assertNoErrors();
	}

	private String SNIPSET_1860 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1860() throws Exception {
		validate(file(this.SNIPSET_1860)).assertNoErrors();
	}

	private String SNIPSET_1861 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1861() throws Exception {
		validate(file(this.SNIPSET_1861)).assertNoErrors();
	}

	private String SNIPSET_1862 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1862() throws Exception {
		validate(file(this.SNIPSET_1862)).assertNoErrors();
	}

	private String SNIPSET_1863 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1863() throws Exception {
		validate(file(this.SNIPSET_1863)).assertNoErrors();
	}

	private String SNIPSET_1864 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1864() throws Exception {
		validate(file(this.SNIPSET_1864)).assertNoErrors();
	}

	private String SNIPSET_1865 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1865() throws Exception {
		validate(file(this.SNIPSET_1865)).assertNoErrors();
	}

	private String SNIPSET_1866 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1866() throws Exception {
		validate(file(this.SNIPSET_1866)).assertNoErrors();
	}

	private String SNIPSET_1867 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1867() throws Exception {
		validate(file(this.SNIPSET_1867)).assertNoErrors();
	}

	private String SNIPSET_1868 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1868() throws Exception {
		validate(file(this.SNIPSET_1868)).assertNoErrors();
	}

	private String SNIPSET_1869 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1869() throws Exception {
		validate(file(this.SNIPSET_1869)).assertNoErrors();
	}

	private String SNIPSET_1870 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1870() throws Exception {
		validate(file(this.SNIPSET_1870)).assertNoErrors();
	}

	private String SNIPSET_1871 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1871() throws Exception {
		validate(file(this.SNIPSET_1871)).assertNoErrors();
	}

	private String SNIPSET_1872 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1872() throws Exception {
		validate(file(this.SNIPSET_1872)).assertNoErrors();
	}

	private String SNIPSET_1873 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1873() throws Exception {
		validate(file(this.SNIPSET_1873)).assertNoErrors();
	}

	private String SNIPSET_1874 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1874() throws Exception {
		validate(file(this.SNIPSET_1874)).assertNoErrors();
	}

	private String SNIPSET_1875 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1875() throws Exception {
		validate(file(this.SNIPSET_1875)).assertNoErrors();
	}

	private String SNIPSET_1876 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1876() throws Exception {
		validate(file(this.SNIPSET_1876)).assertNoErrors();
	}

	private String SNIPSET_1877 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1877() throws Exception {
		validate(file(this.SNIPSET_1877)).assertNoErrors();
	}

	private String SNIPSET_1878 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1878() throws Exception {
		validate(file(this.SNIPSET_1878)).assertNoErrors();
	}

	private String SNIPSET_1879 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1879() throws Exception {
		validate(file(this.SNIPSET_1879)).assertNoErrors();
	}

	private String SNIPSET_1880 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1880() throws Exception {
		validate(file(this.SNIPSET_1880)).assertNoErrors();
	}

	private String SNIPSET_1881 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1881() throws Exception {
		validate(file(this.SNIPSET_1881)).assertNoErrors();
	}

	private String SNIPSET_1882 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1882() throws Exception {
		validate(file(this.SNIPSET_1882)).assertNoErrors();
	}

	private String SNIPSET_1883 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1883() throws Exception {
		validate(file(this.SNIPSET_1883)).assertNoErrors();
	}

	private String SNIPSET_1884 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1884() throws Exception {
		validate(file(this.SNIPSET_1884)).assertNoErrors();
	}

	private String SNIPSET_1885 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1885() throws Exception {
		validate(file(this.SNIPSET_1885)).assertNoErrors();
	}

	private String SNIPSET_1886 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1886() throws Exception {
		validate(file(this.SNIPSET_1886)).assertNoErrors();
	}

	private String SNIPSET_1887 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : int {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1887() throws Exception {
		validate(file(this.SNIPSET_1887)).assertNoErrors();
	}

	private String SNIPSET_1888 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1888() throws Exception {
		validate(file(this.SNIPSET_1888)).assertNoErrors();
	}

	private String SNIPSET_1889 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1889() throws Exception {
		validate(file(this.SNIPSET_1889)).assertNoErrors();
	}

	private String SNIPSET_1890 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1890() throws Exception {
		validate(file(this.SNIPSET_1890)).assertNoErrors();
	}

	private String SNIPSET_1891 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1891() throws Exception {
		validate(file(this.SNIPSET_1891)).assertNoErrors();
	}

	private String SNIPSET_1892 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1892() throws Exception {
		validate(file(this.SNIPSET_1892)).assertNoErrors();
	}

	private String SNIPSET_1893 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1893() throws Exception {
		validate(file(this.SNIPSET_1893)).assertNoErrors();
	}

	private String SNIPSET_1894 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1894() throws Exception {
		validate(file(this.SNIPSET_1894)).assertNoErrors();
	}

	private String SNIPSET_1895 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1895() throws Exception {
		validate(file(this.SNIPSET_1895)).assertNoErrors();
	}

	private String SNIPSET_1896 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1896() throws Exception {
		validate(file(this.SNIPSET_1896)).assertNoErrors();
	}

	private String SNIPSET_1897 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1897() throws Exception {
		validate(file(this.SNIPSET_1897)).assertNoErrors();
	}

	private String SNIPSET_1898 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1898() throws Exception {
		validate(file(this.SNIPSET_1898)).assertNoErrors();
	}

	private String SNIPSET_1899 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1899() throws Exception {
		validate(file(this.SNIPSET_1899)).assertNoErrors();
	}

	private String SNIPSET_1900 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1900() throws Exception {
		validate(file(this.SNIPSET_1900)).assertNoErrors();
	}

	private String SNIPSET_1901 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1901() throws Exception {
		validate(file(this.SNIPSET_1901)).assertNoErrors();
	}

	private String SNIPSET_1902 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1902() throws Exception {
		validate(file(this.SNIPSET_1902)).assertNoErrors();
	}

	private String SNIPSET_1903 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1903() throws Exception {
		validate(file(this.SNIPSET_1903)).assertNoErrors();
	}

	private String SNIPSET_1904 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1904() throws Exception {
		validate(file(this.SNIPSET_1904)).assertNoErrors();
	}

	private String SNIPSET_1905 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1905() throws Exception {
		validate(file(this.SNIPSET_1905)).assertNoErrors();
	}

	private String SNIPSET_1906 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1906() throws Exception {
		validate(file(this.SNIPSET_1906)).assertNoErrors();
	}

	private String SNIPSET_1907 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1907() throws Exception {
		validate(file(this.SNIPSET_1907)).assertNoErrors();
	}

	private String SNIPSET_1908 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1908() throws Exception {
		validate(file(this.SNIPSET_1908)).assertNoErrors();
	}

	private String SNIPSET_1909 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1909() throws Exception {
		validate(file(this.SNIPSET_1909)).assertNoErrors();
	}

	private String SNIPSET_1910 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1910() throws Exception {
		validate(file(this.SNIPSET_1910)).assertNoErrors();
	}

	private String SNIPSET_1911 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1911() throws Exception {
		validate(file(this.SNIPSET_1911)).assertNoErrors();
	}

	private String SNIPSET_1912 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1912() throws Exception {
		validate(file(this.SNIPSET_1912)).assertNoErrors();
	}

	private String SNIPSET_1913 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1913() throws Exception {
		validate(file(this.SNIPSET_1913)).assertNoErrors();
	}

	private String SNIPSET_1914 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1914() throws Exception {
		validate(file(this.SNIPSET_1914)).assertNoErrors();
	}

	private String SNIPSET_1915 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : int {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1915() throws Exception {
		validate(file(this.SNIPSET_1915)).assertNoErrors();
	}

	private String SNIPSET_1916 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1916() throws Exception {
		validate(file(this.SNIPSET_1916)).assertNoErrors();
	}

	private String SNIPSET_1917 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1917() throws Exception {
		validate(file(this.SNIPSET_1917)).assertNoErrors();
	}

	private String SNIPSET_1918 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1918() throws Exception {
		validate(file(this.SNIPSET_1918)).assertNoErrors();
	}

	private String SNIPSET_1919 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1919() throws Exception {
		validate(file(this.SNIPSET_1919)).assertNoErrors();
	}

	private String SNIPSET_1920 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1920() throws Exception {
		validate(file(this.SNIPSET_1920)).assertNoErrors();
	}

	private String SNIPSET_1921 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1921() throws Exception {
		validate(file(this.SNIPSET_1921)).assertNoErrors();
	}

	private String SNIPSET_1922 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1922() throws Exception {
		validate(file(this.SNIPSET_1922)).assertNoErrors();
	}

	private String SNIPSET_1923 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1923() throws Exception {
		validate(file(this.SNIPSET_1923)).assertNoErrors();
	}

	private String SNIPSET_1924 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1924() throws Exception {
		validate(file(this.SNIPSET_1924)).assertNoErrors();
	}

	private String SNIPSET_1925 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1925() throws Exception {
		validate(file(this.SNIPSET_1925)).assertNoErrors();
	}

	private String SNIPSET_1926 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1926() throws Exception {
		validate(file(this.SNIPSET_1926)).assertNoErrors();
	}

	private String SNIPSET_1927 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1927() throws Exception {
		validate(file(this.SNIPSET_1927)).assertNoErrors();
	}

	private String SNIPSET_1928 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1928() throws Exception {
		validate(file(this.SNIPSET_1928)).assertNoErrors();
	}

	private String SNIPSET_1929 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : int {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1929() throws Exception {
		validate(file(this.SNIPSET_1929)).assertNoErrors();
	}

	private String SNIPSET_1930 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1930() throws Exception {
		validate(file(this.SNIPSET_1930)).assertNoErrors();
	}

	private String SNIPSET_1931 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1931() throws Exception {
		validate(file(this.SNIPSET_1931)).assertNoErrors();
	}

	private String SNIPSET_1932 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1932() throws Exception {
		validate(file(this.SNIPSET_1932)).assertNoErrors();
	}

	private String SNIPSET_1933 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1933() throws Exception {
		validate(file(this.SNIPSET_1933)).assertNoErrors();
	}

	private String SNIPSET_1934 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1934() throws Exception {
		validate(file(this.SNIPSET_1934)).assertNoErrors();
	}

	private String SNIPSET_1935 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1935() throws Exception {
		validate(file(this.SNIPSET_1935)).assertNoErrors();
	}

	private String SNIPSET_1936 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1936() throws Exception {
		validate(file(this.SNIPSET_1936)).assertNoErrors();
	}

	private String SNIPSET_1937 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1937() throws Exception {
		validate(file(this.SNIPSET_1937)).assertNoErrors();
	}

	private String SNIPSET_1938 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1938() throws Exception {
		validate(file(this.SNIPSET_1938)).assertNoErrors();
	}

	private String SNIPSET_1939 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1939() throws Exception {
		validate(file(this.SNIPSET_1939)).assertNoErrors();
	}

	private String SNIPSET_1940 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1940() throws Exception {
		validate(file(this.SNIPSET_1940)).assertNoErrors();
	}

	private String SNIPSET_1941 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1941() throws Exception {
		validate(file(this.SNIPSET_1941)).assertNoErrors();
	}

	private String SNIPSET_1942 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1942() throws Exception {
		validate(file(this.SNIPSET_1942)).assertNoErrors();
	}

	private String SNIPSET_1943 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1943() throws Exception {
		validate(file(this.SNIPSET_1943)).assertNoErrors();
	}

	private String SNIPSET_1944 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1944() throws Exception {
		validate(file(this.SNIPSET_1944)).assertNoErrors();
	}

	private String SNIPSET_1945 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1945() throws Exception {
		validate(file(this.SNIPSET_1945)).assertNoErrors();
	}

	private String SNIPSET_1946 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1946() throws Exception {
		validate(file(this.SNIPSET_1946)).assertNoErrors();
	}

	private String SNIPSET_1947 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1947() throws Exception {
		validate(file(this.SNIPSET_1947)).assertNoErrors();
	}

	private String SNIPSET_1948 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1948() throws Exception {
		validate(file(this.SNIPSET_1948)).assertNoErrors();
	}

	private String SNIPSET_1949 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1949() throws Exception {
		validate(file(this.SNIPSET_1949)).assertNoErrors();
	}

	private String SNIPSET_1950 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1950() throws Exception {
		validate(file(this.SNIPSET_1950)).assertNoErrors();
	}

	private String SNIPSET_1951 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1951() throws Exception {
		validate(file(this.SNIPSET_1951)).assertNoErrors();
	}

	private String SNIPSET_1952 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1952() throws Exception {
		validate(file(this.SNIPSET_1952)).assertNoErrors();
	}

	private String SNIPSET_1953 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1953() throws Exception {
		validate(file(this.SNIPSET_1953)).assertNoErrors();
	}

	private String SNIPSET_1954 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1954() throws Exception {
		validate(file(this.SNIPSET_1954)).assertNoErrors();
	}

	private String SNIPSET_1955 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1955() throws Exception {
		validate(file(this.SNIPSET_1955)).assertNoErrors();
	}

	private String SNIPSET_1956 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1956() throws Exception {
		validate(file(this.SNIPSET_1956)).assertNoErrors();
	}

	private String SNIPSET_1957 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1957() throws Exception {
		validate(file(this.SNIPSET_1957)).assertNoErrors();
	}

	private String SNIPSET_1958 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1958() throws Exception {
		validate(file(this.SNIPSET_1958)).assertNoErrors();
	}

	private String SNIPSET_1959 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1959() throws Exception {
		validate(file(this.SNIPSET_1959)).assertNoErrors();
	}

	private String SNIPSET_1960 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1960() throws Exception {
		validate(file(this.SNIPSET_1960)).assertNoErrors();
	}

	private String SNIPSET_1961 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1961() throws Exception {
		validate(file(this.SNIPSET_1961)).assertNoErrors();
	}

	private String SNIPSET_1962 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1962() throws Exception {
		validate(file(this.SNIPSET_1962)).assertNoErrors();
	}

	private String SNIPSET_1963 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1963() throws Exception {
		validate(file(this.SNIPSET_1963)).assertNoErrors();
	}

	private String SNIPSET_1964 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1964() throws Exception {
		validate(file(this.SNIPSET_1964)).assertNoErrors();
	}

	private String SNIPSET_1965 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1965() throws Exception {
		validate(file(this.SNIPSET_1965)).assertNoErrors();
	}

	private String SNIPSET_1966 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1966() throws Exception {
		validate(file(this.SNIPSET_1966)).assertNoErrors();
	}

	private String SNIPSET_1967 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1967() throws Exception {
		validate(file(this.SNIPSET_1967)).assertNoErrors();
	}

	private String SNIPSET_1968 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1968() throws Exception {
		validate(file(this.SNIPSET_1968)).assertNoErrors();
	}

	private String SNIPSET_1969 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1969() throws Exception {
		validate(file(this.SNIPSET_1969)).assertNoErrors();
	}

	private String SNIPSET_1970 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1970() throws Exception {
		validate(file(this.SNIPSET_1970)).assertNoErrors();
	}

	private String SNIPSET_1971 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1971() throws Exception {
		validate(file(this.SNIPSET_1971)).assertNoErrors();
	}

	private String SNIPSET_1972 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1972() throws Exception {
		validate(file(this.SNIPSET_1972)).assertNoErrors();
	}

	private String SNIPSET_1973 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1973() throws Exception {
		validate(file(this.SNIPSET_1973)).assertNoErrors();
	}

	private String SNIPSET_1974 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1974() throws Exception {
		validate(file(this.SNIPSET_1974)).assertNoErrors();
	}

	private String SNIPSET_1975 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1975() throws Exception {
		validate(file(this.SNIPSET_1975)).assertNoErrors();
	}

	private String SNIPSET_1976 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1976() throws Exception {
		validate(file(this.SNIPSET_1976)).assertNoErrors();
	}

	private String SNIPSET_1977 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1977() throws Exception {
		validate(file(this.SNIPSET_1977)).assertNoErrors();
	}

	private String SNIPSET_1978 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1978() throws Exception {
		validate(file(this.SNIPSET_1978)).assertNoErrors();
	}

	private String SNIPSET_1979 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1979() throws Exception {
		validate(file(this.SNIPSET_1979)).assertNoErrors();
	}

	private String SNIPSET_1980 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1980() throws Exception {
		validate(file(this.SNIPSET_1980)).assertNoErrors();
	}

	private String SNIPSET_1981 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1981() throws Exception {
		validate(file(this.SNIPSET_1981)).assertNoErrors();
	}

	private String SNIPSET_1982 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1982() throws Exception {
		validate(file(this.SNIPSET_1982)).assertNoErrors();
	}

	private String SNIPSET_1983 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1983() throws Exception {
		validate(file(this.SNIPSET_1983)).assertNoErrors();
	}

	private String SNIPSET_1984 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1984() throws Exception {
		validate(file(this.SNIPSET_1984)).assertNoErrors();
	}

	private String SNIPSET_1985 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1985() throws Exception {
		validate(file(this.SNIPSET_1985)).assertNoErrors();
	}

	private String SNIPSET_1986 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1986() throws Exception {
		validate(file(this.SNIPSET_1986)).assertNoErrors();
	}

	private String SNIPSET_1987 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1987() throws Exception {
		validate(file(this.SNIPSET_1987)).assertNoErrors();
	}

	private String SNIPSET_1988 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1988() throws Exception {
		validate(file(this.SNIPSET_1988)).assertNoErrors();
	}

	private String SNIPSET_1989 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1989() throws Exception {
		validate(file(this.SNIPSET_1989)).assertNoErrors();
	}

	private String SNIPSET_1990 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1990() throws Exception {
		validate(file(this.SNIPSET_1990)).assertNoErrors();
	}

	private String SNIPSET_1991 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1991() throws Exception {
		validate(file(this.SNIPSET_1991)).assertNoErrors();
	}

	private String SNIPSET_1992 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1992() throws Exception {
		validate(file(this.SNIPSET_1992)).assertNoErrors();
	}

	private String SNIPSET_1993 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1993() throws Exception {
		validate(file(this.SNIPSET_1993)).assertNoErrors();
	}

	private String SNIPSET_1994 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1994() throws Exception {
		validate(file(this.SNIPSET_1994)).assertNoErrors();
	}

	private String SNIPSET_1995 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1995() throws Exception {
		validate(file(this.SNIPSET_1995)).assertNoErrors();
	}

	private String SNIPSET_1996 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1996() throws Exception {
		validate(file(this.SNIPSET_1996)).assertNoErrors();
	}

	private String SNIPSET_1997 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1997() throws Exception {
		validate(file(this.SNIPSET_1997)).assertNoErrors();
	}

	private String SNIPSET_1998 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1998() throws Exception {
		validate(file(this.SNIPSET_1998)).assertNoErrors();
	}

	private String SNIPSET_1999 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1999() throws Exception {
		validate(file(this.SNIPSET_1999)).assertNoErrors();
	}

	private String SNIPSET_2000 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2000() throws Exception {
		validate(file(this.SNIPSET_2000)).assertNoErrors();
	}

	private String SNIPSET_2001 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2001() throws Exception {
		validate(file(this.SNIPSET_2001)).assertNoErrors();
	}

	private String SNIPSET_2002 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2002() throws Exception {
		validate(file(this.SNIPSET_2002)).assertNoErrors();
	}

	private String SNIPSET_2003 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2003() throws Exception {
		validate(file(this.SNIPSET_2003)).assertNoErrors();
	}

	private String SNIPSET_2004 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2004() throws Exception {
		validate(file(this.SNIPSET_2004)).assertNoErrors();
	}

	private String SNIPSET_2005 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2005() throws Exception {
		validate(file(this.SNIPSET_2005)).assertNoErrors();
	}

	private String SNIPSET_2006 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2006() throws Exception {
		validate(file(this.SNIPSET_2006)).assertNoErrors();
	}

	private String SNIPSET_2007 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2007() throws Exception {
		validate(file(this.SNIPSET_2007)).assertNoErrors();
	}

	private String SNIPSET_2008 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2008() throws Exception {
		validate(file(this.SNIPSET_2008)).assertNoErrors();
	}

	private String SNIPSET_2009 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2009() throws Exception {
		validate(file(this.SNIPSET_2009)).assertNoErrors();
	}

	private String SNIPSET_2010 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2010() throws Exception {
		validate(file(this.SNIPSET_2010)).assertNoErrors();
	}

	private String SNIPSET_2011 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2011() throws Exception {
		validate(file(this.SNIPSET_2011)).assertNoErrors();
	}

	private String SNIPSET_2012 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2012() throws Exception {
		validate(file(this.SNIPSET_2012)).assertNoErrors();
	}

	private String SNIPSET_2013 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : int {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_2013() throws Exception {
		validate(file(this.SNIPSET_2013)).assertNoErrors();
	}

	private String SNIPSET_2014 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2014() throws Exception {
		validate(file(this.SNIPSET_2014)).assertNoErrors();
	}

	private String SNIPSET_2015 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2015() throws Exception {
		validate(file(this.SNIPSET_2015)).assertNoErrors();
	}

	private String SNIPSET_2016 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2016() throws Exception {
		validate(file(this.SNIPSET_2016)).assertNoErrors();
	}

	private String SNIPSET_2017 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2017() throws Exception {
		validate(file(this.SNIPSET_2017)).assertNoErrors();
	}

	private String SNIPSET_2018 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : int) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2018() throws Exception {
		validate(file(this.SNIPSET_2018)).assertNoErrors();
	}

	private String SNIPSET_2019 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2019() throws Exception {
		validate(file(this.SNIPSET_2019)).assertNoErrors();
	}

	private String SNIPSET_2020 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2020() throws Exception {
		validate(file(this.SNIPSET_2020)).assertNoErrors();
	}

	private String SNIPSET_2021 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2021() throws Exception {
		validate(file(this.SNIPSET_2021)).assertNoErrors();
	}

	private String SNIPSET_2022 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Integer) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2022() throws Exception {
		validate(file(this.SNIPSET_2022)).assertNoErrors();
	}

	private String SNIPSET_2023 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2023() throws Exception {
		validate(file(this.SNIPSET_2023)).assertNoErrors();
	}

	private String SNIPSET_2024 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Byte) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2024() throws Exception {
		validate(file(this.SNIPSET_2024)).assertNoErrors();
	}

	private String SNIPSET_2025 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : Short) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2025() throws Exception {
		validate(file(this.SNIPSET_2025)).assertNoErrors();
	}

	private String SNIPSET_2026 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2026() throws Exception {
		validate(file(this.SNIPSET_2026)).assertNoErrors();
	}

	private String SNIPSET_2027 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Short, b : AtomicInteger) : int {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_2027() throws Exception {
		validate(file(this.SNIPSET_2027)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
