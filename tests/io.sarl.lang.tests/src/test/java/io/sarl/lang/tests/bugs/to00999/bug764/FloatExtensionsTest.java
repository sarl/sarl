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
public class FloatExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_1183 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float) : float {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_1183() throws Exception {
		validate(file(this.SNIPSET_1183)).assertNoErrors();
	}

	private String SNIPSET_1184 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1184() throws Exception {
		validate(file(this.SNIPSET_1184)).assertNoErrors();
	}

	private String SNIPSET_1185 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1185() throws Exception {
		validate(file(this.SNIPSET_1185)).assertNoErrors();
	}

	private String SNIPSET_1186 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1186() throws Exception {
		validate(file(this.SNIPSET_1186)).assertNoErrors();
	}

	private String SNIPSET_1187 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1187() throws Exception {
		validate(file(this.SNIPSET_1187)).assertNoErrors();
	}

	private String SNIPSET_1188 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1188() throws Exception {
		validate(file(this.SNIPSET_1188)).assertNoErrors();
	}

	private String SNIPSET_1189 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1189() throws Exception {
		validate(file(this.SNIPSET_1189)).assertNoErrors();
	}

	private String SNIPSET_1190 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1190() throws Exception {
		validate(file(this.SNIPSET_1190)).assertNoErrors();
	}

	private String SNIPSET_1191 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1191() throws Exception {
		validate(file(this.SNIPSET_1191)).assertNoErrors();
	}

	private String SNIPSET_1192 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1192() throws Exception {
		validate(file(this.SNIPSET_1192)).assertNoErrors();
	}

	private String SNIPSET_1193 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1193() throws Exception {
		validate(file(this.SNIPSET_1193)).assertNoErrors();
	}

	private String SNIPSET_1194 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1194() throws Exception {
		validate(file(this.SNIPSET_1194)).assertNoErrors();
	}

	private String SNIPSET_1195 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1195() throws Exception {
		validate(file(this.SNIPSET_1195)).assertNoErrors();
	}

	private String SNIPSET_1196 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1196() throws Exception {
		validate(file(this.SNIPSET_1196)).assertNoErrors();
	}

	private String SNIPSET_1197 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_1197() throws Exception {
		validate(file(this.SNIPSET_1197)).assertNoErrors();
	}

	private String SNIPSET_1198 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1198() throws Exception {
		validate(file(this.SNIPSET_1198)).assertNoErrors();
	}

	private String SNIPSET_1199 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1199() throws Exception {
		validate(file(this.SNIPSET_1199)).assertNoErrors();
	}

	private String SNIPSET_1200 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1200() throws Exception {
		validate(file(this.SNIPSET_1200)).assertNoErrors();
	}

	private String SNIPSET_1201 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1201() throws Exception {
		validate(file(this.SNIPSET_1201)).assertNoErrors();
	}

	private String SNIPSET_1202 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1202() throws Exception {
		validate(file(this.SNIPSET_1202)).assertNoErrors();
	}

	private String SNIPSET_1203 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1203() throws Exception {
		validate(file(this.SNIPSET_1203)).assertNoErrors();
	}

	private String SNIPSET_1204 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1204() throws Exception {
		validate(file(this.SNIPSET_1204)).assertNoErrors();
	}

	private String SNIPSET_1205 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1205() throws Exception {
		validate(file(this.SNIPSET_1205)).assertNoErrors();
	}

	private String SNIPSET_1206 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1206() throws Exception {
		validate(file(this.SNIPSET_1206)).assertNoErrors();
	}

	private String SNIPSET_1207 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1207() throws Exception {
		validate(file(this.SNIPSET_1207)).assertNoErrors();
	}

	private String SNIPSET_1208 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1208() throws Exception {
		validate(file(this.SNIPSET_1208)).assertNoErrors();
	}

	private String SNIPSET_1209 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1209() throws Exception {
		validate(file(this.SNIPSET_1209)).assertNoErrors();
	}

	private String SNIPSET_1210 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1210() throws Exception {
		validate(file(this.SNIPSET_1210)).assertNoErrors();
	}

	private String SNIPSET_1211 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_1211() throws Exception {
		validate(file(this.SNIPSET_1211)).assertNoErrors();
	}

	private String SNIPSET_1212 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1212() throws Exception {
		validate(file(this.SNIPSET_1212)).assertNoErrors();
	}

	private String SNIPSET_1213 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1213() throws Exception {
		validate(file(this.SNIPSET_1213)).assertNoErrors();
	}

	private String SNIPSET_1214 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1214() throws Exception {
		validate(file(this.SNIPSET_1214)).assertNoErrors();
	}

	private String SNIPSET_1215 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1215() throws Exception {
		validate(file(this.SNIPSET_1215)).assertNoErrors();
	}

	private String SNIPSET_1216 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1216() throws Exception {
		validate(file(this.SNIPSET_1216)).assertNoErrors();
	}

	private String SNIPSET_1217 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1217() throws Exception {
		validate(file(this.SNIPSET_1217)).assertNoErrors();
	}

	private String SNIPSET_1218 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1218() throws Exception {
		validate(file(this.SNIPSET_1218)).assertNoErrors();
	}

	private String SNIPSET_1219 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1219() throws Exception {
		validate(file(this.SNIPSET_1219)).assertNoErrors();
	}

	private String SNIPSET_1220 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1220() throws Exception {
		validate(file(this.SNIPSET_1220)).assertNoErrors();
	}

	private String SNIPSET_1221 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1221() throws Exception {
		validate(file(this.SNIPSET_1221)).assertNoErrors();
	}

	private String SNIPSET_1222 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1222() throws Exception {
		validate(file(this.SNIPSET_1222)).assertNoErrors();
	}

	private String SNIPSET_1223 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1223() throws Exception {
		validate(file(this.SNIPSET_1223)).assertNoErrors();
	}

	private String SNIPSET_1224 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1224() throws Exception {
		validate(file(this.SNIPSET_1224)).assertNoErrors();
	}

	private String SNIPSET_1225 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_1225() throws Exception {
		validate(file(this.SNIPSET_1225)).assertNoErrors();
	}

	private String SNIPSET_1226 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1226() throws Exception {
		validate(file(this.SNIPSET_1226)).assertNoErrors();
	}

	private String SNIPSET_1227 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1227() throws Exception {
		validate(file(this.SNIPSET_1227)).assertNoErrors();
	}

	private String SNIPSET_1228 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1228() throws Exception {
		validate(file(this.SNIPSET_1228)).assertNoErrors();
	}

	private String SNIPSET_1229 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1229() throws Exception {
		validate(file(this.SNIPSET_1229)).assertNoErrors();
	}

	private String SNIPSET_1230 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1230() throws Exception {
		validate(file(this.SNIPSET_1230)).assertNoErrors();
	}

	private String SNIPSET_1231 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1231() throws Exception {
		validate(file(this.SNIPSET_1231)).assertNoErrors();
	}

	private String SNIPSET_1232 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1232() throws Exception {
		validate(file(this.SNIPSET_1232)).assertNoErrors();
	}

	private String SNIPSET_1233 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1233() throws Exception {
		validate(file(this.SNIPSET_1233)).assertNoErrors();
	}

	private String SNIPSET_1234 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1234() throws Exception {
		validate(file(this.SNIPSET_1234)).assertNoErrors();
	}

	private String SNIPSET_1235 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1235() throws Exception {
		validate(file(this.SNIPSET_1235)).assertNoErrors();
	}

	private String SNIPSET_1236 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1236() throws Exception {
		validate(file(this.SNIPSET_1236)).assertNoErrors();
	}

	private String SNIPSET_1237 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1237() throws Exception {
		validate(file(this.SNIPSET_1237)).assertNoErrors();
	}

	private String SNIPSET_1238 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1238() throws Exception {
		validate(file(this.SNIPSET_1238)).assertNoErrors();
	}

	private String SNIPSET_1239 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_1239() throws Exception {
		validate(file(this.SNIPSET_1239)).assertNoErrors();
	}

	private String SNIPSET_1240 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1240() throws Exception {
		validate(file(this.SNIPSET_1240)).assertNoErrors();
	}

	private String SNIPSET_1241 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1241() throws Exception {
		validate(file(this.SNIPSET_1241)).assertNoErrors();
	}

	private String SNIPSET_1242 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1242() throws Exception {
		validate(file(this.SNIPSET_1242)).assertNoErrors();
	}

	private String SNIPSET_1243 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1243() throws Exception {
		validate(file(this.SNIPSET_1243)).assertNoErrors();
	}

	private String SNIPSET_1244 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1244() throws Exception {
		validate(file(this.SNIPSET_1244)).assertNoErrors();
	}

	private String SNIPSET_1245 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1245() throws Exception {
		validate(file(this.SNIPSET_1245)).assertNoErrors();
	}

	private String SNIPSET_1246 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1246() throws Exception {
		validate(file(this.SNIPSET_1246)).assertNoErrors();
	}

	private String SNIPSET_1247 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1247() throws Exception {
		validate(file(this.SNIPSET_1247)).assertNoErrors();
	}

	private String SNIPSET_1248 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1248() throws Exception {
		validate(file(this.SNIPSET_1248)).assertNoErrors();
	}

	private String SNIPSET_1249 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1249() throws Exception {
		validate(file(this.SNIPSET_1249)).assertNoErrors();
	}

	private String SNIPSET_1250 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1250() throws Exception {
		validate(file(this.SNIPSET_1250)).assertNoErrors();
	}

	private String SNIPSET_1251 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1251() throws Exception {
		validate(file(this.SNIPSET_1251)).assertNoErrors();
	}

	private String SNIPSET_1252 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1252() throws Exception {
		validate(file(this.SNIPSET_1252)).assertNoErrors();
	}

	private String SNIPSET_1253 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_1253() throws Exception {
		validate(file(this.SNIPSET_1253)).assertNoErrors();
	}

	private String SNIPSET_1254 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1254() throws Exception {
		validate(file(this.SNIPSET_1254)).assertNoErrors();
	}

	private String SNIPSET_1255 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1255() throws Exception {
		validate(file(this.SNIPSET_1255)).assertNoErrors();
	}

	private String SNIPSET_1256 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1256() throws Exception {
		validate(file(this.SNIPSET_1256)).assertNoErrors();
	}

	private String SNIPSET_1257 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1257() throws Exception {
		validate(file(this.SNIPSET_1257)).assertNoErrors();
	}

	private String SNIPSET_1258 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1258() throws Exception {
		validate(file(this.SNIPSET_1258)).assertNoErrors();
	}

	private String SNIPSET_1259 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1259() throws Exception {
		validate(file(this.SNIPSET_1259)).assertNoErrors();
	}

	private String SNIPSET_1260 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1260() throws Exception {
		validate(file(this.SNIPSET_1260)).assertNoErrors();
	}

	private String SNIPSET_1261 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1261() throws Exception {
		validate(file(this.SNIPSET_1261)).assertNoErrors();
	}

	private String SNIPSET_1262 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1262() throws Exception {
		validate(file(this.SNIPSET_1262)).assertNoErrors();
	}

	private String SNIPSET_1263 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1263() throws Exception {
		validate(file(this.SNIPSET_1263)).assertNoErrors();
	}

	private String SNIPSET_1264 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1264() throws Exception {
		validate(file(this.SNIPSET_1264)).assertNoErrors();
	}

	private String SNIPSET_1265 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1265() throws Exception {
		validate(file(this.SNIPSET_1265)).assertNoErrors();
	}

	private String SNIPSET_1266 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1266() throws Exception {
		validate(file(this.SNIPSET_1266)).assertNoErrors();
	}

	private String SNIPSET_1267 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_1267() throws Exception {
		validate(file(this.SNIPSET_1267)).assertNoErrors();
	}

	private String SNIPSET_1268 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1268() throws Exception {
		validate(file(this.SNIPSET_1268)).assertNoErrors();
	}

	private String SNIPSET_1269 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1269() throws Exception {
		validate(file(this.SNIPSET_1269)).assertNoErrors();
	}

	private String SNIPSET_1270 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1270() throws Exception {
		validate(file(this.SNIPSET_1270)).assertNoErrors();
	}

	private String SNIPSET_1271 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1271() throws Exception {
		validate(file(this.SNIPSET_1271)).assertNoErrors();
	}

	private String SNIPSET_1272 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1272() throws Exception {
		validate(file(this.SNIPSET_1272)).assertNoErrors();
	}

	private String SNIPSET_1273 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1273() throws Exception {
		validate(file(this.SNIPSET_1273)).assertNoErrors();
	}

	private String SNIPSET_1274 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1274() throws Exception {
		validate(file(this.SNIPSET_1274)).assertNoErrors();
	}

	private String SNIPSET_1275 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1275() throws Exception {
		validate(file(this.SNIPSET_1275)).assertNoErrors();
	}

	private String SNIPSET_1276 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1276() throws Exception {
		validate(file(this.SNIPSET_1276)).assertNoErrors();
	}

	private String SNIPSET_1277 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1277() throws Exception {
		validate(file(this.SNIPSET_1277)).assertNoErrors();
	}

	private String SNIPSET_1278 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1278() throws Exception {
		validate(file(this.SNIPSET_1278)).assertNoErrors();
	}

	private String SNIPSET_1279 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1279() throws Exception {
		validate(file(this.SNIPSET_1279)).assertNoErrors();
	}

	private String SNIPSET_1280 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1280() throws Exception {
		validate(file(this.SNIPSET_1280)).assertNoErrors();
	}

	private String SNIPSET_1281 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_1281() throws Exception {
		validate(file(this.SNIPSET_1281)).assertNoErrors();
	}

	private String SNIPSET_1282 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1282() throws Exception {
		validate(file(this.SNIPSET_1282)).assertNoErrors();
	}

	private String SNIPSET_1283 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1283() throws Exception {
		validate(file(this.SNIPSET_1283)).assertNoErrors();
	}

	private String SNIPSET_1284 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1284() throws Exception {
		validate(file(this.SNIPSET_1284)).assertNoErrors();
	}

	private String SNIPSET_1285 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1285() throws Exception {
		validate(file(this.SNIPSET_1285)).assertNoErrors();
	}

	private String SNIPSET_1286 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1286() throws Exception {
		validate(file(this.SNIPSET_1286)).assertNoErrors();
	}

	private String SNIPSET_1287 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1287() throws Exception {
		validate(file(this.SNIPSET_1287)).assertNoErrors();
	}

	private String SNIPSET_1288 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1288() throws Exception {
		validate(file(this.SNIPSET_1288)).assertNoErrors();
	}

	private String SNIPSET_1289 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1289() throws Exception {
		validate(file(this.SNIPSET_1289)).assertNoErrors();
	}

	private String SNIPSET_1290 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1290() throws Exception {
		validate(file(this.SNIPSET_1290)).assertNoErrors();
	}

	private String SNIPSET_1291 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1291() throws Exception {
		validate(file(this.SNIPSET_1291)).assertNoErrors();
	}

	private String SNIPSET_1292 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1292() throws Exception {
		validate(file(this.SNIPSET_1292)).assertNoErrors();
	}

	private String SNIPSET_1293 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1293() throws Exception {
		validate(file(this.SNIPSET_1293)).assertNoErrors();
	}

	private String SNIPSET_1294 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1294() throws Exception {
		validate(file(this.SNIPSET_1294)).assertNoErrors();
	}

	private String SNIPSET_1295 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_1295() throws Exception {
		validate(file(this.SNIPSET_1295)).assertNoErrors();
	}

	private String SNIPSET_1296 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1296() throws Exception {
		validate(file(this.SNIPSET_1296)).assertNoErrors();
	}

	private String SNIPSET_1297 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1297() throws Exception {
		validate(file(this.SNIPSET_1297)).assertNoErrors();
	}

	private String SNIPSET_1298 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1298() throws Exception {
		validate(file(this.SNIPSET_1298)).assertNoErrors();
	}

	private String SNIPSET_1299 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1299() throws Exception {
		validate(file(this.SNIPSET_1299)).assertNoErrors();
	}

	private String SNIPSET_1300 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1300() throws Exception {
		validate(file(this.SNIPSET_1300)).assertNoErrors();
	}

	private String SNIPSET_1301 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1301() throws Exception {
		validate(file(this.SNIPSET_1301)).assertNoErrors();
	}

	private String SNIPSET_1302 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1302() throws Exception {
		validate(file(this.SNIPSET_1302)).assertNoErrors();
	}

	private String SNIPSET_1303 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1303() throws Exception {
		validate(file(this.SNIPSET_1303)).assertNoErrors();
	}

	private String SNIPSET_1304 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1304() throws Exception {
		validate(file(this.SNIPSET_1304)).assertNoErrors();
	}

	private String SNIPSET_1305 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1305() throws Exception {
		validate(file(this.SNIPSET_1305)).assertNoErrors();
	}

	private String SNIPSET_1306 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1306() throws Exception {
		validate(file(this.SNIPSET_1306)).assertNoErrors();
	}

	private String SNIPSET_1307 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1307() throws Exception {
		validate(file(this.SNIPSET_1307)).assertNoErrors();
	}

	private String SNIPSET_1308 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1308() throws Exception {
		validate(file(this.SNIPSET_1308)).assertNoErrors();
	}

	private String SNIPSET_1309 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_1309() throws Exception {
		validate(file(this.SNIPSET_1309)).assertNoErrors();
	}

	private String SNIPSET_1310 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1310() throws Exception {
		validate(file(this.SNIPSET_1310)).assertNoErrors();
	}

	private String SNIPSET_1311 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1311() throws Exception {
		validate(file(this.SNIPSET_1311)).assertNoErrors();
	}

	private String SNIPSET_1312 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1312() throws Exception {
		validate(file(this.SNIPSET_1312)).assertNoErrors();
	}

	private String SNIPSET_1313 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1313() throws Exception {
		validate(file(this.SNIPSET_1313)).assertNoErrors();
	}

	private String SNIPSET_1314 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1314() throws Exception {
		validate(file(this.SNIPSET_1314)).assertNoErrors();
	}

	private String SNIPSET_1315 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1315() throws Exception {
		validate(file(this.SNIPSET_1315)).assertNoErrors();
	}

	private String SNIPSET_1316 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1316() throws Exception {
		validate(file(this.SNIPSET_1316)).assertNoErrors();
	}

	private String SNIPSET_1317 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1317() throws Exception {
		validate(file(this.SNIPSET_1317)).assertNoErrors();
	}

	private String SNIPSET_1318 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1318() throws Exception {
		validate(file(this.SNIPSET_1318)).assertNoErrors();
	}

	private String SNIPSET_1319 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1319() throws Exception {
		validate(file(this.SNIPSET_1319)).assertNoErrors();
	}

	private String SNIPSET_1320 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1320() throws Exception {
		validate(file(this.SNIPSET_1320)).assertNoErrors();
	}

	private String SNIPSET_1321 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1321() throws Exception {
		validate(file(this.SNIPSET_1321)).assertNoErrors();
	}

	private String SNIPSET_1322 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1322() throws Exception {
		validate(file(this.SNIPSET_1322)).assertNoErrors();
	}

	private String SNIPSET_1323 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_1323() throws Exception {
		validate(file(this.SNIPSET_1323)).assertNoErrors();
	}

	private String SNIPSET_1324 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1324() throws Exception {
		validate(file(this.SNIPSET_1324)).assertNoErrors();
	}

	private String SNIPSET_1325 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1325() throws Exception {
		validate(file(this.SNIPSET_1325)).assertNoErrors();
	}

	private String SNIPSET_1326 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1326() throws Exception {
		validate(file(this.SNIPSET_1326)).assertNoErrors();
	}

	private String SNIPSET_1327 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1327() throws Exception {
		validate(file(this.SNIPSET_1327)).assertNoErrors();
	}

	private String SNIPSET_1328 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1328() throws Exception {
		validate(file(this.SNIPSET_1328)).assertNoErrors();
	}

	private String SNIPSET_1329 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1329() throws Exception {
		validate(file(this.SNIPSET_1329)).assertNoErrors();
	}

	private String SNIPSET_1330 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1330() throws Exception {
		validate(file(this.SNIPSET_1330)).assertNoErrors();
	}

	private String SNIPSET_1331 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1331() throws Exception {
		validate(file(this.SNIPSET_1331)).assertNoErrors();
	}

	private String SNIPSET_1332 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1332() throws Exception {
		validate(file(this.SNIPSET_1332)).assertNoErrors();
	}

	private String SNIPSET_1333 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1333() throws Exception {
		validate(file(this.SNIPSET_1333)).assertNoErrors();
	}

	private String SNIPSET_1334 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1334() throws Exception {
		validate(file(this.SNIPSET_1334)).assertNoErrors();
	}

	private String SNIPSET_1335 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1335() throws Exception {
		validate(file(this.SNIPSET_1335)).assertNoErrors();
	}

	private String SNIPSET_1336 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1336() throws Exception {
		validate(file(this.SNIPSET_1336)).assertNoErrors();
	}

	private String SNIPSET_1337 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_1337() throws Exception {
		validate(file(this.SNIPSET_1337)).assertNoErrors();
	}

	private String SNIPSET_1338 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : byte) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1338() throws Exception {
		validate(file(this.SNIPSET_1338)).assertNoErrors();
	}

	private String SNIPSET_1339 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : long) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1339() throws Exception {
		validate(file(this.SNIPSET_1339)).assertNoErrors();
	}

	private String SNIPSET_1340 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1340() throws Exception {
		validate(file(this.SNIPSET_1340)).assertNoErrors();
	}

	private String SNIPSET_1341 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : short) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1341() throws Exception {
		validate(file(this.SNIPSET_1341)).assertNoErrors();
	}

	private String SNIPSET_1342 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : int) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1342() throws Exception {
		validate(file(this.SNIPSET_1342)).assertNoErrors();
	}

	private String SNIPSET_1343 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1343() throws Exception {
		validate(file(this.SNIPSET_1343)).assertNoErrors();
	}

	private String SNIPSET_1344 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Long) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1344() throws Exception {
		validate(file(this.SNIPSET_1344)).assertNoErrors();
	}

	private String SNIPSET_1345 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1345() throws Exception {
		validate(file(this.SNIPSET_1345)).assertNoErrors();
	}

	private String SNIPSET_1346 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Integer) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1346() throws Exception {
		validate(file(this.SNIPSET_1346)).assertNoErrors();
	}

	private String SNIPSET_1347 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1347() throws Exception {
		validate(file(this.SNIPSET_1347)).assertNoErrors();
	}

	private String SNIPSET_1348 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Byte) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1348() throws Exception {
		validate(file(this.SNIPSET_1348)).assertNoErrors();
	}

	private String SNIPSET_1349 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : Short) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1349() throws Exception {
		validate(file(this.SNIPSET_1349)).assertNoErrors();
	}

	private String SNIPSET_1350 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicLong) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1350() throws Exception {
		validate(file(this.SNIPSET_1350)).assertNoErrors();
	}

	private String SNIPSET_1351 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : Float, b : AtomicInteger) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_1351() throws Exception {
		validate(file(this.SNIPSET_1351)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
