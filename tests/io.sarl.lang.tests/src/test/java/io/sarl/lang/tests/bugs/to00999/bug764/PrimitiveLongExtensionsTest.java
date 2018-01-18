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
public class PrimitiveLongExtensionsTest extends AbstractSarlTest {

	// BEGIN GENERATED BLOCK

	private String SNIPSET_169 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long) : long {",
		"      -a",
		"   }",
		"}");

	@Test
	public void parsing_169() throws Exception {
		validate(file(this.SNIPSET_169)).assertNoErrors();
	}

	private String SNIPSET_170 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_170() throws Exception {
		validate(file(this.SNIPSET_170)).assertNoErrors();
	}

	private String SNIPSET_171 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_171() throws Exception {
		validate(file(this.SNIPSET_171)).assertNoErrors();
	}

	private String SNIPSET_172 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_172() throws Exception {
		validate(file(this.SNIPSET_172)).assertNoErrors();
	}

	private String SNIPSET_173 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_173() throws Exception {
		validate(file(this.SNIPSET_173)).assertNoErrors();
	}

	private String SNIPSET_174 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_174() throws Exception {
		validate(file(this.SNIPSET_174)).assertNoErrors();
	}

	private String SNIPSET_175 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_175() throws Exception {
		validate(file(this.SNIPSET_175)).assertNoErrors();
	}

	private String SNIPSET_176 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_176() throws Exception {
		validate(file(this.SNIPSET_176)).assertNoErrors();
	}

	private String SNIPSET_177 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_177() throws Exception {
		validate(file(this.SNIPSET_177)).assertNoErrors();
	}

	private String SNIPSET_178 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_178() throws Exception {
		validate(file(this.SNIPSET_178)).assertNoErrors();
	}

	private String SNIPSET_179 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_179() throws Exception {
		validate(file(this.SNIPSET_179)).assertNoErrors();
	}

	private String SNIPSET_180 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_180() throws Exception {
		validate(file(this.SNIPSET_180)).assertNoErrors();
	}

	private String SNIPSET_181 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_181() throws Exception {
		validate(file(this.SNIPSET_181)).assertNoErrors();
	}

	private String SNIPSET_182 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_182() throws Exception {
		validate(file(this.SNIPSET_182)).assertNoErrors();
	}

	private String SNIPSET_183 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : boolean {",
		"      a > b",
		"   }",
		"}");

	@Test
	public void parsing_183() throws Exception {
		validate(file(this.SNIPSET_183)).assertNoErrors();
	}

	private String SNIPSET_184 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_184() throws Exception {
		validate(file(this.SNIPSET_184)).assertNoErrors();
	}

	private String SNIPSET_185 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_185() throws Exception {
		validate(file(this.SNIPSET_185)).assertNoErrors();
	}

	private String SNIPSET_186 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_186() throws Exception {
		validate(file(this.SNIPSET_186)).assertNoErrors();
	}

	private String SNIPSET_187 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_187() throws Exception {
		validate(file(this.SNIPSET_187)).assertNoErrors();
	}

	private String SNIPSET_188 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_188() throws Exception {
		validate(file(this.SNIPSET_188)).assertNoErrors();
	}

	private String SNIPSET_189 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_189() throws Exception {
		validate(file(this.SNIPSET_189)).assertNoErrors();
	}

	private String SNIPSET_190 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_190() throws Exception {
		validate(file(this.SNIPSET_190)).assertNoErrors();
	}

	private String SNIPSET_191 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : float {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_191() throws Exception {
		validate(file(this.SNIPSET_191)).assertNoErrors();
	}

	private String SNIPSET_192 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_192() throws Exception {
		validate(file(this.SNIPSET_192)).assertNoErrors();
	}

	private String SNIPSET_193 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : double {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_193() throws Exception {
		validate(file(this.SNIPSET_193)).assertNoErrors();
	}

	private String SNIPSET_194 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_194() throws Exception {
		validate(file(this.SNIPSET_194)).assertNoErrors();
	}

	private String SNIPSET_195 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_195() throws Exception {
		validate(file(this.SNIPSET_195)).assertNoErrors();
	}

	private String SNIPSET_196 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_196() throws Exception {
		validate(file(this.SNIPSET_196)).assertNoErrors();
	}

	private String SNIPSET_197 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : long {",
		"      a % b",
		"   }",
		"}");

	@Test
	public void parsing_197() throws Exception {
		validate(file(this.SNIPSET_197)).assertNoErrors();
	}

	private String SNIPSET_198 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_198() throws Exception {
		validate(file(this.SNIPSET_198)).assertNoErrors();
	}

	private String SNIPSET_199 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_199() throws Exception {
		validate(file(this.SNIPSET_199)).assertNoErrors();
	}

	private String SNIPSET_200 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_200() throws Exception {
		validate(file(this.SNIPSET_200)).assertNoErrors();
	}

	private String SNIPSET_201 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_201() throws Exception {
		validate(file(this.SNIPSET_201)).assertNoErrors();
	}

	private String SNIPSET_202 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_202() throws Exception {
		validate(file(this.SNIPSET_202)).assertNoErrors();
	}

	private String SNIPSET_203 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_203() throws Exception {
		validate(file(this.SNIPSET_203)).assertNoErrors();
	}

	private String SNIPSET_204 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_204() throws Exception {
		validate(file(this.SNIPSET_204)).assertNoErrors();
	}

	private String SNIPSET_205 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_205() throws Exception {
		validate(file(this.SNIPSET_205)).assertNoErrors();
	}

	private String SNIPSET_206 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_206() throws Exception {
		validate(file(this.SNIPSET_206)).assertNoErrors();
	}

	private String SNIPSET_207 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_207() throws Exception {
		validate(file(this.SNIPSET_207)).assertNoErrors();
	}

	private String SNIPSET_208 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_208() throws Exception {
		validate(file(this.SNIPSET_208)).assertNoErrors();
	}

	private String SNIPSET_209 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_209() throws Exception {
		validate(file(this.SNIPSET_209)).assertNoErrors();
	}

	private String SNIPSET_210 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_210() throws Exception {
		validate(file(this.SNIPSET_210)).assertNoErrors();
	}

	private String SNIPSET_211 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : boolean {",
		"      a <= b",
		"   }",
		"}");

	@Test
	public void parsing_211() throws Exception {
		validate(file(this.SNIPSET_211)).assertNoErrors();
	}

	private String SNIPSET_212 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_212() throws Exception {
		validate(file(this.SNIPSET_212)).assertNoErrors();
	}

	private String SNIPSET_213 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_213() throws Exception {
		validate(file(this.SNIPSET_213)).assertNoErrors();
	}

	private String SNIPSET_214 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_214() throws Exception {
		validate(file(this.SNIPSET_214)).assertNoErrors();
	}

	private String SNIPSET_215 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_215() throws Exception {
		validate(file(this.SNIPSET_215)).assertNoErrors();
	}

	private String SNIPSET_216 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_216() throws Exception {
		validate(file(this.SNIPSET_216)).assertNoErrors();
	}

	private String SNIPSET_217 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_217() throws Exception {
		validate(file(this.SNIPSET_217)).assertNoErrors();
	}

	private String SNIPSET_218 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_218() throws Exception {
		validate(file(this.SNIPSET_218)).assertNoErrors();
	}

	private String SNIPSET_219 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : float {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_219() throws Exception {
		validate(file(this.SNIPSET_219)).assertNoErrors();
	}

	private String SNIPSET_220 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_220() throws Exception {
		validate(file(this.SNIPSET_220)).assertNoErrors();
	}

	private String SNIPSET_221 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : double {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_221() throws Exception {
		validate(file(this.SNIPSET_221)).assertNoErrors();
	}

	private String SNIPSET_222 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_222() throws Exception {
		validate(file(this.SNIPSET_222)).assertNoErrors();
	}

	private String SNIPSET_223 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_223() throws Exception {
		validate(file(this.SNIPSET_223)).assertNoErrors();
	}

	private String SNIPSET_224 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_224() throws Exception {
		validate(file(this.SNIPSET_224)).assertNoErrors();
	}

	private String SNIPSET_225 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : long {",
		"      a / b",
		"   }",
		"}");

	@Test
	public void parsing_225() throws Exception {
		validate(file(this.SNIPSET_225)).assertNoErrors();
	}

	private String SNIPSET_226 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_226() throws Exception {
		validate(file(this.SNIPSET_226)).assertNoErrors();
	}

	private String SNIPSET_227 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_227() throws Exception {
		validate(file(this.SNIPSET_227)).assertNoErrors();
	}

	private String SNIPSET_228 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_228() throws Exception {
		validate(file(this.SNIPSET_228)).assertNoErrors();
	}

	private String SNIPSET_229 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_229() throws Exception {
		validate(file(this.SNIPSET_229)).assertNoErrors();
	}

	private String SNIPSET_230 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_230() throws Exception {
		validate(file(this.SNIPSET_230)).assertNoErrors();
	}

	private String SNIPSET_231 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_231() throws Exception {
		validate(file(this.SNIPSET_231)).assertNoErrors();
	}

	private String SNIPSET_232 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_232() throws Exception {
		validate(file(this.SNIPSET_232)).assertNoErrors();
	}

	private String SNIPSET_233 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : float {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_233() throws Exception {
		validate(file(this.SNIPSET_233)).assertNoErrors();
	}

	private String SNIPSET_234 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_234() throws Exception {
		validate(file(this.SNIPSET_234)).assertNoErrors();
	}

	private String SNIPSET_235 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : double {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_235() throws Exception {
		validate(file(this.SNIPSET_235)).assertNoErrors();
	}

	private String SNIPSET_236 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_236() throws Exception {
		validate(file(this.SNIPSET_236)).assertNoErrors();
	}

	private String SNIPSET_237 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_237() throws Exception {
		validate(file(this.SNIPSET_237)).assertNoErrors();
	}

	private String SNIPSET_238 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_238() throws Exception {
		validate(file(this.SNIPSET_238)).assertNoErrors();
	}

	private String SNIPSET_239 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : long {",
		"      a * b",
		"   }",
		"}");

	@Test
	public void parsing_239() throws Exception {
		validate(file(this.SNIPSET_239)).assertNoErrors();
	}

	private String SNIPSET_240 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_240() throws Exception {
		validate(file(this.SNIPSET_240)).assertNoErrors();
	}

	private String SNIPSET_241 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_241() throws Exception {
		validate(file(this.SNIPSET_241)).assertNoErrors();
	}

	private String SNIPSET_242 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_242() throws Exception {
		validate(file(this.SNIPSET_242)).assertNoErrors();
	}

	private String SNIPSET_243 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_243() throws Exception {
		validate(file(this.SNIPSET_243)).assertNoErrors();
	}

	private String SNIPSET_244 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_244() throws Exception {
		validate(file(this.SNIPSET_244)).assertNoErrors();
	}

	private String SNIPSET_245 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_245() throws Exception {
		validate(file(this.SNIPSET_245)).assertNoErrors();
	}

	private String SNIPSET_246 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_246() throws Exception {
		validate(file(this.SNIPSET_246)).assertNoErrors();
	}

	private String SNIPSET_247 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_247() throws Exception {
		validate(file(this.SNIPSET_247)).assertNoErrors();
	}

	private String SNIPSET_248 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_248() throws Exception {
		validate(file(this.SNIPSET_248)).assertNoErrors();
	}

	private String SNIPSET_249 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_249() throws Exception {
		validate(file(this.SNIPSET_249)).assertNoErrors();
	}

	private String SNIPSET_250 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_250() throws Exception {
		validate(file(this.SNIPSET_250)).assertNoErrors();
	}

	private String SNIPSET_251 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_251() throws Exception {
		validate(file(this.SNIPSET_251)).assertNoErrors();
	}

	private String SNIPSET_252 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_252() throws Exception {
		validate(file(this.SNIPSET_252)).assertNoErrors();
	}

	private String SNIPSET_253 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : boolean {",
		"      a == b",
		"   }",
		"}");

	@Test
	public void parsing_253() throws Exception {
		validate(file(this.SNIPSET_253)).assertNoErrors();
	}

	private String SNIPSET_254 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_254() throws Exception {
		validate(file(this.SNIPSET_254)).assertNoErrors();
	}

	private String SNIPSET_255 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_255() throws Exception {
		validate(file(this.SNIPSET_255)).assertNoErrors();
	}

	private String SNIPSET_256 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_256() throws Exception {
		validate(file(this.SNIPSET_256)).assertNoErrors();
	}

	private String SNIPSET_257 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_257() throws Exception {
		validate(file(this.SNIPSET_257)).assertNoErrors();
	}

	private String SNIPSET_258 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_258() throws Exception {
		validate(file(this.SNIPSET_258)).assertNoErrors();
	}

	private String SNIPSET_259 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_259() throws Exception {
		validate(file(this.SNIPSET_259)).assertNoErrors();
	}

	private String SNIPSET_260 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_260() throws Exception {
		validate(file(this.SNIPSET_260)).assertNoErrors();
	}

	private String SNIPSET_261 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_261() throws Exception {
		validate(file(this.SNIPSET_261)).assertNoErrors();
	}

	private String SNIPSET_262 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_262() throws Exception {
		validate(file(this.SNIPSET_262)).assertNoErrors();
	}

	private String SNIPSET_263 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_263() throws Exception {
		validate(file(this.SNIPSET_263)).assertNoErrors();
	}

	private String SNIPSET_264 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_264() throws Exception {
		validate(file(this.SNIPSET_264)).assertNoErrors();
	}

	private String SNIPSET_265 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_265() throws Exception {
		validate(file(this.SNIPSET_265)).assertNoErrors();
	}

	private String SNIPSET_266 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_266() throws Exception {
		validate(file(this.SNIPSET_266)).assertNoErrors();
	}

	private String SNIPSET_267 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : boolean {",
		"      a < b",
		"   }",
		"}");

	@Test
	public void parsing_267() throws Exception {
		validate(file(this.SNIPSET_267)).assertNoErrors();
	}

	private String SNIPSET_268 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_268() throws Exception {
		validate(file(this.SNIPSET_268)).assertNoErrors();
	}

	private String SNIPSET_269 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_269() throws Exception {
		validate(file(this.SNIPSET_269)).assertNoErrors();
	}

	private String SNIPSET_270 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_270() throws Exception {
		validate(file(this.SNIPSET_270)).assertNoErrors();
	}

	private String SNIPSET_271 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_271() throws Exception {
		validate(file(this.SNIPSET_271)).assertNoErrors();
	}

	private String SNIPSET_272 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_272() throws Exception {
		validate(file(this.SNIPSET_272)).assertNoErrors();
	}

	private String SNIPSET_273 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_273() throws Exception {
		validate(file(this.SNIPSET_273)).assertNoErrors();
	}

	private String SNIPSET_274 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_274() throws Exception {
		validate(file(this.SNIPSET_274)).assertNoErrors();
	}

	private String SNIPSET_275 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_275() throws Exception {
		validate(file(this.SNIPSET_275)).assertNoErrors();
	}

	private String SNIPSET_276 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_276() throws Exception {
		validate(file(this.SNIPSET_276)).assertNoErrors();
	}

	private String SNIPSET_277 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_277() throws Exception {
		validate(file(this.SNIPSET_277)).assertNoErrors();
	}

	private String SNIPSET_278 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_278() throws Exception {
		validate(file(this.SNIPSET_278)).assertNoErrors();
	}

	private String SNIPSET_279 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_279() throws Exception {
		validate(file(this.SNIPSET_279)).assertNoErrors();
	}

	private String SNIPSET_280 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_280() throws Exception {
		validate(file(this.SNIPSET_280)).assertNoErrors();
	}

	private String SNIPSET_281 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : double {",
		"      a ** b",
		"   }",
		"}");

	@Test
	public void parsing_281() throws Exception {
		validate(file(this.SNIPSET_281)).assertNoErrors();
	}

	private String SNIPSET_282 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_282() throws Exception {
		validate(file(this.SNIPSET_282)).assertNoErrors();
	}

	private String SNIPSET_283 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_283() throws Exception {
		validate(file(this.SNIPSET_283)).assertNoErrors();
	}

	private String SNIPSET_284 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_284() throws Exception {
		validate(file(this.SNIPSET_284)).assertNoErrors();
	}

	private String SNIPSET_285 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_285() throws Exception {
		validate(file(this.SNIPSET_285)).assertNoErrors();
	}

	private String SNIPSET_286 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_286() throws Exception {
		validate(file(this.SNIPSET_286)).assertNoErrors();
	}

	private String SNIPSET_287 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_287() throws Exception {
		validate(file(this.SNIPSET_287)).assertNoErrors();
	}

	private String SNIPSET_288 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_288() throws Exception {
		validate(file(this.SNIPSET_288)).assertNoErrors();
	}

	private String SNIPSET_289 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_289() throws Exception {
		validate(file(this.SNIPSET_289)).assertNoErrors();
	}

	private String SNIPSET_290 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_290() throws Exception {
		validate(file(this.SNIPSET_290)).assertNoErrors();
	}

	private String SNIPSET_291 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_291() throws Exception {
		validate(file(this.SNIPSET_291)).assertNoErrors();
	}

	private String SNIPSET_292 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_292() throws Exception {
		validate(file(this.SNIPSET_292)).assertNoErrors();
	}

	private String SNIPSET_293 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_293() throws Exception {
		validate(file(this.SNIPSET_293)).assertNoErrors();
	}

	private String SNIPSET_294 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_294() throws Exception {
		validate(file(this.SNIPSET_294)).assertNoErrors();
	}

	private String SNIPSET_295 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : boolean {",
		"      a >= b",
		"   }",
		"}");

	@Test
	public void parsing_295() throws Exception {
		validate(file(this.SNIPSET_295)).assertNoErrors();
	}

	private String SNIPSET_296 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_296() throws Exception {
		validate(file(this.SNIPSET_296)).assertNoErrors();
	}

	private String SNIPSET_297 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_297() throws Exception {
		validate(file(this.SNIPSET_297)).assertNoErrors();
	}

	private String SNIPSET_298 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_298() throws Exception {
		validate(file(this.SNIPSET_298)).assertNoErrors();
	}

	private String SNIPSET_299 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_299() throws Exception {
		validate(file(this.SNIPSET_299)).assertNoErrors();
	}

	private String SNIPSET_300 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_300() throws Exception {
		validate(file(this.SNIPSET_300)).assertNoErrors();
	}

	private String SNIPSET_301 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_301() throws Exception {
		validate(file(this.SNIPSET_301)).assertNoErrors();
	}

	private String SNIPSET_302 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_302() throws Exception {
		validate(file(this.SNIPSET_302)).assertNoErrors();
	}

	private String SNIPSET_303 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_303() throws Exception {
		validate(file(this.SNIPSET_303)).assertNoErrors();
	}

	private String SNIPSET_304 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_304() throws Exception {
		validate(file(this.SNIPSET_304)).assertNoErrors();
	}

	private String SNIPSET_305 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_305() throws Exception {
		validate(file(this.SNIPSET_305)).assertNoErrors();
	}

	private String SNIPSET_306 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_306() throws Exception {
		validate(file(this.SNIPSET_306)).assertNoErrors();
	}

	private String SNIPSET_307 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_307() throws Exception {
		validate(file(this.SNIPSET_307)).assertNoErrors();
	}

	private String SNIPSET_308 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_308() throws Exception {
		validate(file(this.SNIPSET_308)).assertNoErrors();
	}

	private String SNIPSET_309 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : boolean {",
		"      a != b",
		"   }",
		"}");

	@Test
	public void parsing_309() throws Exception {
		validate(file(this.SNIPSET_309)).assertNoErrors();
	}

	private String SNIPSET_310 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_310() throws Exception {
		validate(file(this.SNIPSET_310)).assertNoErrors();
	}

	private String SNIPSET_311 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_311() throws Exception {
		validate(file(this.SNIPSET_311)).assertNoErrors();
	}

	private String SNIPSET_312 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_312() throws Exception {
		validate(file(this.SNIPSET_312)).assertNoErrors();
	}

	private String SNIPSET_313 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_313() throws Exception {
		validate(file(this.SNIPSET_313)).assertNoErrors();
	}

	private String SNIPSET_314 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_314() throws Exception {
		validate(file(this.SNIPSET_314)).assertNoErrors();
	}

	private String SNIPSET_315 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_315() throws Exception {
		validate(file(this.SNIPSET_315)).assertNoErrors();
	}

	private String SNIPSET_316 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_316() throws Exception {
		validate(file(this.SNIPSET_316)).assertNoErrors();
	}

	private String SNIPSET_317 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : float {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_317() throws Exception {
		validate(file(this.SNIPSET_317)).assertNoErrors();
	}

	private String SNIPSET_318 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_318() throws Exception {
		validate(file(this.SNIPSET_318)).assertNoErrors();
	}

	private String SNIPSET_319 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : double {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_319() throws Exception {
		validate(file(this.SNIPSET_319)).assertNoErrors();
	}

	private String SNIPSET_320 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_320() throws Exception {
		validate(file(this.SNIPSET_320)).assertNoErrors();
	}

	private String SNIPSET_321 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_321() throws Exception {
		validate(file(this.SNIPSET_321)).assertNoErrors();
	}

	private String SNIPSET_322 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_322() throws Exception {
		validate(file(this.SNIPSET_322)).assertNoErrors();
	}

	private String SNIPSET_323 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : long {",
		"      a - b",
		"   }",
		"}");

	@Test
	public void parsing_323() throws Exception {
		validate(file(this.SNIPSET_323)).assertNoErrors();
	}

	private String SNIPSET_324 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : byte) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_324() throws Exception {
		validate(file(this.SNIPSET_324)).assertNoErrors();
	}

	private String SNIPSET_325 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_325() throws Exception {
		validate(file(this.SNIPSET_325)).assertNoErrors();
	}

	private String SNIPSET_326 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_326() throws Exception {
		validate(file(this.SNIPSET_326)).assertNoErrors();
	}

	private String SNIPSET_327 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : short) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_327() throws Exception {
		validate(file(this.SNIPSET_327)).assertNoErrors();
	}

	private String SNIPSET_328 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : int) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_328() throws Exception {
		validate(file(this.SNIPSET_328)).assertNoErrors();
	}

	private String SNIPSET_329 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_329() throws Exception {
		validate(file(this.SNIPSET_329)).assertNoErrors();
	}

	private String SNIPSET_330 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Long) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_330() throws Exception {
		validate(file(this.SNIPSET_330)).assertNoErrors();
	}

	private String SNIPSET_331 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Float) : float {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_331() throws Exception {
		validate(file(this.SNIPSET_331)).assertNoErrors();
	}

	private String SNIPSET_332 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Integer) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_332() throws Exception {
		validate(file(this.SNIPSET_332)).assertNoErrors();
	}

	private String SNIPSET_333 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Double) : double {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_333() throws Exception {
		validate(file(this.SNIPSET_333)).assertNoErrors();
	}

	private String SNIPSET_334 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Byte) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_334() throws Exception {
		validate(file(this.SNIPSET_334)).assertNoErrors();
	}

	private String SNIPSET_335 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : Short) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_335() throws Exception {
		validate(file(this.SNIPSET_335)).assertNoErrors();
	}

	private String SNIPSET_336 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicLong) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_336() throws Exception {
		validate(file(this.SNIPSET_336)).assertNoErrors();
	}

	private String SNIPSET_337 = multilineString(
		"package io.sarl.lang.tests.bug764",
		"import java.util.concurrent.atomic.AtomicInteger;",
		"import java.util.concurrent.atomic.AtomicLong;",
		"class X {",
		"   def fct(a : long, b : AtomicInteger) : long {",
		"      a + b",
		"   }",
		"}");

	@Test
	public void parsing_337() throws Exception {
		validate(file(this.SNIPSET_337)).assertNoErrors();
	}

	// END GENERATED BLOCK

}
