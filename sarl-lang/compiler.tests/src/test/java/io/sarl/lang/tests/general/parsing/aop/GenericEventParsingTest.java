/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.tests.general.parsing.aop;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.eclipse.xtext.xbase.validation.IssueCodes.*;
import static io.sarl.lang.validation.IssueCodes.*;

import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.xtext.XtextPackage;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @since 0.14
 */
@DisplayName("Parsing: generic Event")
@Tag("core")
@Tag("sarlValidation")
@SuppressWarnings("all")
public class GenericEventParsingTest {

	@DisplayName("Direct Event inheritance")
	@Nested
	public class DirectEventInheritance extends AbstractSarlTest {

		@Test
		@DisplayName("Empty event with block")
		public void basicCompile_withBlock() throws Exception {
			SarlScript mas = file(getParseHelper(), "event E1<T1, T2 extends Number, T3 extends Double> { }");
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("Empty event without block")
		public void basicCompile_withoutBlock() throws Exception {
			SarlScript mas = file(getParseHelper(), "event E1<T1, T2 extends Number, T3 extends Double>");
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With string field")
		public void withVarAttributesCompile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var name : String",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With constructor")
		public void noStaticField() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	val titi : int = 4",
					"	val toto : int",
					"	new(a : int) {",
					"		this.toto = a",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Without modifier")
		public void eventmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With public modifier")
		public void eventmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"public event E1<T1, T2 extends Number, T3 extends Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With abstract modifier")
		public void eventmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"abstract event E1<T1, T2 extends Number, T3 extends Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With package modifier")
		public void eventmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"package event E1<T1, T2 extends Number, T3 extends Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With final modifier")
		public void eventmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"final event E1<T1, T2 extends Number, T3 extends Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("Without field modifier")
		public void fieldmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	var field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With public field modifier")
		public void fieldmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	public var field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Without constructor modifier")
		public void constructormodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With public constructor modifier")
		public void constructormodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	public new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With private constructor modifier")
		public void constructormodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	private new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With package constructor modifier")
		public void constructormodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	package new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With protected constructor modifier")
		public void constructormodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"	protected new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

	}

	@DisplayName("Same super generic prototype")
	@Nested
	public class SameGenericEventInheritance extends AbstractSarlTest {

		@Test
		@DisplayName("Empty event with block")
		public void basicCompile_withBlock() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("Empty event without block")
		public void basicCompile_withoutBlock() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1 extends Object, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With string field")
		public void withVarAttributesCompile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"  var name : String",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With constructor")
		public void noStaticField() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3> {",
					"	val titi : int = 4",
					"	val toto : int",
					"	new(a : int) {",
					"		this.toto = a",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Without modifier")
		public void eventmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With public modifier")
		public void eventmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"public event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With abstract modifier")
		public void eventmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"abstract event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With package modifier")
		public void eventmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"package event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With final modifier")
		public void eventmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"final event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("Without field modifier")
		public void fieldmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	var field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With public field modifier")
		public void fieldmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	public var field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Without constructor modifier")
		public void constructormodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With public constructor modifier")
		public void constructormodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	public new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With private constructor modifier")
		public void constructormodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	private new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With package constructor modifier")
		public void constructormodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	package new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With protected constructor modifier")
		public void constructormodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T3>{",
					"	protected new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

	}

	@DisplayName("Fixed super generic prototype")
	@Nested
	public class FixedGenericEventInheritance extends AbstractSarlTest {

		@Test
		@DisplayName("Empty event with block")
		public void basicCompile_withBlock() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("Empty event without block")
		public void basicCompile_withoutBlock() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With string field")
		public void withVarAttributesCompile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"  var name : String",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With constructor")
		public void noStaticField() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	val titi : int = 4",
					"	val toto : int",
					"	new(a : int) {",
					"		this.toto = a",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Without modifier")
		public void eventmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With public modifier")
		public void eventmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"public event E2 extends E1<String, Float, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With abstract modifier")
		public void eventmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"abstract event E2 extends E1<String, Float, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With package modifier")
		public void eventmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"package event E2 extends E1<String, Float, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With final modifier")
		public void eventmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"final event E2 extends E1<String, Float, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("Without field modifier")
		public void fieldmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	var field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With public field modifier")
		public void fieldmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	public var field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Without constructor modifier")
		public void constructormodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With public constructor modifier")
		public void constructormodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	public new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With private constructor modifier")
		public void constructormodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	private new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With package constructor modifier")
		public void constructormodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	package new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With protected constructor modifier")
		public void constructormodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2 extends E1<String, Float, Double> {",
					"	protected new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

	}

	@DisplayName("Limited super generic prototype")
	@Nested
	public class LimitedGenericEventInheritance extends AbstractSarlTest {

		@Test
		@DisplayName("Empty event with block")
		public void basicCompile_withBlock() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> { }"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("Empty event without block")
		public void basicCompile_withoutBlock() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With string field")
		public void withVarAttributesCompile() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"  var name : String",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With constructor")
		public void noStaticField() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	val titi : int = 4",
					"	val toto : int",
					"	new(a : int) {",
					"		this.toto = a",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Without modifier")
		public void eventmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With public modifier")
		public void eventmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"public event E2<T2 extends Number> extends E1<String, T2, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With abstract modifier")
		public void eventmodifier_abstract() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"abstract event E2<T2 extends Number> extends E1<String, T2, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With package modifier")
		public void eventmodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"package event E2<T2 extends Number> extends E1<String, T2, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With final modifier")
		public void eventmodifier_final() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"final event E2<T2 extends Number> extends E1<String, T2, Double>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("Without field modifier")
		public void fieldmodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	var field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With public field modifier")
		public void fieldmodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	public var field : int",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Without constructor modifier")
		public void constructormodifier_none() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With public constructor modifier")
		public void constructormodifier_public() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	public new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With private constructor modifier")
		public void constructormodifier_private() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	private new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}
	
		@Test
		@DisplayName("With package constructor modifier")
		public void constructormodifier_package() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	package new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("With protected constructor modifier")
		public void constructormodifier_protected() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> { }",
					"event E2<T2 extends Number> extends E1<String, T2, Double> {",
					"	protected new { super(null) }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

	}

	@DisplayName("Cycle in inheritance tree")
	@Nested
	public class InheritanceCycle extends AbstractSarlTest {

		@Test
		@DisplayName("Direct cycle")
		public void directCycle() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> extends E2<T1, T2, T3>",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T2>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Event'")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Indirect cycle")
		public void indirectCycle() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> extends E3<T1, T2, T3>",
					"event E2<T1, T2 extends Number, T3 extends Double> extends E1<T1, T2, T2>",
					"event E3<T1, T2 extends Number, T3 extends Double> extends E2<T1, T2, T2>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(),
					INVALID_EXTENDED_TYPE,
					"Supertype must be of type 'io.sarl.lang.core.Event'")
				.assertNoErrors();
		}

	}

	@DisplayName("Invalid Bounds")
	@Nested
	public class InvalidBounds extends AbstractSarlTest {

		@Test
		@DisplayName("Invalid generic parameter in extends")
		public void invalidTypeInExtends0() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"event E2<T2 extends Number> extends E1<String, Double, Double>",
					"event E3<T2 extends Number> extends E1<String, Double, String>"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertError(SarlPackage.eINSTANCE.getSarlEvent(),
					TYPE_BOUNDS_MISMATCH,
					"Bounds mismatch: The type arguments <String, Double, String> are not a valid substitute for the bounded type parameters <T1 extends Object, T2 extends Number, T3 extends Double> of the super type E1")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Invalid upper bound in extends")
		public void invalidTypeInExtends1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"event E2<T2 extends Number> extends E1<String, Double, Double>",
					"event E3<T2 extends Number> extends E1<String, Double, Integer>"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertError(SarlPackage.eINSTANCE.getSarlEvent(), TYPE_BOUNDS_MISMATCH,
					"Bounds mismatch: The type arguments <String, Double, Integer> are not a valid substitute for the bounded type parameters <T1 extends Object, T2 extends Number, T3 extends Double> of the super type E1")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Type parameter switch")
		public void typeSwitch() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"event E2<T2 extends Number> extends E1<String, Double, T2>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlEvent(), TYPE_BOUNDS_MISMATCH,
					"Bounds mismatch: The type arguments <String, Double, T2> are not a valid substitute for the bounded type parameters <T1 extends Object, T2 extends Number, T3 extends Double> of the super type E1")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Not enough type parameters")
		public void notEnoughTypeParameters() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"event E2<T2 extends Number> extends E1<T2>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					INVALID_NUMBER_OF_TYPE_ARGUMENTS,
					"Incorrect number of arguments for type E1<T1, T2, T3>; it cannot be parameterized with arguments <T2>")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Missed type parameters")
		public void missedTypeParameters() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"event E2<T2 extends Number> extends E1"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					RAW_TYPE,
					"E1 is a raw type. References to generic type E1<T1, T2, T3> should be parameterized")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Too many type parameters")
		public void tooManyTypeParameters() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"event E2<T2 extends Number> extends E1<String, T2, Double, String>"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					INVALID_NUMBER_OF_TYPE_ARGUMENTS,
					"Incorrect number of arguments for type E1<T1, T2, T3>; it cannot be parameterized with arguments <String, T2, Double, String>")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Valid bounds in new")
		public void validBoundsInNew() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Event",
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"class C1 {",
					"  def theFunction : Event {",
					"    return new E1<String, Float, Double>",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Invalid arguments in new")
		public void invalidArgumentsInNew() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Event",
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"class C1 {",
					"  def theFunction : Event {",
					"    return new E1<String, Float, Integer>",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXConstructorCall(),
					TYPE_BOUNDS_MISMATCH,
					"Bounds mismatch: The type arguments <String, Float, Integer> are not a valid substitute for the bounded type parameters <T1, T2 extends Number, T3 extends Double> of the constructor E1<T1, T2, T3>()")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Not enough type arguments in new")
		public void notEnoughArgsInNew() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Event",
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"class C1 {",
					"  def theFunction : Event {",
					"    return new E1<String, Float>",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXConstructorCall(),
					INVALID_NUMBER_OF_TYPE_ARGUMENTS,
					"Invalid number of type arguments. The constructor E1<T1, T2, T3><T1, T2 extends Number, T3 extends Double> is not applicable for the type arguments <String, Float>")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Too many type arguments in new")
		public void tooManyArgsInNew() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Event",
					"event E1<T1, T2 extends Number, T3 extends Double>",
					"class C1 {",
					"  def theFunction : Event {",
					"    return new E1<String, Float, Double, String>",
					"  }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					XbasePackage.eINSTANCE.getXConstructorCall(),
					INVALID_NUMBER_OF_TYPE_ARGUMENTS,
					"Invalid number of type arguments. The constructor E1<T1, T2, T3><T1, T2 extends Number, T3 extends Double> is not applicable for the type arguments <String, Float, Double, String>")
				.assertNoErrors();
		}

	}


	@DisplayName("Event with generic-type fields")
	@Nested
	public class GenericTypeFieldEvent extends AbstractSarlTest {

		@Test
		@DisplayName("Field T1")
		public void singleField1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field1 : T1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Field T2")
		public void singleField2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field2 : T2",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Field T3")
		public void singleField3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field3 : T3",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Fields T1 T2")
		public void twoField1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field1 : T1",
					"  var field2 : T2",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Fields T1 T3")
		public void twoField2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field1 : T1",
					"  var field3 : T3",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Fields T2 T3")
		public void twoField3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field2 : T2",
					"  var field3 : T3",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Fields T1 T2 T3")
		public void threeField() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field1 : T1",
					"  var field2 : T2",
					"  var field3 : T3",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Fields T1 T1")
		public void twoSingleTypeFields1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field1a : T1",
					"  var field1b : T1",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Fields T2 T2")
		public void twoSingleTypeFields2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field2a : T2",
					"  var field2b : T2",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Fields T3 T3")
		public void twoSingleTypeFields3() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field3a : T3",
					"  var field3b : T3",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

		@Test
		@DisplayName("Fields T1 T1 T2")
		public void twoSingleTypeFieldsOneField1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number, T3 extends Double> {",
					"  var field1a : T1",
					"  var field1b : T1",
					"  var field2 : T2",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas)
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T1")
				.assertNoWarnings(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T2")
				.assertWarning(
					SarlPackage.eINSTANCE.getSarlEvent(),
					UNUSED_TYPE_PARAMETER,
					"Unused type parameter T3")
				.assertNoErrors();
		}

	}

	@DisplayName("Multiple behavior units")
	@Nested
	public class MultipleBehaviorUnits extends AbstractSarlTest {

		@Test
		@DisplayName("Different erasures")
		public void differentErasures1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number> {",
					"  var field1 : T1",
					"  var field2 : T2",
					"}",
					"agent X {",
					"  on E1<?, ?> { System.out.println(\"2\") }",
					"  on E1<String, ?> { System.out.println(\"3\") }",
					"  on E1<Double, ?> { System.out.println(\"4a\") }",
					"  on E1<Double, ?> { System.out.println(\"4b\") }",
					"  on E1<?, Double> { System.out.println(\"5\") }",
					"  on E1<String, Double> { System.out.println(\"6\") }",
					"  on E1 { System.out.println(\"1\") }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Different erasures with multiple events")
		public void differentErasures2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1, T2 extends Number> {",
					"  var field1 : T1",
					"  var field2 : T2",
					"}",
					"event E2<T3> {",
					"  var field3 : T3",
					"}",
					"event E3",
					"agent X {",
					"  on E1<?, ?> { System.out.println(\"2\") }",
					"  on E2<?> { System.out.println(\"7\") }",
					"  on E1<String, ?> { System.out.println(\"3\") }",
					"  on E1<Double, ?> { System.out.println(\"4a\") }",
					"  on E2<String> { System.out.println(\"8\") }",
					"  on E1<Double, ?> { System.out.println(\"4b\") }",
					"  on E1<?, Double> { System.out.println(\"5\") }",
					"  on E3 { System.out.println(\"9\") }",
					"  on E1<String, Double> { System.out.println(\"6\") }",
					"  on E1 { System.out.println(\"1\") }",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

		@Test
		@DisplayName("Too many type parameters 1")
		public void illegalGeneric1() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1",
					"agent X {",
					"  on E1<?> {}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					TYPE_ARGUMENT_ON_NON_GENERIC_TYPE,
					"The type E1 is not generic; it cannot be parameterized with arguments <? >");
		}

		@Test
		@DisplayName("Too many type parameters 2")
		public void illegalGeneric2() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1>",
					"agent X {",
					"  on E1<?, ?> {}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					INVALID_NUMBER_OF_TYPE_ARGUMENTS,
					"Incorrect number of arguments for type E1<T1>");
		}

		@Test
		@DisplayName("Not enough type parameters")
		public void rawGeneric() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1>",
					"agent X {",
					"  on E1 {}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					RAW_TYPE,
					"E1 is a raw type");
		}

		@Test
		@DisplayName("Invalid type parameter bound")
		public void invalidBound() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1<T1 extends Number> {",
					"  var f : T1",
					"}",
					"agent X {",
					"  on E1<String> {}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertError(
					SarlPackage.eINSTANCE.getSarlBehaviorUnit(),
					TYPE_BOUNDS_MISMATCH,
					"Bounds mismatch: The type arguments <String> are not a valid substitute for the bounded type parameters <T1 extends Number> of the super type E1");
		}

	}

}