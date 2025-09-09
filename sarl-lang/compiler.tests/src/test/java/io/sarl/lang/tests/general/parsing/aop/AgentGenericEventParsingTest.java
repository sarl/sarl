/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_MODIFIER;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version compiler.tests 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 */
@SuppressWarnings("all")
@DisplayName("Parsing: Agent with generic event")
@Tag("core")
@Tag("sarlParsing")
public class AgentGenericEventParsingTest extends AbstractSarlTest {
	
	@Test
	@DisplayName("True guard")
	public void trueGuard() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1, T2 extends Number>",
				"agent A1 {",
				"  on E1<String, Double>  [ true ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("False guard")
	public void falseGuard() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1, T2 extends Number>",
				"agent A1 {",
				"  on E1<String, Double> [ false ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("General guard")
	public void generalGuard() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1, T2 extends Number> { var i : T2 }",
				"agent A1 {",
				"  on E1<String, Double> [ occurrence.i == 1 ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("With wildcard 1")
	public void withWildcard1() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<?, Double> [ occurrence.a == 1 ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("With wildcard 2")
	public void withWildcard2() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1 extends Number, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<?, Double> [ occurrence.a == 1 ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("Raw notation")
	public void rawNotation() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1 [ occurrence.a == 1 ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("With two wildcards")
	public void withTwoWildcards() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<?, ?> [ occurrence.a == 1 ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("With upper bound")
	public void withUpperBound() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<? extends Number, Double> [ occurrence.a == 1 ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("With upper bound and raw #1")
	public void withUpperBoundAndRaw() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event E1<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  on E1<? extends Number, Double> [ occurrence.a == 1 ] {",
				"    System.^out.println(occurrence)",
				"  }",
				"  on E1 {",
				"    System.^out.println(occurrence)",
				"  }",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@DisplayName("With upper bound and raw #2")
	public void withUpperBoundAndRaw1() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"event GenericEvent<T1, T2 extends Number> { var a : T1; var b : T2 }",
				"agent A1 {",
				"  	on GenericEvent {",
				"		\"RAW\".println",
				"	}",
				"	on GenericEvent<?, ?> {",
				"		\"<?,?>\".println",
				"	}",
				"	on GenericEvent<Number, ?> {",
				"		\"<Number,?>\".println",
				"	}",
				"	on GenericEvent<? extends Number, ?> {",
				"		\"<? extends Number,?>\".println",
				"	}",
				"	on GenericEvent<String, ?> {",
				"		\"<String,?>/1\".println",
				"	}",
				"	on GenericEvent<String, ?> {",
				"		\"<String,?>/2\".println",
				"	}",
				"	on GenericEvent<Double, ?> {",
				"		\"<Double,?>\".println",
				"	}",
				"	on GenericEvent<?, Float> {",
				"		\"<?,Float>\".println",
				"	}",
				"	on GenericEvent<String, Float> {",
				"		\"<String,Float>\".println",
				"	}",
				"   def println(v : String) {}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

}
