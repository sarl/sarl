/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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
package io.sarl.lang.tests.general.parsing.general;

import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifier;
import static io.sarl.tests.api.tools.TestAssertions.assertXExpression;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.common.base.Strings;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: type casting")
@Tag("core")
public class DataTypeParsingTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void intToDouble() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"	var internalTime : Double = 0",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXNumberLiteral(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to Double");
	}

	@Test
	@Tag("sarlParsing")
	public void doubleToDouble_1() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"agent A1 {",
				"	var internalTime : Double = 0.0",
				"}"
				));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlField attr = (SarlField) agent.getMembers().get(0);
		assertEquals("internalTime", attr.getName());
		assertTypeReferenceIdentifier(attr.getType(), "java.lang.Double");
		assertXExpression(attr.getInitialValue(), XNumberLiteral.class, "0.0");
	}

	@Test
	@Tag("sarlValidation")
	public void doubleToDouble_2() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"	var internalTime : Double = 0.",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlAgent(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"no viable alternative at input '}'");
	}

	@Test
	@Tag("sarlValidation")
	public void doubleToDouble_3() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"agent A1 {",
				"	var internalTime : Double = .0",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"no viable alternative at input '.'");
	}

	@Test
	@Tag("sarlParsing")
	public void doubleToDouble_4() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
				"agent A1 {",
				"	var internalTime : Double = 0d",
				"}"
				));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		SarlField attr = (SarlField) agent.getMembers().get(0);
		assertEquals("internalTime", attr.getName());
		assertTypeReferenceIdentifier(attr.getType(), "java.lang.Double");
		assertXExpression(attr.getInitialValue(), XNumberLiteral.class, "0d");
	}

}
