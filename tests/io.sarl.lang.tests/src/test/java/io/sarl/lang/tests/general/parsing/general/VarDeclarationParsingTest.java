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

import static io.sarl.tests.api.tools.TestAssertions.assertParameterDefaultValues;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterNames;
import static io.sarl.tests.api.tools.TestAssertions.assertParameterTypes;
import static io.sarl.tests.api.tools.TestAssertions.assertTypeReferenceIdentifier;
import static io.sarl.tests.api.tools.TestAssertions.assertXExpression;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.common.base.Strings;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: var")
@Tag("core")
public class VarDeclarationParsingTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void variableDeclaration_SarlFieldScope_xtend() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import java.util.List",
			"agent A1 {",
				"var List<Integer> list",
				"var i = 45",
				"var double j = 45",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas)
			.assertError(
				SarlPackage.eINSTANCE.getSarlAgent(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"mismatched input '<' expecting '}");
	}

	@Test
	@Tag("sarlValidation")
	public void variableDeclaration_localScope_xtend() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"agent A1 {",
				"def myaction {",
					"var int i",
					"var j = 45",
					"var double k = 45",
					"System.out.println(i)",
					"System.out.println(j)",
					"System.out.println(k)",
				"}",
			"}"
		));
		Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				Diagnostic.LINKING_DIAGNOSTIC,
				"The method or field i is undefined");
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				Diagnostic.LINKING_DIAGNOSTIC,
				"The method or field k is undefined");
	}

	@Test
	@Tag("sarlParsing")
	public void variableDeclaration_SarlFieldScope() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"import java.util.List",
			"agent A1 {",
				"var list : List<Integer>",
				"var i = 45",
				"var j : double = 45",
			"}"
		));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(3, agent.getMembers().size());
		//
		SarlField attr1 = (SarlField) agent.getMembers().get(0);
		assertEquals("list", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.List<java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		SarlField attr2 = (SarlField) agent.getMembers().get(1);
		assertEquals("i", attr2.getName());
		assertNull(attr2.getType());
		assertXExpression(attr2.getInitialValue(), XNumberLiteral.class, "45");
		//
		SarlField attr3 = (SarlField) agent.getMembers().get(2);
		assertEquals("j", attr3.getName());
		assertTypeReferenceIdentifier(attr3.getType(), "double");
		assertXExpression(attr3.getInitialValue(), XNumberLiteral.class, "45");
	}

	@Test
	@Tag("sarlParsing")
	public void variableDeclaration_localScope() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"import java.util.List",
			"agent A1 {",
				"def myaction {",
					"var i : List<Integer>",
					"var j = 45",
					"var k : double = 45",
					"System.out.println(i)",
					"System.out.println(j)",
					"System.out.println(k)",
				"}",
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
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
		assertParameterNames(action.getParameters());
	}

	@Test
	@Tag("sarlValidation")
	public void valueDeclaration_SarlFieldScope_xtend() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import java.util.List",
			"agent A1 {",
				"val List<Integer> list",
				"val i = 45",
				"val double j = 45",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas)
			.assertError(
				SarlPackage.eINSTANCE.getSarlAgent(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"mismatched input '<' expecting '}");
	}

	@Test
	@Tag("sarlValidation")
	public void valueDeclaration_localScope_xtend() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"agent A1 {",
				"def myaction {",
					"val int i",
					"val j = 45",
					"val double k = 45",
					"System.out.println(i)",
					"System.out.println(j)",
					"System.out.println(k)",
				"}",
			"}"
		));
		Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				Diagnostic.LINKING_DIAGNOSTIC,
				"The method or field i is undefined");
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				Diagnostic.LINKING_DIAGNOSTIC,
				"The method or field k is undefined");
	}

	@Test
	@Tag("sarlParsing")
	public void valueDeclaration_SarlFieldScope() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"import java.util.List",
			"agent A1 {",
				"val list : List<Integer> = null",
				"val i = 45",
				"val j : double = 45",
			"}"
		));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(3, agent.getMembers().size());
		//
		SarlField attr1 = (SarlField) agent.getMembers().get(0);
		assertEquals("list", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.List<java.lang.Integer>");
		assertXExpression(attr1.getInitialValue(), XNullLiteral.class, null);
		//
		SarlField attr2 = (SarlField) agent.getMembers().get(1);
		assertEquals("i", attr2.getName());
		assertNull(attr2.getType());
		assertXExpression(attr2.getInitialValue(), XNumberLiteral.class, "45");
		//
		SarlField attr3 = (SarlField) agent.getMembers().get(2);
		assertEquals("j", attr3.getName());
		assertTypeReferenceIdentifier(attr3.getType(), "double");
		assertXExpression(attr3.getInitialValue(), XNumberLiteral.class, "45");
	}

	@Test
	@Tag("sarlParsing")
	public void valueDeclaration_localScope() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"agent A1 {",
				"def myaction {",
					"val j = 45",
					"val k : double = 45",
					"System.out.println(j)",
					"System.out.println(k)",
				"}",
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
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
		assertParameterNames(action.getParameters());
	}

	@Test
	@Tag("sarlValidation")
	public void forLoop_xtend() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import java.util.List",
			"agent A1 {",
				"var list : List<Integer>",
				"def myaction {",
					"for( Number i : list) {",
						"System.out.println(i)",
					"}",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXFeatureCall(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"missing ';' at 'i'");
	}

	@Test
	@Tag("sarlParsing")
	public void forLoop_inferredType() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"import java.util.List",
			"agent A1 {",
				"var list : List<Integer>",
				"def myaction {",
					"for( i : list) {",
						"System.out.println(i)",
					"}",
				"}",
			"}"
		));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(2, agent.getMembers().size());
		//
		SarlField attr = (SarlField) agent.getMembers().get(0);
		assertEquals("list", attr.getName());
		assertTypeReferenceIdentifier(attr.getType(), "java.util.List<java.lang.Integer>");
		assertNull(attr.getInitialValue());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(1);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
		assertParameterNames(action.getParameters());
	}

	@Test
	@Tag("sarlParsing")
	public void forLoop_explicitType() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"import java.util.List",
			"agent A1 {",
				"var list : List<Integer>",
				"def myaction {",
					"for( i as Number : list) {",
						"System.out.println(i)",
					"}",
				"}",
			"}"
		));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(2, agent.getMembers().size());
		//
		SarlField attr = (SarlField) agent.getMembers().get(0);
		assertEquals("list", attr.getName());
		assertTypeReferenceIdentifier(attr.getType(), "java.util.List<java.lang.Integer>");
		assertNull(attr.getInitialValue());
		//
		SarlAction action = (SarlAction) agent.getMembers().get(1);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
		assertParameterNames(action.getParameters());
	}

	@Test
	@Tag("sarlValidation")
	public void catch_xtend() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"agent A1 {",
				"def myaction {",
					"try {",
						"System.out.println(\"G\")",
					"}",
					"catch(Throwable e) {",
						"System.out.println(e)",
					"}",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"missing ':' at 'e'");
	}

	@Test
	@Tag("sarlParsing")
	public void catch_oneType() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"agent A1 {",
				"def myaction {",
					"try {",
						"System.out.println(\"G\")",
					"}",
					"catch(e : Throwable) {",
						"System.out.println(e)",
					"}",
				"}",
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
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
		assertParameterNames(action.getParameters());
	}

	@Test
	@Tag("sarlValidation")
	public void multicatch_xtend() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"agent A1 {",
				"def myaction {",
					"try {",
						"System.out.println(\"G\")",
					"}",
					"catch(Exception e) {",
						"System.out.println(e)",
					"}",
					"catch(Throwable e) {",
						"System.out.println(e)",
					"}",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"missing ':' at 'e'");
	}

	@Test
	@Tag("sarlParsing")
	public void multicatch_oneType() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"agent A1 {",
				"def myaction {",
					"try {",
						"System.out.println(\"G\")",
					"}",
					"catch(e : Exception) {",
						"System.out.println(e)",
					"}",
					"catch(e : Throwable) {",
						"System.out.println(e)",
					"}",
				"}",
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
		SarlAction action = (SarlAction) agent.getMembers().get(0);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getReturnType(), "void");
		assertParameterNames(action.getParameters());
	}

	@Test
	@Tag("sarlValidation")
	public void closure_xtend() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"agent A1 {",
				"def mycall(a : int, f : (Number,Number) => int) {",
					"return a + f.apply",
				"}",
				"def myaction {",
					"mycall(4) [ Float a, Integer b |",
					"	2 * a.floatValue + b.intValue",
					"]",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXClosure(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ']'");
	}

	@Test
	@Tag("sarlParsing")
	public void closure_twoParams() throws Exception {
		SarlScript mas = file(getParseHelper(), getValidationHelper(), multilineString(
			"agent A1 {",
				"def mycall(a : int, f : (Float,Integer) => float) : float {",
					"return a + f.apply(5.45f, 6)",
				"}",
				"def myaction : void {",
					"mycall(4) [ a : Float, b : Integer |",
					"	2f * a.floatValue + b.intValue",
					"]",
				"}",
			"}"
		));
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(2, agent.getMembers().size());
		//
		SarlAction action1 = (SarlAction) agent.getMembers().get(0);
		assertEquals("mycall", action1.getName());
		assertTypeReferenceIdentifier(action1.getReturnType(), "float");
		assertParameterNames(action1.getParameters(), "a", "f");
		assertParameterTypes(action1.getParameters(), "int", "(java.lang.Float, java.lang.Integer)=>float");
		assertParameterDefaultValues(action1.getParameters(), null, null);
		//
		SarlAction action2 = (SarlAction) agent.getMembers().get(1);
		assertEquals("myaction", action2.getName());
		assertTypeReferenceIdentifier(action2.getReturnType(), "void");
		assertParameterNames(action2.getParameters());
	}

}
