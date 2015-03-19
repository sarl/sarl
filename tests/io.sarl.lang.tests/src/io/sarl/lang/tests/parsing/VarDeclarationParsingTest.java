/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.parsing;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.common.base.Strings;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class VarDeclarationParsingTest extends AbstractSarlTest {
	@Inject
	private ParseHelper<SarlScript> parser;
	
	@Inject
	private ValidationTestHelper validator;

	@Test
	public void variableDeclaration_attributeScope_xtend() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import java.util.List",
			"agent A1 {",
				"var List<Integer> list",
				"var i = 45",
				"var double j = 45",
			"}"
		));
		this.validator.assertError(mas,
			SarlPackage.eINSTANCE.getAttribute(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"no viable alternative at input 'List'");
		this.validator.assertError(mas,
			SarlPackage.eINSTANCE.getAttribute(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"no viable alternative at input 'double'");
	}

	@Test
	public void variableDeclaration_localScope_xtend() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			Diagnostic.LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'i'");
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXAssignment(),
			Diagnostic.LINKING_DIAGNOSTIC,
			"The method k(int) is undefined");
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			Diagnostic.LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'k'");
	}

	@Test
	public void variableDeclaration_attributeScope() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import java.util.List",
			"agent A1 {",
				"var list : List<Integer>",
				"var i = 45",
				"var j : double = 45",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(3, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("list", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.List<java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("i", attr2.getName());
		assertNull(attr2.getType());
		assertXExpression(attr2.getInitialValue(), XNumberLiteral.class, "45");
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("j", attr3.getName());
		assertTypeReferenceIdentifier(attr3.getType(), "double");
		assertXExpression(attr3.getInitialValue(), XNumberLiteral.class, "45");
	}

	@Test
	public void variableDeclaration_localScope() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(1, agent.getFeatures().size());
		//
		Action action = (Action) agent.getFeatures().get(0);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getType(), "void");
		assertParameterNames(action.getParams());
	}

	@Test
	public void valueDeclaration_attributeScope_xtend() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import java.util.List",
			"agent A1 {",
				"val List<Integer> list",
				"val i = 45",
				"val double j = 45",
			"}"
		));
		this.validator.assertError(mas,
			SarlPackage.eINSTANCE.getAttribute(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"no viable alternative at input 'List'");
		this.validator.assertError(mas,
			SarlPackage.eINSTANCE.getAttribute(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"no viable alternative at input 'double'");
	}

	@Test
	public void valueDeclaration_localScope_xtend() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			Diagnostic.LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'i'");
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXAssignment(),
			Diagnostic.LINKING_DIAGNOSTIC,
			"The method k(int) is undefined");
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			Diagnostic.LINKING_DIAGNOSTIC,
			"Couldn't resolve reference to JvmIdentifiableElement 'k'");
	}

	@Test
	public void valueDeclaration_attributeScope() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import java.util.List",
			"agent A1 {",
				"val list : List<Integer> = null",
				"val i = 45",
				"val j : double = 45",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(3, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("list", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.List<java.lang.Integer>");
		assertXExpression(attr1.getInitialValue(), XNullLiteral.class, null);
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("i", attr2.getName());
		assertNull(attr2.getType());
		assertXExpression(attr2.getInitialValue(), XNumberLiteral.class, "45");
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("j", attr3.getName());
		assertTypeReferenceIdentifier(attr3.getType(), "double");
		assertXExpression(attr3.getInitialValue(), XNumberLiteral.class, "45");
	}

	@Test
	public void valueDeclaration_localScope() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"agent A1 {",
				"def myaction {",
					"val j = 45",
					"val k : double = 45",
					"System.out.println(j)",
					"System.out.println(k)",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(1, agent.getFeatures().size());
		//
		Action action = (Action) agent.getFeatures().get(0);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getType(), "void");
		assertParameterNames(action.getParams());
	}

	@Test
	public void forLoop_xtend() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"missing ')' at 'i'");
	}

	@Test
	public void forLoop_inferredType() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(2, agent.getFeatures().size());
		//
		Attribute attr = (Attribute) agent.getFeatures().get(0);
		assertEquals("list", attr.getName());
		assertTypeReferenceIdentifier(attr.getType(), "java.util.List<java.lang.Integer>");
		assertNull(attr.getInitialValue());
		//
		Action action = (Action) agent.getFeatures().get(1);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getType(), "void");
		assertParameterNames(action.getParams());
	}

	@Test
	public void forLoop_explicitType() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(2, agent.getFeatures().size());
		//
		Attribute attr = (Attribute) agent.getFeatures().get(0);
		assertEquals("list", attr.getName());
		assertTypeReferenceIdentifier(attr.getType(), "java.util.List<java.lang.Integer>");
		assertNull(attr.getInitialValue());
		//
		Action action = (Action) agent.getFeatures().get(1);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getType(), "void");
		assertParameterNames(action.getParams());
	}

	@Test
	public void catch_xtend() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"missing ':' at 'e'");
	}

	@Test
	public void catch_oneType() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(1, agent.getFeatures().size());
		//
		Action action = (Action) agent.getFeatures().get(0);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getType(), "void");
		assertParameterNames(action.getParams());
	}

	@Test
	public void multicatch_xtend() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"missing ':' at 'e'");
	}

	@Test
	public void multicatch_oneType() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(1, agent.getFeatures().size());
		//
		Action action = (Action) agent.getFeatures().get(0);
		assertEquals("myaction", action.getName());
		assertTypeReferenceIdentifier(action.getType(), "void");
		assertParameterNames(action.getParams());
	}

	@Test
	public void closure_xtend() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXClosure(),
			Diagnostic.SYNTAX_DIAGNOSTIC,
			"mismatched input ',' expecting ']'");
	}

	@Test
	public void closure_twoParams() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getName()));
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(2, agent.getFeatures().size());
		//
		Action action1 = (Action) agent.getFeatures().get(0);
		assertEquals("mycall", action1.getName());
		assertTypeReferenceIdentifier(action1.getType(), "float");
		assertParameterNames(action1.getParams(), "a", "f");
		assertParameterTypes(action1.getParams(), "int", "(java.lang.Float, java.lang.Integer)=>float");
		assertParameterDefaultValues(action1.getParams(), null, null);
		//
		Action action2 = (Action) agent.getFeatures().get(1);
		assertEquals("myaction", action2.getName());
		assertTypeReferenceIdentifier(action2.getType(), "void");
		assertParameterNames(action2.getParams());
	}
	
}
