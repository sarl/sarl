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
import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.Action;
import io.sarl.lang.sarl.Agent;
import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.inject.Inject;

/**
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLMapExtensionsTest extends AbstractSarlTest {
	
	@Inject
	private ParseHelper<SarlScript> parser;
	
	@Inject
	private ValidationTestHelper validator;

	@Test
	public void operator_addMapPair_0() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	map += str -> num",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(4, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("str", attr2.getName());
		assertNull(attr2.getType());
		assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "a");
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("num", attr3.getName());
		assertNull(attr3.getType());
		assertXExpression(attr3.getInitialValue(), XNumberLiteral.class, "4");
		//
		Action action = (Action) agent.getFeatures().get(3);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "java.lang.Object");
	}

	@Test
	public void operator_addMapPair_1() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction1 : Object {",
				"	var p = str -> num",
				"	map += p",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(4, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("str", attr2.getName());
		assertNull(attr2.getType());
		assertXExpression(attr2.getInitialValue(), XStringLiteral.class, "a");
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("num", attr3.getName());
		assertNull(attr3.getType());
		assertXExpression(attr3.getInitialValue(), XNumberLiteral.class, "4");
		//
		Action action = (Action) agent.getFeatures().get(3);
		assertEquals("myaction1", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "java.lang.Object");
	}

	@Test
	public void operator_addMapMap_0() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Number>",
				"var map2 : Map<String, Integer>",
				"def myaction0 {",
				"	map1 += map2",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(3, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map1", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Number>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("map2", attr2.getName());
		assertTypeReferenceIdentifier(attr2.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr2.getInitialValue());
		//
		Action action = (Action) agent.getFeatures().get(2);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "void");
	}

	@Test
	public void operator_addMapMap_1() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Number>",
				"var map2 : Map<String, Integer>",
				"def myaction0 : Object {",
				"	map2 += map1",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from Map<String, Number> to Pair<? extends String, ? extends Integer>");
	}

	@Test
	public void operator_plusMapPair_0() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	r = map + (str -> num)",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(5, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("r", attr2.getName());
		assertTypeReferenceIdentifier(attr2.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr2.getInitialValue());
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("str", attr3.getName());
		assertNull(attr3.getType());
		assertXExpression(attr3.getInitialValue(), XStringLiteral.class, "a");
		//
		Attribute attr4 = (Attribute) agent.getFeatures().get(3);
		assertEquals("num", attr4.getName());
		assertNull(attr4.getType());
		assertXExpression(attr4.getInitialValue(), XNumberLiteral.class, "4");
		//
		Action action = (Action) agent.getFeatures().get(4);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "java.lang.Object");
	}

	@Test
	public void operator_plusMapPair_1() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	var p = str -> num",
				"	r = map + p",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(5, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("r", attr2.getName());
		assertTypeReferenceIdentifier(attr2.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr2.getInitialValue());
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("str", attr3.getName());
		assertNull(attr3.getType());
		assertXExpression(attr3.getInitialValue(), XStringLiteral.class, "a");
		//
		Attribute attr4 = (Attribute) agent.getFeatures().get(3);
		assertEquals("num", attr4.getName());
		assertNull(attr4.getType());
		assertXExpression(attr4.getInitialValue(), XNumberLiteral.class, "4");
		//
		Action action = (Action) agent.getFeatures().get(4);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "java.lang.Object");
	}

	@Test
	public void operator_plusMapMap_0() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Integer>",
				"var map2 : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"def myaction0 : Object {",
				"	r = map1 + map2",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(4, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map1", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("map2", attr2.getName());
		assertTypeReferenceIdentifier(attr2.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr2.getInitialValue());
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("r", attr3.getName());
		assertTypeReferenceIdentifier(attr3.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr3.getInitialValue());
		//
		Action action = (Action) agent.getFeatures().get(3);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "java.lang.Object");
	}

	@Test
	public void operator_plusMapMap_1() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Integer>",
				"var map2 : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"def myaction0 : Object {",
				"	r = map2 + map1",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(4, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map1", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("map2", attr2.getName());
		assertTypeReferenceIdentifier(attr2.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr2.getInitialValue());
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("r", attr3.getName());
		assertTypeReferenceIdentifier(attr3.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr3.getInitialValue());
		//
		Action action = (Action) agent.getFeatures().get(3);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
	}

	@Test
	public void operator_plusMapMap_2() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Integer>",
				"var map2 : Map<String, Number>",
				"var r : Map<String, Integer>",
				"def myaction0 : Object {",
				"	r = map1 + map2",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from Map<String, Number> to Pair<? extends String, ? extends Integer>");
	}

	@Test
	public void operator_plusMapMap_3() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Integer>",
				"var map2 : Map<String, Number>",
				"var r : Map<String, Number>",
				"def myaction0 : Object {",
				"	r = map2 + map1",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(4, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map1", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("map2", attr2.getName());
		assertTypeReferenceIdentifier(attr2.getType(), "java.util.Map<java.lang.String, java.lang.Number>");
		assertNull(attr2.getInitialValue());
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("r", attr3.getName());
		assertTypeReferenceIdentifier(attr3.getType(), "java.util.Map<java.lang.String, java.lang.Number>");
		assertNull(attr3.getInitialValue());
		//
		Action action = (Action) agent.getFeatures().get(3);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "java.lang.Object");
	}

	@Test
	public void operator_plusMapMap_4() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map1 : Map<String, Integer>",
				"var map2 : Map<String, Number>",
				"var r : Map<String, Integer>",
				"def myaction0 : Object {",
				"	r = map2 + map1",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXBinaryOperation(),
			IssueCodes.INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from String to Map<String, Integer>");
	}

	@Test
	public void operator_removeMapK_0() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	map -= str",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(5, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("r", attr2.getName());
		assertTypeReferenceIdentifier(attr2.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr2.getInitialValue());
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("str", attr3.getName());
		assertNull(attr3.getType());
		assertXExpression(attr3.getInitialValue(), XStringLiteral.class, "a");
		//
		Attribute attr4 = (Attribute) agent.getFeatures().get(3);
		assertEquals("num", attr4.getName());
		assertNull(attr4.getType());
		assertXExpression(attr4.getInitialValue(), XNumberLiteral.class, "4");
		//
		Action action = (Action) agent.getFeatures().get(4);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "java.lang.Object");
	}

	@Test
	public void operator_removeMapK_1() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 {",
				"	map -= num",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from int to Iterable<? super String>");
	}

	@Test
	public void operator_minusMapK_0() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	r = map - str",
				"}",
			"}"
		));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getElements().size());
		//
		assertEquals("test", mas.getName());
		//
		Agent agent = (Agent) mas.getElements().get(0);
		assertEquals("A1", agent.getName());
		assertTypeReferenceIdentifiers(agent.getSuperTypes());
		assertEquals(5, agent.getFeatures().size());
		//
		Attribute attr1 = (Attribute) agent.getFeatures().get(0);
		assertEquals("map", attr1.getName());
		assertTypeReferenceIdentifier(attr1.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr1.getInitialValue());
		//
		Attribute attr2 = (Attribute) agent.getFeatures().get(1);
		assertEquals("r", attr2.getName());
		assertTypeReferenceIdentifier(attr2.getType(), "java.util.Map<java.lang.String, java.lang.Integer>");
		assertNull(attr2.getInitialValue());
		//
		Attribute attr3 = (Attribute) agent.getFeatures().get(2);
		assertEquals("str", attr3.getName());
		assertNull(attr3.getType());
		assertXExpression(attr3.getInitialValue(), XStringLiteral.class, "a");
		//
		Attribute attr4 = (Attribute) agent.getFeatures().get(3);
		assertEquals("num", attr4.getName());
		assertNull(attr4.getType());
		assertXExpression(attr4.getInitialValue(), XNumberLiteral.class, "4");
		//
		Action action = (Action) agent.getFeatures().get(4);
		assertEquals("myaction0", action.getName());
		assertTypeReferenceIdentifiers(action.getFiredEvents());
		assertParameterNames(action.getParams());
		assertTypeReferenceIdentifier(action.getType(), "java.lang.Object");
	}

	@Test
	public void operator_minusMapK_1() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	r = str - map",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXBinaryOperation(),
			Diagnostic.LINKING_DIAGNOSTIC,
			"The method -(Map<String, Integer>) is undefined");
	}

	@Test
	public void operator_minusMapK_2() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	r = map - num",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from int to Map<? extends String, ? extends Integer>");
	}

	@Test
	public void operator_minusMapK_3() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"package test",
			"import java.util.Map",
			"agent A1 {",
				"var map : Map<String, Integer>",
				"var r : Map<String, Integer>",
				"var str = \"a\"",
				"var num = 4",
				"def myaction0 : Object {",
				"	r = num - map",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXBinaryOperation(),
			IssueCodes.INCOMPATIBLE_TYPES,
			"Type mismatch: cannot convert from int to Map<String, Integer>");
	}

}
