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
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;

import com.google.common.base.Strings;
import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class DataTypeParsingTest extends AbstractSarlTest {

	@Inject
	private ParseHelper<XtendFile> parser;

	@Inject
	private ValidationTestHelper validator;

	@Test
	public void intToDouble() throws Exception {
		XtendFile mas = this.parser.parse(multilineString(
				"agent A1 {",
				"	var internalTime : Double = 0",
				"}"
				));
		this.validator.assertError(mas,
				XbasePackage.eINSTANCE.getXNumberLiteral(),
				IssueCodes.INCOMPATIBLE_TYPES,
				"cannot convert from int to Double");
	}

	@Test
	public void doubleToDouble_1() throws Exception {
		XtendFile mas = this.parser.parse(multilineString(
				"agent A1 {",
				"	var internalTime : Double = 0.0",
				"}"
				));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		XtendField attr = (XtendField) agent.getMembers().get(0);
		assertEquals("internalTime", attr.getName());
		assertTypeReferenceIdentifier(attr.getType(), "java.lang.Double");
		assertXExpression(attr.getInitialValue(), XNumberLiteral.class, "0.0");
	}

	@Test
	public void doubleToDouble_2() throws Exception {
		XtendFile mas = this.parser.parse(multilineString(
				"agent A1 {",
				"	var internalTime : Double = 0.",
				"}"
				));
		this.validator.assertError(mas,
				SarlPackage.eINSTANCE.getSarlAgent(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"extraneous input '.' expecting '}'");
	}

	@Test
	public void doubleToDouble_3() throws Exception {
		XtendFile mas = this.parser.parse(multilineString(
				"agent A1 {",
				"	var internalTime : Double = .0",
				"}"
				));
		this.validator.assertError(mas,
				XtendPackage.eINSTANCE.getXtendField(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"no viable alternative at input '.'");
	}

	@Test
	public void doubleToDouble_4() throws Exception {
		XtendFile mas = this.parser.parse(multilineString(
				"agent A1 {",
				"	var internalTime : Double = 0d",
				"}"
				));
		this.validator.assertNoErrors(mas);
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertTrue(Strings.isNullOrEmpty(mas.getPackage()));
		//
		SarlAgent agent = (SarlAgent) mas.getXtendTypes().get(0);
		assertEquals("A1", agent.getName());
		assertNull(agent.getExtends());
		assertEquals(1, agent.getMembers().size());
		//
		XtendField attr = (XtendField) agent.getMembers().get(0);
		assertEquals("internalTime", attr.getName());
		assertTypeReferenceIdentifier(attr.getType(), "java.lang.Double");
		assertXExpression(attr.getInitialValue(), XNumberLiteral.class, "0d");
	}

}
