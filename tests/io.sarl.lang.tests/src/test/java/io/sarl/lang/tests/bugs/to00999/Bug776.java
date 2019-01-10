/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.tests.bugs.to00999;

import static org.junit.Assert.*;

import java.util.ArrayList;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.core.dom.Type;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.validation.UIStrings;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: Invalid visibility of inherited constructors.
 *
 * <p>https://github.com/sarl/sarl/issues/741
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://bugs.eclipse.org/bugs/show_bug.cgi?id=443131"
 */
@SuppressWarnings("all")
public class Bug776 extends AbstractSarlTest {

	@Inject
	private UIStrings strings;
	
	@Inject
	private CommonTypeComputationServices services;

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug776",
				"class X {",
				"}",
				""));
		final Validator validator = validate(mas);
		validator.assertNoErrors();
		JvmTypeReference typeRef = services.getTypeReferences().getTypeForName(ArrayList.class, mas);
		assertFalse(typeRef.getType().eIsProxy());
		assertEquals("ArrayList<E>", this.strings.referenceToString(typeRef, "Object"));
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug776",
				"class X {",
				"}",
				""));
		final Validator validator = validate(mas);
		validator.assertNoErrors();
		JvmTypeReference typeRef = services.getTypeReferences().getTypeForName("io.sarl.lang.tests.bug776.X", mas);
		assertFalse(typeRef.getType().eIsProxy());
		assertEquals("X", this.strings.referenceToString(typeRef, "Object"));
	}

	@Test
	public void parsing_03() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug776",
				"class X {",
				"}",
				"class Y {",
				"}",
				""));
		final Validator validator = validate(mas);
		validator.assertNoErrors();
		EObject secondType = mas.getXtendTypes().get(1);
		JvmTypeReference typeRef = services.getTypeReferences().getTypeForName("io.sarl.lang.tests.bug776.X", secondType);
		assertFalse(typeRef.getType().eIsProxy());
		assertEquals("X", this.strings.referenceToString(typeRef, "Object"));
	}

	@Test
	public void parsing_04() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug776",
				"class X {",
				"}",
				"class Y extends Z {",
				" static class YX { }",
				"}",
				"class Z {",
				"}",
				""));
		final Validator validator = validate(mas);
		validator.assertNoErrors();
		JvmTypeReference typeRef = services.getTypeReferences().getTypeForName("io.sarl.lang.tests.bug776.Y$YX", mas);
		assertFalse(typeRef.getType().eIsProxy());
		assertEquals("YX", this.strings.referenceToString(typeRef, "Object"));
	}

	@Test
	public void parsing_05() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug776",
				"class X {",
				"}",
				"class Y extends Z {",
				" static class YX { }",
				"}",
				"class Z {",
				"}",
				""));
		final Validator validator = validate(mas);
		validator.assertNoErrors();
		JvmTypeReference typeRef = services.getTypeReferences().getTypeForName("io.sarl.lang.tests.bug776.A", mas);
		assertFalse(typeRef.eIsProxy());
		assertNull(typeRef.getType());
		assertEquals("A", this.strings.referenceToString(typeRef, "Object"));
	}

	@Test
	public void parsing_06() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug776",
				"import io.sarl.lang.core.Agent",
				"class X {",
				"}",
				"class Y extends Z {",
				" static class YX {",
				"    var a : Agent",
				" }",
				"}",
				"class Z {",
				"}",
				""));
		final Validator validator = validate(mas);
		validator.assertNoErrors();
		XtendClass container = ((XtendClass) mas.getXtendTypes().get(1).getMembers().get(0));
		JvmTypeReference typeRef = ((SarlField) container.getMembers().get(0)).getType();
		assertFalse(typeRef.getType().eIsProxy());
		assertEquals("Agent", this.strings.referenceToString(typeRef, "Object"));
	}

	@Test
	public void parsing_07() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.bug776",
				"import io.sarl.lang.core.Agent",
				"class X {",
				"}",
				"class Y extends Z {",
				" static class YX<T> {",
				"    var a : T",
				" }",
				"}",
				"class Z {",
				"}",
				""));
		final Validator validator = validate(mas);
		validator.assertNoErrors();
		XtendClass container = ((XtendClass) mas.getXtendTypes().get(1).getMembers().get(0));
		JvmTypeReference typeRef = ((SarlField) container.getMembers().get(0)).getType();
		assertFalse(typeRef.getType().eIsProxy());
		assertEquals("T", this.strings.referenceToString(typeRef, "Object"));
	}

	@Test
	public void parsing_08() throws Exception {
		ParseHelper<SarlScript> parser = getParseHelper();
		SarlScript mas1 = parser.parse(multilineString(
				"package io.sarl.lang.tests.bug776.b",
				"class Toto{}"));
		SarlScript mas2 = parser.parse(multilineString(
				"package io.sarl.lang.tests.bug776",
				"import io.sarl.lang.tests.bug776.b.Toto",
				"class X {",
				"}",
				"class Y extends Z {",
				" static class YX {",
				"    var a : Toto",
				" }",
				"}",
				"class Z {",
				"}",
				""),
				mas1.eResource().getResourceSet());
		XtendClass container = ((XtendClass) mas2.getXtendTypes().get(1).getMembers().get(0));
		JvmTypeReference typeRef = ((SarlField) container.getMembers().get(0)).getType();
		assertEquals("Toto", this.strings.referenceToString(typeRef, "Object"));
	}

}

