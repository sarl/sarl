/*
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.lang.tests.parsing.oop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.eclipse.xtend.core.validation.IssueCodes;
import org.junit.Test;

import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
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
public class AnnotationTypeParsingTest extends AbstractSarlTest {

	@Test
	public void varUsage() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"annotation A1 {",
				"	var field1 : String",
				"}"
				), false);
		validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				IssueCodes.INVALID_MODIFIER,
				49, 3,
				"Illegal modifier for the field field1; only public, static, final & val are permitted");
	}

	@Test
	public void valUsage_0() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"annotation A1 {",
				"	val field1 : String",
				"}"
				), false);
		validate(mas).assertNoIssues();
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertEquals("io.sarl.lang.tests.test", mas.getPackage());
		//
		SarlAnnotationType annotation = (SarlAnnotationType) mas.getXtendTypes().get(0);
		assertEquals("A1", annotation.getName());
		assertEquals(1, annotation.getMembers().size());
		//
		SarlField field = (SarlField) annotation.getMembers().get(0);
		assertEquals("field1", field.getName());
		assertTypeReferenceIdentifier(field.getType(), "java.lang.String");
		assertContains(field.getModifiers(), "val");
	}

	@Test
	public void valUsage_1() throws Exception {
		SarlScript mas = file(multilineString(
				"package io.sarl.lang.tests.test",
				"annotation A1 {",
				"	val field1 = \"a\"",
				"}"
				), false);
		validate(mas).assertNoIssues();
		assertEquals(1, mas.getXtendTypes().size());
		//
		assertEquals("io.sarl.lang.tests.test", mas.getPackage());
		//
		SarlAnnotationType annotation = (SarlAnnotationType) mas.getXtendTypes().get(0);
		assertEquals("A1", annotation.getName());
		assertEquals(1, annotation.getMembers().size());
		//
		SarlField field = (SarlField) annotation.getMembers().get(0);
		assertEquals("field1", field.getName());
		assertNull(field.getType());
		assertContains(field.getModifiers(), "val");
	}

}
