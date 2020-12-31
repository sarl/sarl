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

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: @SuppressWarnings")
@Tag("core")
public class SuppressWarningsTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void noAnnotation() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"capacity C1 {",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlCapacity(),
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION);
	}

	@Test
	@Tag("sarlValidation")
	public void annotation_all() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"@SuppressWarnings(\"all\")",
				"capacity C1 {",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlCapacity(),
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION);
	}

	@Test
	@Tag("sarlValidation")
	public void annotation_correctId() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"@SuppressWarnings(\"" + IssueCodes.DISCOURAGED_CAPACITY_DEFINITION + "\")",
				"capacity C1 {",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlCapacity(),
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION);
	}

	@Test
	@Tag("sarlValidation")
	public void annotation_invalidId() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"@SuppressWarnings(\"x.y.z\")",
				"capacity C1 {",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlCapacity(),
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION);
	}

	@Test
	@Tag("sarlValidation")
	public void annotation_all_inlist() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"@SuppressWarnings(\"a\", \"b\", \"c\", \"all\")",
				"capacity C1 {",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlCapacity(),
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION);
	}

	@Test
	@Tag("sarlValidation")
	public void annotation_correctId_inlist() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"@SuppressWarnings(\"a\", \"b\", \"c\", \"" + IssueCodes.DISCOURAGED_CAPACITY_DEFINITION + "\")",
				"capacity C1 {",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoWarnings(
				SarlPackage.eINSTANCE.getSarlCapacity(),
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION);
	}

	@Test
	@Tag("sarlValidation")
	public void annotation_invalidId_inlist() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"@SuppressWarnings(\"a\", \"b\", \"c\", \"x.y.z\")",
				"capacity C1 {",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				SarlPackage.eINSTANCE.getSarlCapacity(),
				IssueCodes.DISCOURAGED_CAPACITY_DEFINITION);
	}


	@Test
	@Tag("sarlValidation")
	public void expression_noSuppression() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"agent A1 {",
				"	def mytst : void {",
				"     if (1==1) {",
				"     }",
				"   }",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XbasePackage.eINSTANCE.getXBinaryOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION);
	}

	@Test
	@Tag("sarlValidation")
	public void expression_suppression_01() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"agent A1 {",
				"   @SuppressWarnings(\"constant_condition\")",
				"	def mytst : void {",
				"     if (1==1) {",
				"     }",
				"   }",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoWarnings(
				XbasePackage.eINSTANCE.getXBinaryOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION);
	}

	@Test
	@Tag("sarlValidation")
	public void expression_suppression_02() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"package io.sarl.lang.tests.test",
				"@SuppressWarnings(\"constant_condition\")",
				"agent A1 {",
				"	def mytst : void {",
				"     if (1==1) {",
				"     }",
				"   }",
				"}"));
		validate(getValidationHelper(), getInjector(), mas).assertNoWarnings(
				XbasePackage.eINSTANCE.getXBinaryOperation(),
				org.eclipse.xtext.xbase.validation.IssueCodes.CONSTANT_BOOLEAN_CONDITION);
	}

}
