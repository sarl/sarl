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

import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

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
@DisplayName("Syntax: discouraged annotations")
@Tag("core")
public class DiscouragedAnnotationTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void defaultValue() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.DefaultValue",
				"agent A1 {",
				"	def fct(@DefaultValue(\"\") a : int) {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void defaultValueSource() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.DefaultValueSource",
				"agent A1 {",
				"	@DefaultValueSource",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void defaultValueUse() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.DefaultValueUse",
				"agent A1 {",
				"	@DefaultValueUse(\"\")",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void firedEvent() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.FiredEvent",
				"agent A1 {",
				"	@FiredEvent",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void importedCapacityFeature() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.ImportedCapacityFeature",
				"agent A1 {",
				"	@ImportedCapacityFeature(Object)",
				"	var field : int",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void perceptGuardEvaluator() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.PerceptGuardEvaluator",
				"agent A1 {",
				"	@PerceptGuardEvaluator",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void sarlSourceCode() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.SarlSourceCode",
				"agent A1 {",
				"	@SarlSourceCode",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void sarlSpecification() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.SarlSpecification",
				"@SarlSpecification",
				"agent A1 {",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void syntheticMember() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.SyntheticMember",
				"agent A1 {",
				"   @SyntheticMember",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void earlyExit_onFunction() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"agent A1 {",
				"   @EarlyExit",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void earlyExit_onAgent() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"@EarlyExit",
				"agent A1 {",
				"	def fct {",
				"	}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertWarning(
				XAnnotationsPackage.eINSTANCE.getXAnnotation(),
				IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				"Discouraged use of reserved annotation");
	}

	@Test
	@Tag("sarlValidation")
	public void earlyExit_onEvent() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.annotation.EarlyExit",
				"@EarlyExit",
				"event E1"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors(IssueCodes.USED_RESERVED_SARL_ANNOTATION);
	}

}
