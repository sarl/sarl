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
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Syntax: forbidden calls")
@Tag("core")
public class ForbiddenCallTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void systemExit_agent_action() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"agent A1 {",
				"def test {",
					"System.exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_agent_behaviorUnit() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"event E1 { }",
			"agent A1 {",
				"on E1 {",
					"System.exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_action() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"behavior B1 {",
				"def test {",
					"System.exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_behaviorUnit() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"event E1 { }",
			"behavior B1 {",
				"on E1 {",
					"System.exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_constructor() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"event E1 { }",
			"behavior B1 {",
				"new (a : Agent) {",
					"super(a)",
					"System.exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_skill_action() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"capacity C1 { }",
			"skill S1 implements C1 {",
				"def test {",
					"System.exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_skill_constructor() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"capacity C1 { }",
			"event E1 { }",
			"skill S1 implements C1 {",
				"new (a : Agent) {",
					"super(a)",
					"System.exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_agent_action_staticImport() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static java.lang.System.*",
			"agent A1 {",
				"def test {",
					"exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_agent_behaviorUnit_staticImport() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static java.lang.System.*",
			"event E1 { }",
			"agent A1 {",
				"on E1 {",
					"exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_action_staticImport() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static java.lang.System.*",
			"behavior B1 {",
				"def test {",
					"exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_behaviorUnit_staticImport() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static java.lang.System.*",
			"event E1 { }",
			"behavior B1 {",
				"on E1 {",
					"exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_constructor_staticImport() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import io.sarl.lang.core.Agent",
			"import static java.lang.System.*",
			"event E1 { }",
			"behavior B1 {",
				"new (a : Agent) {",
					"super(a)",
					"exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_skill_action_staticImport() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static java.lang.System.*",
			"capacity C1 { }",
			"skill S1 implements C1 {",
				"def test {",
					"exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_skill_constructor_staticImport() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import io.sarl.lang.core.Agent",
			"import static java.lang.System.*",
			"capacity C1 { }",
			"event E1 { }",
			"skill S1 implements C1 {",
				"new (a : Agent) {",
					"super(a)",
					"exit(0)",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_agent_action_extension() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static extension java.lang.System.*",
			"agent A1 {",
				"def test {",
					"0.exit",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_agent_behaviorUnit_extension() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static extension java.lang.System.*",
			"event E1 { }",
			"agent A1 {",
				"on E1 {",
					"0.exit",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_action_extension() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static extension java.lang.System.*",
			"behavior B1 {",
				"def test {",
					"0.exit",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_behaviorUnit_extension() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static extension java.lang.System.*",
			"event E1 { }",
			"behavior B1 {",
				"on E1 {",
					"0.exit",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_behavior_constructor_extension() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import io.sarl.lang.core.Agent",
			"import static extension java.lang.System.*",
			"event E1 { }",
			"behavior B1 {",
				"new (a : Agent) {",
					"super(a)",
					"0.exit",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_skill_action_extension() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import static extension java.lang.System.*",
			"capacity C1 { }",
			"skill S1 implements C1 {",
				"def test {",
					"0.exit",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

	@Test
	@Tag("sarlValidation")
	public void systemExit_skill_constructor_extension() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
			"import io.sarl.lang.core.Agent",
			"import static extension java.lang.System.*",
			"capacity C1 { }",
			"event E1 { }",
			"skill S1 implements C1 {",
				"new (a : Agent) {",
					"super(a)",
					"0.exit",
				"}",
			"}"
		));
		validate(getValidationHelper(), getInjector(), mas).assertError(
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden feature call: java.lang.System.exit(int)");
	}

}
