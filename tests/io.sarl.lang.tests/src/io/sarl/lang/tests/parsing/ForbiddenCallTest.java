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

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;

import com.google.inject.Inject;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(XtextRunner.class)
@InjectWith(SARLInjectorProvider.class)
@SuppressWarnings("all")
public class ForbiddenCallTest extends AbstractSarlTest {

	@Inject
	private ParseHelper<SarlScript> parser;

	@Inject
	private ValidationTestHelper validator;

	@Test
	public void systemExit_agent_action() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"agent A1 {",
				"def test {",
					"System.exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_agent_behaviorUnit() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"event E1 { }",
			"agent A1 {",
				"on E1 {",
					"System.exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_action() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"behavior B1 {",
				"def test {",
					"System.exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_behaviorUnit() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"event E1 { }",
			"behavior B1 {",
				"on E1 {",
					"System.exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_constructor() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"event E1 { }",
			"behavior B1 {",
				"new (a : Agent) {",
					"super(a)",
					"System.exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_skill_action() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"capacity C1 { }",
			"skill S1 implements C1 {",
				"def test {",
					"System.exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_skill_constructor() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"capacity C1 { }",
			"event E1 { }",
			"skill S1 implements C1 {",
				"new (a : Agent) {",
					"super(a)",
					"System.exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_agent_action_staticImport() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static java.lang.System.*",
			"agent A1 {",
				"def test {",
					"exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_agent_behaviorUnit_staticImport() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static java.lang.System.*",
			"event E1 { }",
			"agent A1 {",
				"on E1 {",
					"exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_action_staticImport() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static java.lang.System.*",
			"behavior B1 {",
				"def test {",
					"exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_behaviorUnit_staticImport() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static java.lang.System.*",
			"event E1 { }",
			"behavior B1 {",
				"on E1 {",
					"exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_constructor_staticImport() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_skill_action_staticImport() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static java.lang.System.*",
			"capacity C1 { }",
			"skill S1 implements C1 {",
				"def test {",
					"exit(0)",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_skill_constructor_staticImport() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_agent_action_extension() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static extension java.lang.System.*",
			"agent A1 {",
				"def test {",
					"0.exit",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_agent_behaviorUnit_extension() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static extension java.lang.System.*",
			"event E1 { }",
			"agent A1 {",
				"on E1 {",
					"0.exit",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_action_extension() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static extension java.lang.System.*",
			"behavior B1 {",
				"def test {",
					"0.exit",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_behaviorUnit_extension() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static extension java.lang.System.*",
			"event E1 { }",
			"behavior B1 {",
				"on E1 {",
					"0.exit",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_behavior_constructor_extension() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_skill_action_extension() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
			"import static extension java.lang.System.*",
			"capacity C1 { }",
			"skill S1 implements C1 {",
				"def test {",
					"0.exit",
				"}",
			"}"
		));
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

	@Test
	public void systemExit_skill_constructor_extension() throws Exception {
		SarlScript mas = this.parser.parse(multilineString(
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
		this.validator.assertError(mas,
			XbasePackage.eINSTANCE.getXMemberFeatureCall(),
			IssueCodes.FORBIDDEN_REFERENCE,
			"Forbidden call to the exit function. The killing feature of the agent must be used");
	}

}
