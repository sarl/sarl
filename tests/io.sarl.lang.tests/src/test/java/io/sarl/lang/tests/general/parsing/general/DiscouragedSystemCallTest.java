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
import org.junit.jupiter.api.Nested;
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
@DisplayName("Syntax: discouraged system calls")
@Tag("core")
public class DiscouragedSystemCallTest {

	@Nested
	public class ConsoleTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in agent action")
		public void systemConsole_agent_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"	def test {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in agent behavior unit")
		public void systemConsole_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in behavior action")
		public void systemConsole_behavior_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in behavior behavior unit")
		public void systemConsole_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in behavior constructor")
		public void systemConsole_behavior_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.SARLInjectorProvider;",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in skill action")
		public void systemConsole_skill_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in skill constructor")
		public void systemConsole_skill_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in agent static import")
		public void systemConsole_agent_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"import static java.lang.System.*",
					"import static java.lang.System.*",
					"import static java.lang.System.*",
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.console in skill static import")
		public void systemConsole_skill_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"import io.sarl.tests.api.AbstractSarlTest",
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		console().readLine();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	@Nested
	public class ErrCallTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in agent static import")
		public void systemErr_agent_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"	def test {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in agent behavior unit")
		public void systemErr_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in agent behavior action")
		public void systemErr_behavior_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in behavior behavior unit")
		public void systemErr_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in behavior constructor")
		public void systemErr_behavior_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in skill action")
		public void systemErr_skill_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in skill constructor")
		public void systemErr_skill_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in agent static import and action")
		public void systemErr_agent_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in agent static import and behavior unit")
		public void systemErr_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in behavior static import and action")
		public void systemErr_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in behavior static import and behavior unit")
		public void systemErr_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		@DisplayName("Syntax: discouraged system.err in behavior static import and constructor")
		public void systemErr_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemErr_skill_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemErr_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	@Nested
	public class OutCallTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void systemOut_agent_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"	def test {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_behavior_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_behavior_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_skill_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_skill_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_agent_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlParsing")
		public void systemOut_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_skill_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemOut_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	@Nested
	public class SetErrCallTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_agent_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"	def test {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_behavior_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_behavior_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_skill_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_skill_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_agent_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_skill_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetErr_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	@Nested
	public class SetOutCallTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_agent_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"	def test {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_behavior_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_behavior_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_skill_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_skill_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_agent_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_skill_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemSetOut_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	@Nested
	public class OutputImplicitCallTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void println_agent_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"	def test {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void println_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void println_behavior_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def test {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void println_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void println_behavior_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void println_skill_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void println_skill_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	@Nested
	public class InheritedChannelCallTest extends AbstractSarlTest {

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_agent_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"agent A1 {",
					"	def test {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_behavior_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_behavior_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_skill_action() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_skill_constructor() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_agent_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_skill_action_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		@Tag("sarlValidation")
		public void systemInheritedChannel_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import io.sarl.lang.core.Agent",
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

}
