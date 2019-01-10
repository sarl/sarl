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
package io.sarl.lang.tests.general.parsing.general;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	DiscouragedSystemCallTest.ConsoleTest.class,
	DiscouragedSystemCallTest.ErrCallTest.class,
	DiscouragedSystemCallTest.OutCallTest.class,
	DiscouragedSystemCallTest.SetErrCallTest.class,
	DiscouragedSystemCallTest.SetOutCallTest.class,
	DiscouragedSystemCallTest.OutputImplicitCallTest.class,
	DiscouragedSystemCallTest.InheritedChannelCallTest.class,
})
@SuppressWarnings("all")
public class DiscouragedSystemCallTest {

	public static class ConsoleTest extends AbstractSarlTest {

		@Test
		public void systemConsole_agent_action() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"	def test {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemConsole_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemConsole_behavior_action() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemConsole_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemConsole_behavior_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.SARLInjectorProvider;",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemConsole_skill_action() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemConsole_skill_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.console().readLine();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemConsole_agent_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
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
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemConsole_skill_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
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
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	public static class ErrCallTest extends AbstractSarlTest {

		@Test
		public void systemErr_agent_action() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"	def test {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_behavior_action() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_behavior_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_skill_action() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_skill_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_agent_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_skill_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		err.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemErr_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
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
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	public static class OutCallTest extends AbstractSarlTest {

		@Test
		public void systemOut_agent_action() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"	def test {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_behavior_action() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_behavior_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_skill_action() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_skill_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_agent_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_skill_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		out.println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemOut_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
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
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	public static class SetErrCallTest extends AbstractSarlTest {

		@Test
		public void systemSetErr_agent_action() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"	def test {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_behavior_action() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_behavior_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_skill_action() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_skill_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_agent_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_skill_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		setErr(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetErr_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
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
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	public static class SetOutCallTest extends AbstractSarlTest {

		@Test
		public void systemSetOut_agent_action() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"	def test {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_behavior_action() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_behavior_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_skill_action() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_skill_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_agent_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_skill_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		setOut(null)",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemSetOut_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
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
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	public static class OutputImplicitCallTest extends AbstractSarlTest {

		@Test
		public void println_agent_action() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"	def test {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void println_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void println_behavior_action() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"	def test {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void println_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void println_behavior_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void println_skill_action() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void println_skill_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		println(\"\")",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

	public static class InheritedChannelCallTest extends AbstractSarlTest {

		@Test
		public void systemInheritedChannel_agent_action() throws Exception {
			SarlScript mas = file(multilineString(
					"agent A1 {",
					"	def test {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_agent_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_behavior_action() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"	def test {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_behavior_behaviorUnit() throws Exception {
			SarlScript mas = file(multilineString(
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_behavior_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_skill_action() throws Exception {
			SarlScript mas = file(multilineString(
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_skill_constructor() throws Exception {
			SarlScript mas = file(multilineString(
					"import io.sarl.lang.core.Agent",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		System.inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXMemberFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_agent_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"agent A1 {",
					"	def test {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_agent_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"agent A1 {",
					"	on E1 {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_behavior_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"behavior B1 {",
					"	def test {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_behavior_behaviorUnit_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"event E1 { }",
					"behavior B1 {",
					"	on E1 {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_behavior_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"import io.sarl.lang.core.Agent",
					"behavior B1 {",
					"	new (a : Agent) {",
					"		super(a)",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_skill_action_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
					"import static java.lang.System.*",
					"capacity C1 { }",
					"skill S1 implements C1 {",
					"	def test {",
					"		inheritedChannel().isOpen();",
					"	}",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

		@Test
		public void systemInheritedChannel_skill_constructor_staticImport() throws Exception {
			SarlScript mas = file(multilineString(
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
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXFeatureCall(),
					IssueCodes.DISCOURAGED_REFERENCE,
					"Discouraged feature call");
		}

	}

}
