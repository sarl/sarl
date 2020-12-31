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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid warning "Possible invalid usage of ‘occurrence’".
 *
 * <p>https://github.com/sarl/sarl/issues/1013
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1013"
 */
@DisplayName("Bug #1013")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlParsing")
public class Bug1013Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1013",
			"import io.sarl.core.Logging",
			"import java.util.Map",
			"event E_SetupPlayer {",
			"	var agentName : String",
			"	var configDir : String",
			"	var configFile : String",
			"	var teamRoles : Map<String, Integer>",
			"	var centralBuilder : String",
			"	new(agentName : String, configDir : String, configFile : String, teamRoles : Map<String, Integer>, centralBuilder : String) {",
			"		this.agentName = agentName",
			"		this.configDir = configDir",
			"		this.configFile = configFile",
			"		this.teamRoles = teamRoles",
			"		this.centralBuilder = centralBuilder",
			"	}",
			"}",
			"agent Bug1013Agent {",
			"   uses Logging",
			"   var agentName : String",
			"   on E_SetupPlayer [occurrence.agentName == this.agentName] {",
			"		val teamRoles = occurrence.teamRoles",
			"       info(teamRoles)",
			"   }",
			"}");

	@Test
	@DisplayName("occurrence field to local variable")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE,
				"Possible invalid usage of 'occurrence'",
				"be copied within a local variable")
			.assertNoIssues();
	}

	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug1013",
			"import io.sarl.core.Logging",
			"import java.util.Map",
			"event E_SetupPlayer {",
			"	var agentName : String",
			"	var configDir : String",
			"	var configFile : String",
			"	var teamRoles : Map<String, Integer>",
			"	var centralBuilder : String",
			"	new(agentName : String, configDir : String, configFile : String, teamRoles : Map<String, Integer>, centralBuilder : String) {",
			"		this.agentName = agentName",
			"		this.configDir = configDir",
			"		this.configFile = configFile",
			"		this.teamRoles = teamRoles",
			"		this.centralBuilder = centralBuilder",
			"	}",
			"}",
			"agent Bug1013Agent {",
			"   uses Logging",
			"   var agentName : String",
			"   on E_SetupPlayer [occurrence.agentName == this.agentName] {",
			"		debug(\"Starting to register new behaviors and skills using {0} file\", occurrence.configFile)",
			"   }",
			"}");

	@Test
	@DisplayName("occurrence field to function")
	public void parsing02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertNoIssues();
	}

	private static final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug1013",
			"import io.sarl.core.Logging",
			"import java.util.Map",
			"event E_SetupPlayer {",
			"	var agentName : String",
			"	var configDir : String",
			"	var configFile : String",
			"	var teamRoles : Map<String, Integer>",
			"	var centralBuilder : String",
			"	new(agentName : String, configDir : String, configFile : String, teamRoles : Map<String, Integer>, centralBuilder : String) {",
			"		this.agentName = agentName",
			"		this.configDir = configDir",
			"		this.configFile = configFile",
			"		this.teamRoles = teamRoles",
			"		this.centralBuilder = centralBuilder",
			"	}",
			"}",
			"agent Bug1013Agent {",
			"   uses Logging",
			"   var agentName : String",
			"   on E_SetupPlayer [occurrence.agentName == this.agentName] {",
			"		debug(\"Starting to register new behaviors and skills using {0} file\", occurrence.configFile)",
			"		val teamRoles = occurrence.teamRoles",
			"       info(teamRoles)",
			"   }",
			"}");

	@Test
	@DisplayName("occurrence field to local variable and function argument")
	public void parsing03() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE,
				"Possible invalid usage of 'occurrence'",
				"be copied within a local variable")
			.assertNoIssues();
	}

	private static final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug1013",
			"import io.sarl.core.Logging",
			"import java.util.Map",
			"event E_SetupPlayer {",
			"	var agentName : String",
			"	var configDir : String",
			"	var configFile : String",
			"	var teamRoles : Map<String, Integer>",
			"	var centralBuilder : String",
			"	new(agentName : String, configDir : String, configFile : String, teamRoles : Map<String, Integer>, centralBuilder : String) {",
			"		this.agentName = agentName",
			"		this.configDir = configDir",
			"		this.configFile = configFile",
			"		this.teamRoles = teamRoles",
			"		this.centralBuilder = centralBuilder",
			"	}",
			"}",
			"agent Bug1013Agent {",
			"   uses Logging",
			"   var agentName : String",
			"   on E_SetupPlayer [occurrence.agentName == this.agentName] {",
			"		debug(\"Starting to register new behaviors and skills using {0} file\", occurrence.configFile)",
			"		val teamRoles = occurrence.teamRoles",
			"   }",
			"}");

	@Test
	@DisplayName("occurrence field to local variable and function argument, no local variable use")
	public void parsing04() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertWarning(
				XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE,
				"Possible invalid usage of 'occurrence'",
				"be copied within a local variable")
			.assertWarning(
					XtendPackage.eINSTANCE.getXtendVariableDeclaration(),
					org.eclipse.xtext.xbase.validation.IssueCodes.UNUSED_LOCAL_VARIABLE,
					"local variable teamRoles is not used")
			.assertNoIssues();
	}

}
