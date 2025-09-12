/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.api.core.tests.bugs;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Compiler accepts "uses <SkillName>" the same as "uses <CapacityName>".
 *
 * <p>https://github.com/sarl/sarl/issues/835
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/835"
 */
@DisplayName("Bug #835")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug835Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package ^skill.^capacity.bug",
			"import io.sarl.api.core.Initialize",
			"capacity SomeCapacity{",
			"	def someFunction",
			"}",
			"skill SomeSkill implements SomeCapacity{",
			"	def someFunction{",
			"	}",
			"}",
			"agent SomeAgent{",
			"	uses SomeSkill",
			"   on Initialize {",
			"       someFunction",
			"   }",
			"}");

	private static final String SNIPSET02 = multilineString(
			"package ^skill.^capacity.bug",
			"import io.sarl.api.core.Initialize",
			"capacity SomeCapacity{",
			"	def someFunction",
			"}",
			"skill SomeSkill implements SomeCapacity{",
			"	def someFunction{",
			"	}",
			"}",
			"agent SomeAgent{",
			"	uses SomeCapacity",	
			"   on Initialize {",
			"       someFunction",
			"   }",
			"}");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.INVALID_CAPACITY_TYPE,
				"Only capacities can be used after the keyword 'uses'");
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

}

