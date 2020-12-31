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
package io.sarl.lang.tests.general.parsing.aop;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.xbase.XbasePackage;
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
@DisplayName("Syntax: @DefaultSkill")
@Tag("core")
public class DefaultSkillTest extends AbstractSarlTest {
		
	@Test
	@Tag("sarlValidation")
	public void valid() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.core.DefaultSkill",
				"@DefaultSkill(typeof(S1))",
				"capacity C1 {",
				"  def myFct",
				"}",
				"skill S1 implements C1 {",
				"  def myFct {}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

	@Test
	@Tag("sarlValidation")
	public void notASkill() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.core.DefaultSkill",
				"@DefaultSkill(typeof(S1))",
				"capacity C1 {",
				"  def myFct",
				"}",
				"class S1 {",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXTypeLiteral(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES,
				"Type mismatch");
	}

	@Test
	@Tag("sarlValidation")
	public void notACapacityImplementation() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.core.DefaultSkill",
				"@DefaultSkill(typeof(S1))",
				"capacity C1 {",
				"  def myFct",
				"}",
				"capacity C2 {",
				"  def myFct2",
				"}",
				"skill S1 implements C2 {",
				"  def myFct2 {}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertError(
				XbasePackage.eINSTANCE.getXTypeLiteral(),
				IssueCodes.INVALID_DEFAULT_SKILL_ANNOTATION,
				"Invalid annotation value. S1 is not an implementation of C1");
	}

	@Test
	@Tag("sarlValidation")
	public void validSubType() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"import io.sarl.lang.core.DefaultSkill",
				"@DefaultSkill(typeof(S1))",
				"capacity C1 {",
				"  def myFct",
				"}",
				"capacity C2 extends C1 {",
				"  def myFct2",
				"}",
				"skill S1 implements C2 {",
				"  def myFct {}",
				"  def myFct2 {}",
				"}"
				));
		validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
	}

}
