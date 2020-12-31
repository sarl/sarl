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

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Replace register function in Space by registerStrongParticipant and registerWeakParticipant.
 *
 * <p>https://github.com/sarl/sarl/issues/1008
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1008"
 */
@DisplayName("Bug #1008")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlParsing")
public class Bug1008Test extends AbstractSarlTest {

	private static final String SARL_CODE_01a = multilineString(
			"package io.sarl.lang.tests.bug1008",
			"import io.sarl.lang.annotation.ErrorOnCall",
			"interface XXX {",
			"   @ErrorOnCall(\"This is a message\")",
			"   def myfct : void",
			"   def myfct2 : void",
			"}",
			"class Bug1008Case {",
			"  def fct(obj : XXX) : void {",
			"     obj.myfct;",
			"     obj.myfct2",
			"  }",
			"}");

	@Test
	@DisplayName("instance access w/ annotation")
	public void parsing01a() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01a);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertError(
				XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
				IssueCodes.PROGRAMMATIC_ISSUE_ANNOTATION,
				"This is a message");
	}

	private static final String SARL_CODE_01b = multilineString(
			"package io.sarl.lang.tests.bug1008",
			"interface XXX {",
			"   def myfct : void",
			"   def myfct2 : void",
			"}",
			"class Bug1008Case {",
			"  def fct(obj : XXX) : void {",
			"     obj.myfct;",
			"     obj.myfct2",
			"  }",
			"}");

	@Test
	@DisplayName("instance access w/o annotation")
	public void parsing01b() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01b);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	private static final String SARL_CODE_02a = multilineString(
			"package io.sarl.lang.tests.bug1008",
			"import io.sarl.lang.annotation.ErrorOnCall",
			"class XXX {",
			"   @ErrorOnCall(\"This is a message\")",
			"   static def myfct : void {}",
			"   static def myfct2 : void {}",
			"}",
			"class Bug1008Case {",
			"  def fct : void {",
			"     XXX::myfct;",
			"     XXX::myfct2",
			"  }",
			"}");

	@Test
	@DisplayName("static access w/ annotation")
	public void parsing02a() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02a);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator
			.assertError(
				XbasePackage.eINSTANCE.getXAbstractFeatureCall(),
				IssueCodes.PROGRAMMATIC_ISSUE_ANNOTATION,
				"This is a message");
	}

	private static final String SARL_CODE_02b = multilineString(
			"package io.sarl.lang.tests.bug1008",
			"class XXX {",
			"   static def myfct : void {}",
			"   static def myfct2 : void {}",
			"}",
			"class Bug1008Case {",
			"  def fct : void {",
			"     XXX::myfct;",
			"     XXX::myfct2",
			"  }",
			"}");

	@Test
	@DisplayName("static access w/o annotation")
	public void parsing02b() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02b);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

}
