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

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.bugs.to00999.Bug730Test;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Ambiguous call to capacity functions
 *
 * <p>https://github.com/sarl/sarl/issues/558
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see Bug730Test
 */
@DisplayName("Bug #558")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug558Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug558",
			"event MyEvent",
			"capacity C1 {",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"agent TestAgent {",
			"  uses C1, C2",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");
	
	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug558",
			"event MyEvent",
			"capacity C1 {",
			"  @Deprecated",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"agent TestAgent {",
			"  uses C1, C2",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug558",
			"event MyEvent",
			"capacity C1 {",
			"  @Deprecated",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"capacity C3 {",
			"  @Deprecated",
			"  def myfunction",
			"  def myfunction4",
			"}",
			"agent TestAgent {",
			"  uses C1, C2, C3",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug558",
			"event MyEvent",
			"capacity C1 {",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  @Deprecated",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"capacity C3 {",
			"  def myfunction",
			"  def myfunction4",
			"}",
			"agent TestAgent {",
			"  uses C1, C2, C3",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug558",
			"event MyEvent",
			"capacity C1 {",
			"  @Deprecated",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  @Deprecated",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"capacity C3 {",
			"  @Deprecated",
			"  def myfunction",
			"  def myfunction4",
			"}",
			"agent TestAgent {",
			"  uses C1, C2, C3",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");

	private static final String SNIPSET6 = multilineString(
			"package io.sarl.lang.tests.bug558",
			"event MyEvent",
			"capacity C1 {",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"capacity C3 {",
			"  def myfunction",
			"  def myfunction4",
			"}",
			"agent TestAgent {",
			"  uses C1, C2, C3",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");

	private static final String SNIPSET7 = multilineString(
			"package io.sarl.lang.tests.bug558",
			"event MyEvent",
			"capacity C1 {",
			"  def myfunction",
			"  def myfunction2",
			"}",
			"capacity C2 {",
			"  @Generated",
			"  def myfunction",
			"  def myfunction3",
			"}",
			"capacity C3 {",
			"  def myfunction",
			"  def myfunction4",
			"}",
			"agent TestAgent {",
			"  uses C1, C2, C3",
			"  on MyEvent {",
			"    myfunction",
			"    myfunction2",
			"    myfunction3",
			"  }",
			"}");
	
	@Test
	public void snipset1() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.AMBIGUOUS_FEATURE_CALL,
				"myfunction() in C1", "myfunction() in C2");
	}

	@Test
	public void snipset2() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET2);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	public void snipset3() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET3);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	public void snipset4() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET4);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.AMBIGUOUS_FEATURE_CALL,
				"myfunction() in C1", "myfunction() in C3");
	}

	@Test
	public void snipset5() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET5);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.AMBIGUOUS_FEATURE_CALL,
				"myfunction() in C1", "myfunction() in C2", "myfunction() in C3");
	}

	@Test
	public void snipset6() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET6);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.AMBIGUOUS_FEATURE_CALL,
				"myfunction() in C1", "myfunction() in C2", "myfunction() in C3");
	}

	@Test
	public void snipset7() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET7);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertError(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.AMBIGUOUS_FEATURE_CALL,
				"myfunction() in C1", "myfunction() in C3");
	}

}
