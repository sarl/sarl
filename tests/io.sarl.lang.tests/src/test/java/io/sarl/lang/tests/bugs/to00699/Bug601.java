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

package io.sarl.lang.tests.bugs.to00699;

import java.awt.event.WindowEvent;

import com.google.inject.Inject;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Occurrence keyword redefinition outside a event handler.
 *
 * <p>https://github.com/sarl/sarl/issues/601
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug601 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"class XXX {",
			"  def fct(occurrence : int) { }",
			"}");

	private static final String SNIPSET2 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"class XXX {",
			"  var occurrence : int",
			"}");

	private static final String SNIPSET3 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"class XXX {",
			"  val occurrence = 1",
			"}");

	private static final String SNIPSET4 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"class XXX {",
			"  def fct {",
			"    var occurrence : int",
			"  }",
			"}");

	private static final String SNIPSET5 = multilineString(
			"package io.sarl.lang.tests.bug601",
			"agent XXX {",
			"  on Event {",
			"    var occurrence = 1",
			"  }",
			"}");

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET1);
		final Validator validator = validate(mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlFormalParameter(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid parameter name 'occurrence'");
	}

	@Test
	public void parsing_02() throws Exception {
		SarlScript mas = file(SNIPSET2);
		final Validator validator = validate(mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid attribute name 'occurrence'");
	}

	@Test
	public void parsing_03() throws Exception {
		SarlScript mas = file(SNIPSET3);
		final Validator validator = validate(mas);
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlField(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid attribute name 'occurrence'");
	}

	@Test
	public void parsing_04() throws Exception {
		SarlScript mas = file(SNIPSET4);
		final Validator validator = validate(mas);
		validator.assertError(
				XtendPackage.eINSTANCE.getXtendVariableDeclaration(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid variable name 'occurrence'");
	}

	@Test
	public void parsing_05() throws Exception {
		SarlScript mas = file(SNIPSET5);
		final Validator validator = validate(mas);
		validator.assertError(
				XtendPackage.eINSTANCE.getXtendVariableDeclaration(),
				IssueCodes.VARIABLE_NAME_DISALLOWED,
				"Invalid variable name 'occurrence'");
	}

}
