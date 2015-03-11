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
package io.sarl.lang.ui.tests.validation;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlUiTest;

import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
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
public class SARLUIValidatorTest extends AbstractSarlUiTest {

	@Inject
	private ValidationTestHelper validator;

	/**
	 * @throws Exception
	 */
	@Test
	public void checkFileNamingConventions_validPackageName() throws Exception {
		XtendFile script = this.helper.createSARLScript(
				pathStr("io","sarl","mypackage","test"), //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$
				"package io.sarl.mypackage"); //$NON-NLS-1$
		this.validator.assertNoIssues(script);
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void checkFileNamingConventions_wrongPackageName() throws Exception {
		XtendFile script = this.helper.createSARLScript(
				pathStr("io","sarl","mypackage","test"), //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$
				"package io.sarl.myotherpackage"); //$NON-NLS-1$
		this.validator.assertWarning(
				script,
				XtendPackage.eINSTANCE.getXtendFile(),
				org.eclipse.xtend.core.validation.IssueCodes.WRONG_PACKAGE,
				"The declared package 'io.sarl.myotherpackage' does not match the expected package 'io.sarl.mypackage'"); //$NON-NLS-1$
	}

}
