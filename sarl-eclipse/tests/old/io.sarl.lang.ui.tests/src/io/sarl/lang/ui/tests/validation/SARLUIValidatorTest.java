/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

import static io.sarl.tests.api.tools.TestEObjects.*;
import static io.sarl.tests.api.tools.TestUtils.*;
import static io.sarl.tests.api.tools.TestValidator.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.eclipse.tests.api.AbstractSarlUiTest;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Validation: UI validator")
@Tag("ui")
public class SARLUIValidatorTest extends AbstractSarlUiTest {

	@Test
	@DisplayName("Valid package name")
	public void checkFileNamingConventions_validPackageName() throws Exception {
		SarlScript script = file(getParseHelper(), "package " + getDefaultTestPackage());
		validate(getValidationHelper(), getInjector(), script).assertNoIssues();
	}

	@Test
	@DisplayName("Invalid package name")
	public void checkFileNamingConventions_wrongPackageName() throws Exception {
		SarlScript script = file(getParseHelper(), "package fake." + getDefaultTestPackage());
		validate(getValidationHelper(), getInjector(), script).assertWarning(
				SarlPackage.eINSTANCE.getSarlScript(),
				org.eclipse.xtend.core.validation.IssueCodes.WRONG_PACKAGE,
				"Expecting package definition io.sarl.tests"); //$NON-NLS-1$
	}

}
