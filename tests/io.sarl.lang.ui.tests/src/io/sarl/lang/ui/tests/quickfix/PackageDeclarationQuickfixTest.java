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
package io.sarl.lang.ui.tests.quickfix;

import org.junit.Ignore;
import org.junit.Test;

@SuppressWarnings("all")
public class PackageDeclarationQuickfixTest extends AbstractSARLQuickfixTest {

	@Test
	public void fixPackageName() {
		assertQuickFix(
				org.eclipse.xtend.core.validation.IssueCodes.WRONG_PACKAGE,
				// Code to fix:
				"package foo.^package",
				//
				// Label and description:
				//
				"Change package declaration to 'io.sarl.tests.quickfix.org_eclipse_xtend_core_validation_issuecodes_wrong_package'",
				//
				// Expected fixed code:
				//
				"package io.sarl.tests.quickfix.org_eclipse_xtend_core_validation_issuecodes_wrong_package");
	}

}
