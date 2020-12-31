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
package io.sarl.lang.tests.general.parsing.general;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import com.google.common.base.Throwables;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
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
@DisplayName("Syntax: @Inline - explicit")
@Tag("core")
public class ManualInlineAnnotationTest extends AbstractSarlTest {

	@Test
	@Tag("sarlValidation")
	public void testDefaultValue() throws Exception {
		try {
			SarlScript mas = file(getParseHelper(), multilineString(
					"import org.eclipse.xtext.xbase.lib.Inline",
					"agent A1 {",
					"   @Inline(\"\")",
					"	def fct() {",
					"	}",
					"}"
					));
			validate(getValidationHelper(), getInjector(), mas).assertWarning(
					XAnnotationsPackage.eINSTANCE.getXAnnotation(),
					IssueCodes.MANUAL_INLINE_DEFINITION,
					"Discouraged manual definition of an inline expression");
		} catch (Throwable e) {
			e.printStackTrace();
			Throwables.propagate(e);
		}
	}

}
