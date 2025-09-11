/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import com.google.inject.Inject;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.junit.Before;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.validation.IConfigurableIssueSeveritiesProvider;
import io.sarl.tests.api.tools.TestValidator.Validator;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #507")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlValidation")
public class Bug507Test extends AbstractSarlTest {

	@Inject
	private IConfigurableIssueSeveritiesProvider configurableIssueSeveritiesProvider;
	
	@Before
	public void setUp() {
		// Enable the warning (SARL and Java)
		this.configurableIssueSeveritiesProvider.setSeverity(
				IssueCodes.DEPRECATED_MEMBER_REFERENCE, Severity.WARNING);
		this.configurableIssueSeveritiesProvider.setSeverity(
				IssueCodes.DISCOURAGED_REFERENCE, Severity.WARNING);
	}
	
	@Test
	public void snipset1() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"event Initialize",
				"capacity Lifecycle { def killMe }",
				"capacity Logging {",
				"  @Deprecated def println(m : String)",
				"  def info(m : String)",
				"}",
				"capacity Schedules {",
				"  def in(n : long, h : () => void)",
				"}",
				"agent TestAgent {",
				"  uses Logging, Lifecycle, Schedules",
				"  on Initialize {",
				"    println(\"Hello World!\")",
				"    in(2000)[",
				"      println(\"Kill myself\")",
				"      killMe",
				"    ]",
				"  }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.DISCOURAGED_REFERENCE,
				"Discouraged feature call: println");
	}

	@Test
	public void snipset2() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"event Initialize",
				"capacity Lifecycle { def killMe }",
				"capacity Logging {",
				"  @Deprecated def println(m : String)",
				"  def info(m : String)",
				"}",
				"capacity Schedules {",
				"  def in(n : long, h : () => void)",
				"}",
				"agent TestAgent {",
				"  uses Logging, Lifecycle, Schedules",
				"  on Initialize {",
				"    info(\"Hello World!\")",
				"    in(2000)[",
				"      info(\"Kill myself\")",
				"      killMe",
				"    ]",
				"  }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoWarnings(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.DEPRECATED_MEMBER_REFERENCE);
		validator.assertNoWarnings(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.DISCOURAGED_REFERENCE);
	}

	@Test
	public void snipset3() throws Exception {
		SarlScript mas = file(getParseHelper(), multilineString(
				"event Initialize",
				"capacity Lifecycle { def killMe }",
				"capacity Schedules {",
				"  def in(n : long, h : () => void)",
				"}",
				"agent TestAgent {",
				"  uses Lifecycle, Schedules",
				"  on Initialize {",
				"    println(\"Hello World!\")",
				"    in(2000)[",
				"      println(\"Kill myself\")",
				"      killMe",
				"    ]",
				"  }",
				"}"));
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				XbasePackage.eINSTANCE.getXFeatureCall(),
				IssueCodes.DISCOURAGED_REFERENCE,
				"Discouraged feature call: println");
	}

}
