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

package io.sarl.lang.tests.bugs.to00999;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Annotation problem: Accessors cannot be used.
 *
 * <p>https://github.com/sarl/sarl/issues/917
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/917"
 */
@DisplayName("Bug #917")
@SuppressWarnings("all")
@Tag("core")
public class Bug917Test extends AbstractSarlTest {

	private static final String SARL_CODE = multilineString(
			"package io.sarl.lang.tests.bug917",
			"import org.eclipse.xtend.lib.annotations.Accessors",
			"class SimulationScenario {",
			"  @Accessors",
			"  var mapContainer : Object",
			"}");

	private static final String JAVA_CODE = multilineString(
			"package io.sarl.lang.tests.bug917;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtend.lib.annotations.Accessors;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class SimulationScenario {",
			"  @Accessors",
			"  private Object mapContainer;",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    return super.equals(obj);",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SimulationScenario() {",
			"    super();",
			"  }",
			"  ",
			"  @Pure",
			"  public Object getMapContainer() {",
			"    return this.mapContainer;",
			"  }",
			"  ",
			"  public void setMapContainer(final Object mapContainer) {",
			"    this.mapContainer = mapContainer;",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling() throws Exception {
		getCompileHelper().compile(SARL_CODE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug917.SimulationScenario");
			assertEquals(JAVA_CODE, actual);
		});
	}

}
