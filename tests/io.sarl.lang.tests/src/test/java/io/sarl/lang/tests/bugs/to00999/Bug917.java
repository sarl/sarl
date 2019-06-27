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

package io.sarl.lang.tests.bugs.to00999;

import com.google.inject.Inject;
import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

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
@SuppressWarnings("all")
public class Bug917 extends AbstractSarlTest {

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
	public void parsing() throws Exception {
		SarlScript mas = file(SARL_CODE);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling() throws Exception {
		getCompileHelper().compile(SARL_CODE, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug917.SimulationScenario");
			assertEquals(JAVA_CODE, actual);
		});
	}

}
