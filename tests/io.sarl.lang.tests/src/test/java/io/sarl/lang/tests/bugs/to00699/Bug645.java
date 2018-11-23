/*
 * Copyright (C) 2014-2018 the original authors or authors.
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

import com.google.inject.Inject;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Testing class for issue: Smaller compilation units.
 *
 * <p>https://github.com/sarl/sarl/issues/645
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug645 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug645",
			"",
			"class XXX {",
			"  DDD",
			"}",
			"",
			"class YYY {",
			"  var x : XXX",
			"  def getX {",
			"    x",
			"  }",
			"}");

	private final String EXPECTED_XXX1 = multilineString(
			"package io.sarl.lang.tests.bug645;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class XXX {",
			"  @SyntheticMember",
			"  public XXX() {",
			"    super();",
			"  }",
			"}",
			"");

	private final String EXPECTED_YYY1 = multilineString(
			"package io.sarl.lang.tests.bug645;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.tests.bug645.XXX;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class YYY {",
			"  private XXX x;",
			"  ",
			"  @Pure",
			"  public XXX getX() {",
			"    return this.x;",
			"  }",
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
			"  public YYY() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void validating_01() throws Exception {
		Validator validator = validate(file(SNIPSET1));
		validator.assertError(
				SarlPackage.eINSTANCE.getSarlClass(),
				Diagnostic.SYNTAX_DIAGNOSTIC,
				"extraneous input 'DDD'");
		assertEquals("Too many issues: " + validator.getIssues().toString(), 1, validator.getIssues().size());
	}

	@Test
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			assertEquals("Invalid YYY", EXPECTED_YYY1, it.getGeneratedCode("io.sarl.lang.tests.bug645.YYY"));
			assertEquals("Invalid XXX", EXPECTED_XXX1, it.getGeneratedCode("io.sarl.lang.tests.bug645.XXX"));
		});
	}

}
