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
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Implicit void returned type if the expression is a statement.
 *
 * <p>https://github.com/sarl/sarl/issues/696
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #696")
@SuppressWarnings("all")
@Tag("core")
public class Bug696Test extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug696",
			"class X {",
			"   var f : int",
			"   def myfunction(p : float){",
			"      f = p as int",
			"   }",
			"}");

	private final String EXPECTED1 = multilineString(
			"package io.sarl.lang.tests.bug696;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  private int f;",
			"  ",
			"  public void myfunction(final float p) {",
			"    this.f = ((int) p);",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public boolean equals(final Object obj) {",
			"    if (this == obj)",
			"      return true;",
			"    if (obj == null)",
			"      return false;",
			"    if (getClass() != obj.getClass())",
			"      return false;",
			"    X other = (X) obj;",
			"    if (other.f != this.f)",
			"      return false;",
			"    return super.equals(obj);",
			"  }",
			"  ",
			"  @Override",
			"  @Pure",
			"  @SyntheticMember",
			"  public int hashCode() {",
			"    int result = super.hashCode();",
			"    final int prime = 31;",
			"    result = prime * result + Integer.hashCode(this.f);",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_01() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET1);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug696.X");
			assertEquals(EXPECTED1, actual);
		});
	}

}
