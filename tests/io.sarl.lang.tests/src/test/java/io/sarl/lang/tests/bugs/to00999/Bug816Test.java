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

/** Testing class for issue: Illegal assert statement generation.
 *
 * <p>https://github.com/sarl/sarl/issues/816
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/816"
 */
@DisplayName("Bug #816")
@SuppressWarnings("all")
@Tag("core")
public class Bug816Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"class X {",
			"  var bounds : String",
			"  def fct(condition1 : boolean, condition2 : boolean) : void {",
			"    if (condition1) {",
			"      var bounds = this.bounds",
			"      assert bounds !== null",
			"    }",
			"    if (condition2) {",
			"      var bounds = this.bounds",
			"      assert bounds !== null",
			"    }",
			"  }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  private String bounds;",
			"  ",
			"  public void fct(final boolean condition1, final boolean condition2) {",
			"    if (condition1) {",
			"      String bounds = this.bounds;",
			"      class $AssertEvaluator$ {",
			"        final boolean $$result;",
			"        $AssertEvaluator$(final String bounds) {",
			"          this.$$result = (bounds != null);",
			"        }",
			"      }",
			"      assert new $AssertEvaluator$(bounds).$$result;",
			"    }",
			"    if (condition2) {",
			"      String bounds_1 = this.bounds;",
			"      class $AssertEvaluator$_1 {",
			"        final boolean $$result;",
			"        $AssertEvaluator$_1(final String bounds_1) {",
			"          this.$$result = (bounds_1 != null);",
			"        }",
			"      }",
			"      assert new $AssertEvaluator$_1(bounds_1).$$result;",
			"    }",
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
			"    if (!Objects.equals(this.bounds, other.bounds))",
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
			"    result = prime * result + Objects.hashCode(this.bounds);",
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
		SarlScript mas = file(getParseHelper(), SNIPSET01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_01() throws Exception {
		getCompileHelper().compile(SNIPSET01, (it) -> {
			String actual = it.getGeneratedCode("X");
			assertEquals(EXPECTED01, actual);
		});
	}

}

