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

/** Testing class for issue: Invalid generation of the hashCode function with Integer fields.
 *
 * <p>https://github.com/sarl/sarl/issues/978
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/978"
 */
@DisplayName("Bug #978")
@SuppressWarnings("all")
@Tag("core")
public class Bug978Test extends AbstractSarlTest {

	/** Expression elements are inside the same resource as the expression.
	 */
	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug978",
			"capacity MyCapacity {",
			"}",
			"skill MySkill implements MyCapacity {",
			"  var field0 : Integer",
			"  var field1 : int",
			"  var field2 : Object",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug978;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;", 
			"import io.sarl.lang.annotation.SarlSpecification;", 
			"import io.sarl.lang.annotation.SyntheticMember;", 
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Skill;",
			"import java.util.Objects;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@SuppressWarnings(\"all\")",
			"public class MySkill extends Skill implements MyCapacity {",
			"  private Integer field0;",
			"  ",
			"  private int field1;", 
			"  ",
			"  private Object field2;",
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
			"    MySkill other = (MySkill) obj;",
			"    if (other.field0 == null) {",
			"      if (this.field0 != null)",
			"        return false;",
			"    } else if (this.field0 == null)",
			"      return false;",
			"    if (other.field0 != null && other.field0.intValue() != this.field0.intValue())",
			"      return false;",
			"    if (other.field1 != this.field1)",
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
			"    result = prime * result + Objects.hashCode(this.field0);",
			"    result = prime * result + Integer.hashCode(this.field1);",
			"    return result;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public MySkill() {",
			"    super();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public MySkill(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			String actual;
			actual = it.getGeneratedCode("io.sarl.lang.tests.bug978.MySkill");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

}
