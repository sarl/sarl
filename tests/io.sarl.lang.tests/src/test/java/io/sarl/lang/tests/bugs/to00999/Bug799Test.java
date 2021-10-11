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

/** Testing class for issue: Invalid return type with the Maven compiler
 *
 * <p>https://github.com/sarl/sarl/issues/799
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/799"
 */
@DisplayName("Bug #799")
@SuppressWarnings("all")
@Tag("core")
public class Bug799Test extends AbstractSarlTest {


	private static final String SNIPSET01 = multilineString(
			"package foo.bug799",
			"skill A implements Cap {",
			"  def act_connectToSimulator(address : String, port : int)  {",
			"    while(true) {}",
			"  }",
			"}",
			"skill B implements Cap {",
			"  def act_connectToSimulator(address : String, port : int)  {",
			"    while(true) {}",
			"  }",
			"}");
	
	private static final String EXPECTED01a = multilineString(
			"package foo.bug799;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Skill;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@SuppressWarnings(\"all\")",
			"public class A extends Skill implements Cap {",
			"  public void act_connectToSimulator(final String address, final int port) {",
			"    while (true) {",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	private static final String EXPECTED01b = multilineString(
			"package foo.bug799;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.Skill;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@SuppressWarnings(\"all\")",
			"public class B extends Skill implements Cap {",
			"  public void act_connectToSimulator(final String address, final int port) {",
			"    while (true) {",
			"    }",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public B() {",
			"    super();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public B(final Agent arg0) {",
			"    super(arg0);",
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
			String actual = it.getGeneratedCode("foo.bug799.A");
			assertEquals(EXPECTED01a, actual);
			actual = it.getGeneratedCode("foo.bug799.B");
			assertEquals(EXPECTED01b, actual);
		});
	}

}

