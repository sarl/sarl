/*
 * Copyright 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.tests.bugs.to00399;

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

/** Test for the issue: Obtain agent ID from space.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/291"
 */
@DisplayName("Bug #291")
@SuppressWarnings("all")
@Tag("core")
public class Bug291Test extends AbstractSarlTest {

	private static final String SOURCE_01 = multilineString(
			"package io.sarl.lang.tests.bug291",
			"capacity C1 {",
			"  def myfct : int",
			"}",
			"skill S1 implements C1 {",
			"  def myfct : int {",
			"    if (caller === null) {",
			"    }",
			"    return 0",
			"  }",
			"}");
	
	private static final String EXPECTED_01 = multilineString(
			"package io.sarl.lang.tests.bug291;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.AgentTrait;",
			"import io.sarl.lang.core.Capacities;",
			"import io.sarl.lang.core.Skill;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
			"@SuppressWarnings(\"all\")",
			"public class S1 extends Skill implements C1 {",
			"  public int myfct() {",
			"    AgentTrait _caller = Capacities.getCaller();",
			"    if ((_caller == null)) {",
			"    }",
			"    return 0;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public S1() {",
			"    super();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public S1(final Agent arg0) {",
			"    super(arg0);",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void testParser() throws Exception {
		SarlScript script = file(getParseHelper(), SOURCE_01);
		validate(getValidationHelper(), getInjector(), script).assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void testCompiler() throws Exception {
		getCompileHelper().compile(SOURCE_01, (r) -> {
			assertEquals(EXPECTED_01, r.getGeneratedCode("io.sarl.lang.tests.bug291.S1"));
		});
	}

}
