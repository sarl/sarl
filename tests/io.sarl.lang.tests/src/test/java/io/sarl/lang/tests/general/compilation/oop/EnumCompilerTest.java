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
package io.sarl.lang.tests.general.compilation.oop;

import static io.sarl.tests.api.tools.TestUtils.multilineString;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Compilation: enum")
@Tag("core")
@Tag("compileToJava")
public class EnumCompilerTest {

	@Nested
	public class TopLevelTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "enum E1 { CST1, CST2 }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_ENUMERATION + ")",
					"@SuppressWarnings(\"all\")",
					"public enum E1 {",
					"  CST1,",
					"  ",
					"  CST2;",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class InClassTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "class Container { enum E1 { CST1, CST2 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_ENUMERATION + ")",
					"  public enum E1 {",
					"    CST1,",
					"    ",
					"    CST2;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container() {",
					"    super();",
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

	@Nested
	public class InAgentTest extends AbstractSarlTest {

		@Test
		public void basic() throws Exception {
			String source = "agent Container { enum E1 { CST1, CST2 } }";
			String expected = multilineString(
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class Container extends Agent {",
					"  @SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"  @SarlElementType(" + SarlPackage.SARL_ENUMERATION + ")",
					"  protected enum E1 {",
					"    CST1,",
					"    ",
					"    CST2;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Container(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public Container(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					""
					);
			getCompileHelper().assertCompilesTo(source, expected);
		}

	}

}
