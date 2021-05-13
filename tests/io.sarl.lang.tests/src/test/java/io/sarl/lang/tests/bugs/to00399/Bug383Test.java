/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * $Id$
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
package io.sarl.lang.tests.bugs.to00399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Bug #383")
@SuppressWarnings("all")
@Tag("core")
public class Bug383Test {

	protected static String snippet = multilineString(
			"agent A1 {",
			"  /** Testing documentation generator.",
			"   * @param type the type.",
			"   * @param defaultValue the default value.",
			"   * @return the value.",
			"   */",
			"  def getInstance(type : String, defaultValue : String = null) : String {",
			"    return null",
			"  }",
			"}");

	@Nested
	@Tag("sarlValidation")
	public static class ParserTest extends AbstractSarlTest {

		@Test
		public void parsing() throws Exception {
			SarlScript mas = file(getParseHelper(), snippet);
			validate(getValidationHelper(), getInjector(), mas).assertNoErrors();
		}

	}

	@Nested
	@Tag("compileToJava")
	public static class CompilerTest extends AbstractSarlTest {
		
		@Test
		public void compilation() throws Exception {
			final String expected = multilineString(
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.DynamicSkillProvider;",
					"import java.util.UUID;",
					"import javax.inject.Inject;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
					"@SuppressWarnings(\"all\")",
					"public class A1 extends Agent {",
					"  /**",
					"   * Testing documentation generator.",
					"   * @param type the type.",
					"   * @param defaultValue the default value.",
					"   * @return the value.",
					"   */",
					"  @DefaultValueSource",
					"  @Pure",
					"  protected String getInstance(final String type, @DefaultValue(\"A1#GETINSTANCE_0\") final String defaultValue) {",
					"    return null;",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter defaultValue",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"null\")",
					"  private final String $DEFAULT_VALUE$GETINSTANCE_0() {",
					"    return null;",
					"  }",
					"  ",
					"  /**",
					"   * Testing documentation generator.",
					"   * @param type the type.",
					"   * @optionalparam defaultValue the default value.",
					"   * @return the value.",
					"   */",
					"  @DefaultValueUse(\"java.lang.String,java.lang.String\")",
					"  @SyntheticMember",
					"  @Pure",
					"  protected final String getInstance(final String type) {",
					"    return getInstance(type, $DEFAULT_VALUE$GETINSTANCE_0());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public A1(final UUID arg0, final UUID arg1) {",
					"    super(arg0, arg1);",
					"  }",
					"  ", 
					"  @SyntheticMember", 
					"  @Inject", 
					"  public A1(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {", 
					"    super(arg0, arg1, arg2);", 
					"  }",
					"}",
					"");

			getCompileHelper().compile(snippet, 
					(r) -> assertEquals(expected, r.getGeneratedCode("A1")));
		}

	}

}
