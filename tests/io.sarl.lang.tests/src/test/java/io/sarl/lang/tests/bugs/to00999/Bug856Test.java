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

/** Testing class for issue: Twice rooting error when declared a pure function..
 *
 * <p>https://github.com/sarl/sarl/issues/856
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/856"
 */
@DisplayName("Bug #856")
@SuppressWarnings("all")
@Tag("core")
public class Bug856Test extends AbstractSarlTest {

	private static final String SNIPSET01 = multilineString(
			"package io.sarl.lang.tests.bug856",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"import java.util.List",
			"import java.util.UUID",
			"class SomeClass {",
			"  @Pure",
			"  protected def getResults(agentId : UUID) : List<Object> {",
			"       newArrayList",
			"  }",
			"}");

	private static final String EXPECTED01 = multilineString(
			"package io.sarl.lang.tests.bug856;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.List;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.CollectionLiterals;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeClass {",
			"  @Pure",
			"  protected List<Object> getResults(final UUID agentId) {",
			"    return CollectionLiterals.<Object>newArrayList();",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeClass() {",
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
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug856.SomeClass");
			assertEquals(EXPECTED01, actual);
		});
	}

	private static final String SNIPSET02 = multilineString(
			"package io.sarl.lang.tests.bug856",
			"import org.eclipse.xtext.xbase.lib.Pure",
			"import java.util.List",
			"import java.util.UUID",
			"class SomeClass {",
			"  @Pure",
			"  protected def getResults(agentId : UUID = null) : List<Object> {",
			"       newArrayList",
			"  }",
			"}");

	private static final String EXPECTED02 = multilineString(
			"package io.sarl.lang.tests.bug856;",
			"",
			"import io.sarl.lang.annotation.DefaultValue;",
			"import io.sarl.lang.annotation.DefaultValueSource;",
			"import io.sarl.lang.annotation.DefaultValueUse;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSourceCode;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.List;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.CollectionLiterals;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING+ "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class SomeClass {",
			"  @DefaultValueSource",
			"  @Pure",
			"  protected List<Object> getResults(@DefaultValue(\"io.sarl.lang.tests.bug856.SomeClass#GETRESULTS_0\") final UUID agentId) {",
			"    return CollectionLiterals.<Object>newArrayList();",
			"  }",
			"  ",
			"  /**",
			"   * Default value for the parameter agentId",
			"   */",
			"  @Pure",
			"  @SyntheticMember",
			"  @SarlSourceCode(\"null\")",
			"  private final UUID $DEFAULT_VALUE$GETRESULTS_0() {",
			"    return null;",
			"  }",
			"  ",
			"  @DefaultValueUse(\"java.util.UUID\")",
			"  @SyntheticMember",
			"  @Pure",
			"  protected final List<Object> getResults() {",
			"    return getResults($DEFAULT_VALUE$GETRESULTS_0());",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public SomeClass() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void parsing_02() throws Exception {
		SarlScript mas = file(getParseHelper(), SNIPSET02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compiling_02() throws Exception {
		getCompileHelper().compile(SNIPSET02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug856.SomeClass");
			assertEquals(EXPECTED02, actual);
		});
	}

}

