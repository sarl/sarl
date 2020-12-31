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

import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid cast on String.
 *
 * <p>https://github.com/sarl/sarl/issues/995
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/995"
 */
@DisplayName("Bug #995")
@SuppressWarnings("all")
@Tag("core")
public class Bug995Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug995",
			"import java.util.Map",
			"class Bug995Case {",
			"  def f(config : Map<String, Object>) : String {", 
			"    val name = config.get(\"name\") as String",
			"    return name",
			"  }",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug995;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.Map;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")", 
			"public class Bug995Case {",
			"  @Pure",
			"  public String f(final Map<String, Object> config) {",
			"    Object _get = config.get(\"name\");",
			"    final String name = (_get == null ? null : _get.toString());",
			"    return name;",
			"  }",
			"  ", 
			"  @SyntheticMember",
			"  public Bug995Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoErrors();
	}

	@Test
	@Tag("compileToJava")
	public void compile01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug995.Bug995Case");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

}
