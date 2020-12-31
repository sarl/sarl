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

/** Testing class for issue: Compilation error on cast of Map.
 *
 * <p>https://github.com/sarl/sarl/issues/994
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/994"
 */
@DisplayName("Bug #994")
@SuppressWarnings("all")
@Tag("core")
public class Bug994Test extends AbstractSarlTest {

	private static final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug994",
			"import java.util.Map",
			"class Bug994Case {",
			"  def f(bp : Map<String, Object>) {",
			"    val m  = bp.get(\"initialLaneLaw\")",
			"    if (m instanceof Map) {",
			"      val desc = m as Map<String, Object>",
			"    }",
			"  }",
			"}");

	private static final String JAVA_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug994;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import java.util.Map;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")", 
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")", 
			"@SuppressWarnings(\"all\")", 
			"public class Bug994Case {",
			"  public void f(final Map<String, Object> bp) {",
			"    final Object m = bp.get(\"initialLaneLaw\");",
			"    if ((m instanceof Map)) {",
			"      final Map<String, Object> desc = ((Map<String, Object>) m);",
			"    }",
			"  }",
			"  ", 
			"  @SyntheticMember",
			"  public Bug994Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST,
				"Unnecessary cast from Map to Map<String, Object>");
	}

	@Test
	@Tag("compileToJava")
	public void compile01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug994.Bug994Case");
			assertEquals(JAVA_CODE_01, actual);
		});
	}

	private static final String SARL_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug994",
			"import java.util.Map",
			"class Bug994Case {",
			"  def f(bp : Map<String, Object>) : Object {",
			"    val m  = bp.get(\"initialLaneLaw\")",
			"    if (m instanceof Map) {",
			"      val desc = m",
			"      return desc",
			"    }",
			"    return null",
			"  }",
			"}");

	private static final String JAVA_CODE_02 = multilineString(
			"package io.sarl.lang.tests.bug994;",
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
			"public class Bug994Case {",
			"  @Pure",
			"  public Object f(final Map<String, Object> bp) {",
			"    final Object m = bp.get(\"initialLaneLaw\");",
			"    if ((m instanceof Map)) {",
			"      final Map desc = ((Map)m);",
			"      return desc;",
			"    }",
			"    return null;",
			"  }",
			"  ", 
			"  @SyntheticMember",
			"  public Bug994Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	public void parsing02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compile02() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug994.Bug994Case");
			assertEquals(JAVA_CODE_02, actual);
		});
	}

	private static final String SARL_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug994",
			"import java.util.Map",
			"class Bug994Case {",
			"  def f(bp : Map<String, Object>) : Map<String, Object> {",
			"    val m  = bp.get(\"initialLaneLaw\")",
			"    if (m instanceof Map) {",
			"      val desc = m",
			"      return desc",
			"    }",
			"    return null",
			"  }",
			"}");

	private static final String JAVA_CODE_03 = multilineString(
			"package io.sarl.lang.tests.bug994;",
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
			"public class Bug994Case {",
			"  @Pure",
			"  public Map<String, Object> f(final Map<String, Object> bp) {",
			"    final Object m = bp.get(\"initialLaneLaw\");",
			"    if ((m instanceof Map)) {",
			"      final Map desc = ((Map)m);",
			"      return desc;",
			"    }",
			"    return null;",
			"  }",
			"  ", 
			"  @SyntheticMember",
			"  public Bug994Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	public void parsing03() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_03);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@Tag("compileToJava")
	public void compile03() throws Exception {
		getCompileHelper().compile(SARL_CODE_03, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug994.Bug994Case");
			assertEquals(JAVA_CODE_03, actual);
		});
	}

	private static final String SARL_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug994",
			"import java.util.Map",
			"class Bug994Case {",
			"  def f(bp : Map<String, Object>) : Map<String, Object> {",
			"    val m  = bp.get(\"initialLaneLaw\")",
			"    if (m instanceof Map) {",
			"      val desc = m as Map<String, Object>",
			"      return desc",
			"    }",
			"    return null",
			"  }",
			"}");

	private static final String JAVA_CODE_04 = multilineString(
			"package io.sarl.lang.tests.bug994;",
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
			"public class Bug994Case {",
			"  @Pure",
			"  public Map<String, Object> f(final Map<String, Object> bp) {",
			"    final Object m = bp.get(\"initialLaneLaw\");",
			"    if ((m instanceof Map)) {",
			"      final Map<String, Object> desc = ((Map<String, Object>) m);",
			"      return desc;",
			"    }",
			"    return null;",
			"  }",
			"  ", 
			"  @SyntheticMember",
			"  public Bug994Case() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlParsing")
	public void parsing04() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_04);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST,
				"Unnecessary cast from Map to Map<String, Object>");
	}

	@Test
	@Tag("compileToJava")
	public void compile04() throws Exception {
		getCompileHelper().compile(SARL_CODE_04, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug994.Bug994Case");
			assertEquals(JAVA_CODE_04, actual);
		});
	}

}
