/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestUtils;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid error when extending a Capacity.
 *
 * <p>https://github.com/sarl/sarl/issues/1092
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler.tests 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @see "https://github.com/sarl/sarl/issues/1092"
 */
@DisplayName("Bug #1092")
@SuppressWarnings("all")
@Tag("core")
public class Bug1092Test extends AbstractSarlTest {

	private final String SARL_CODE_BASE = multilineString(
			"capacity Logging {",
			"   def info(text : String)",
			"   def debug(text : String)",
			"}",
			"capacity ErrorLogging extends Logging {",
			"   def error(text : String)",
			"}");

	private final String SARL_CODE_01 = multilineString(
			"package io.sarl.lang.tests.bug1092",
			SARL_CODE_BASE);
	
	private final String JAVA_CODE_BASE = multilineString(
			"import io.sarl.lang.core.AgentTrait;",
			"import io.sarl.lang.core.Capacity;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public interface Logging extends Capacity {",
			"  void info(final String text);",
			"  ",
			"  void debug(final String text);",
			"",
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends Logging> extends Capacity.ContextAwareCapacityWrapper<C> implements Logging {",
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
			"      super(capacity, caller);",
			"    }",
			"",
			"    public void info(final String text) {",
			"      try {",
			"        ensureCallerInLocalThread();",
			"        this.capacity.info(text);",
			"      } finally {",
			"        resetCallerInLocalThread();",
			"      }",
			"    }",
			"",
			"    public void debug(final String text) {",
			"      try {",
			"        ensureCallerInLocalThread();",
			"        this.capacity.debug(text);",
			"      } finally {",
			"        resetCallerInLocalThread();",
			"      }",
			"    }",
			"  }",
			"}",
			"");

	private final String JAVA_CODE_01 = "package io.sarl.lang.tests.bug1092;"
			+ TestUtils.getLineSeparator() + TestUtils.getLineSeparator() + JAVA_CODE_BASE;

	private final String JAVA_CODE_BASE_B = multilineString(
			"import io.sarl.lang.core.AgentTrait;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public interface ErrorLogging extends Logging {",
			"  void error(final String text);",
			"",
			"  /**",
			"   * @ExcludeFromApidoc",
			"   */",
			"  class ContextAwareCapacityWrapper<C extends ErrorLogging> extends Logging.ContextAwareCapacityWrapper<C> implements ErrorLogging {",
			"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
			"      super(capacity, caller);",
			"    }",
			"",
			"    public void error(final String text) {",
			"      try {",
			"        ensureCallerInLocalThread();",
			"        this.capacity.error(text);",
			"      } finally {",
			"        resetCallerInLocalThread();",
			"      }",
			"    }",
			"  }",
			"}",
			"");

	private final String JAVA_CODE_01_B = "package io.sarl.lang.tests.bug1092;"
			+ TestUtils.getLineSeparator() + TestUtils.getLineSeparator() + JAVA_CODE_BASE_B;

	@Test
	@DisplayName("Parsing code")
	@Tag("sarlParsing")
	public void parsing01() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_01);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling code to Logging")
	@Tag("compileToJava")
	public void compiling01() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1092.Logging");
			assertEquals(JAVA_CODE_01, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1092.Logging");
			assertNotNull(type);
		});
	}

	@Test
	@DisplayName("Compiling code to ErrorLogging")
	@Tag("compileToJava")
	public void compiling01b() throws Exception {
		getCompileHelper().compile(SARL_CODE_01, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug1092.ErrorLogging");
			assertEquals(JAVA_CODE_01_B, actual);
			final Class<?> type = it.getCompiledClass("io.sarl.lang.tests.bug1092.ErrorLogging");
			assertNotNull(type);
		});
	}

	private final String SARL_CODE_02 = SARL_CODE_BASE;

	private final String JAVA_CODE_02 = JAVA_CODE_BASE;
	
	private final String JAVA_CODE_02_B = JAVA_CODE_BASE_B;
	
	@Test
	@DisplayName("Parsing code without package")
	@Tag("sarlParsing")
	public void parsing02() throws Exception {
		SarlScript mas = file(getParseHelper(), SARL_CODE_02);
		final Validator validator = validate(getValidationHelper(), getInjector(), mas);
		validator.assertNoIssues();
	}

	@Test
	@DisplayName("Compiling code to Logging without package")
	@Tag("compileToJava")
	public void compiling02() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("Logging");
			assertEquals(JAVA_CODE_02, actual);
			final Class<?> type = it.getCompiledClass("Logging");
			assertNotNull(type);
		});
	}

	@Test
	@DisplayName("Compiling code to ErrorLogging without package")
	@Tag("compileToJava")
	public void compiling02b() throws Exception {
		getCompileHelper().compile(SARL_CODE_02, (it) -> {
			final String actual = it.getGeneratedCode("ErrorLogging");
			assertEquals(JAVA_CODE_02_B, actual);
			final Class<?> type = it.getCompiledClass("ErrorLogging");
			assertNotNull(type);
		});
	}

}
