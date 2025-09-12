/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.tests.maven.compiler;

import static io.sarl.tests.api.tools.TestUtils.getLineSeparator;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.charset.Charset;

import com.google.common.io.Files;
import org.eclipse.xtext.util.Strings;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("SARL Maven Compiler")
@Tag("core")
@Tag("compiler-run")
@DisabledOnOs(value = OS.WINDOWS)
public class SarlMavenCompilerIT extends AbstractSarlMavenCompilerIT {

	private static final String VALID_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.compiler.maven",
			"agent MyTestAgent {",
			"  def oneFunction : int {",
			"    1",
			"  }",
			"}"
			);

	private static final String VALID_JAVA_CODE = multilineString(
			"package io.sarl.lang.tests.compiler.maven;",
			"",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import jakarta.inject.Inject;",
			"import java.util.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class MyTestAgent extends Agent {",
			"  @Pure",
			"  protected int oneFunction() {",
			"    return 1;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public MyTestAgent(final UUID arg0, final UUID arg1) {",
			"    super(arg0, arg1);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  @Inject",
			"  public MyTestAgent(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
			"    super(arg0, arg1, arg2);",
			"  }",
			"}"
			);

	private static final String INVALID_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.compiler.batch",
			"agent MyTestAgent {",
			"  xyz",
			"}"
			);

	@Test
	@DisplayName("Valid SARL code")
	public void testValidCompilation() throws Exception {
		runCompiler("v", VALID_SARL_CODE, true, (sarlcOutputDirectory, errorStream) -> {
			// Check result
			File javaFile = makeFolder(sarlcOutputDirectory, "io", "sarl", "lang", "tests", "compiler", "maven", "MyTestAgent.java");
			assertEquals(VALID_JAVA_CODE, Strings.concat(getLineSeparator(), Files.readLines(javaFile, Charset.defaultCharset())));
			// Check errors
			assertTrue(errorStream.isEmpty());
		});
	}

	@Test
	@DisplayName("Invalid SARL code")
	public void testInvalidCompilation() throws Exception {
		runCompiler("i", INVALID_SARL_CODE, false, (sarlcOutputDirectory, errorStream) -> {
			// No generated file
			File javaFile = makeFolder(sarlcOutputDirectory, "io", "sarl", "lang", "tests", "compiler", "maven", "MyTestAgent.java");
			assertFalse(javaFile.exists(), "Expecting not to generate the Java file from SARL code");
		});
	}

}
