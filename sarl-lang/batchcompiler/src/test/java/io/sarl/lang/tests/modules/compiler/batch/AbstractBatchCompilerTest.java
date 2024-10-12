/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
package io.sarl.lang.tests.modules.compiler.batch;

import static io.sarl.tests.api.tools.TestUtils.getLineSeparator;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.google.common.io.Files;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractBatchCompilerTest extends AbstractSarlTest {

	private static final String VALID_SARL_CODE = multilineString(
			"package io.sarl.lang.tests.compiler.batch",
			"agent MyTestAgent {",
			"  def oneFunction : int {",
			"    1",
			"  }",
			"}"
			);

	private static final String VALID_JAVA_CODE = multilineString(
			"package io.sarl.lang.tests.compiler.batch;",
			"",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.DynamicSkillProvider;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
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

	public abstract boolean runBatchCompiler(File basePath, File sourcePath, File sarlcOutputFolder,
			File javacOutputFolder, File tempFolder, List<Issue> issues);

	protected File makeFolder(File root, String... elements) {
		File output = root;
		for (final String element : elements) {
			output = new File(output, element);
		}
		return output;
	}
	
	private interface Callback {

		void apply(File sarlcOutputDirectory, List<Issue> issues) throws Exception;

	}

	private void runCompiler(String testId, String sarlCode, boolean expectedFailure, Callback callback) throws Exception {
		File tempDirectory = FileSystem.createTempDirectory("sarltests_" + getClass().getSimpleName()+ "_" + testId + "_", "tmp");
		try {
			// Create folders
			File sourceDirectory = new File(tempDirectory, "src");
			sourceDirectory.mkdirs();
			File sarlcOutputDirectory = new File(tempDirectory, "src-gen");
			sarlcOutputDirectory.mkdirs();
			File buildDirectory = new File(tempDirectory, "build");
			buildDirectory.mkdirs();
			File javacOutputDirectory = new File(tempDirectory, "bin");
			javacOutputDirectory.mkdirs();
			// Create source file
			File sarlFile = new File(sourceDirectory, "test.sarl");
			Files.write(sarlCode.getBytes(), sarlFile);
			// Compile
			final List<Issue> issues = new ArrayList();
			final boolean result = runBatchCompiler(tempDirectory, sourceDirectory, sarlcOutputDirectory, javacOutputDirectory, buildDirectory, issues);
			assertEquals(expectedFailure, !result);
			callback.apply(sarlcOutputDirectory, issues);
		} finally {
			FileSystem.delete(tempDirectory);
		}
	}
	
	@Test
	@DisplayName("Valid SARL code")
	public void testValidCompilation() throws Exception {
		runCompiler("v", VALID_SARL_CODE, false, (sarlcOutputDirectory, issues) -> {
			// Check result
			File javaFile = makeFolder(sarlcOutputDirectory, "io", "sarl", "lang", "tests", "compiler", "batch", "MyTestAgent.java");
			assertEquals(VALID_JAVA_CODE, Strings.concat(getLineSeparator(), Files.readLines(javaFile, Charset.defaultCharset())));
			// Check issues
			final List<Issue> errors = issues.stream().filter(it -> it.getSeverity() == Severity.ERROR).collect(Collectors.toList());
			if (!errors.isEmpty()) {
				final StringBuilder builder = new StringBuilder();
				for (Issue issue : errors) {
					builder.append("Code: ").append(issue.getCode()).append("\n");
					builder.append("Message: ").append(issue.getMessage()).append("\n");
					builder.append("Line: ").append(issue.getLineNumber()).append("\n");
					builder.append("Column: ").append(issue.getColumn()).append("\n\n");
				}
				fail("Unexpected errors:\n" + builder.toString());
			}
		});
	}

	@Test
	@DisplayName("Invalid SARL code")
	public void testInvalidCompilation() throws Exception {
		runCompiler("i", INVALID_SARL_CODE, true, (sarlcOutputDirectory, issues) -> {
			assertEquals(1, issues.size());
			assertEquals(1, issues.stream().filter(it -> it.getSeverity() == Severity.ERROR).count(), issues.toString());
		});
	}

}
