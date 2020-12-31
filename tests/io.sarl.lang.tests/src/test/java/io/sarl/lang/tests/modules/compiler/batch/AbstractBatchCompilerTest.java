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

import java.io.File;
import java.nio.charset.Charset;

import com.google.common.io.Files;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.util.Strings;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public abstract class AbstractBatchCompilerTest extends AbstractSarlTest {

	private static final String SARL_CODE = multilineString(
			"package io.sarl.lang.tests.compiler.batch",
			"agent MyTestAgent {",
			"  def oneFunction : int {",
			"    1",
			"  }",
			"}"
			);

	private static final String JAVA_CODE = multilineString(
			"package io.sarl.lang.tests.compiler.batch;",
			"",
			"import io.sarl.lang.annotation.SarlElementType;",
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

	public abstract void runBatchCompiler(File basePath, File sourcePath, File sarlcOutputFolder,
			File javacOutputFolder, File tempFolder) throws Exception;

	protected File makeFolder(File root, String... elements) {
		File output = root;
		for (final String element : elements) {
			output = new File(output, element);
		}
		return output;
	}

	@Test
	public void testCompilation() throws Exception {
		File tempDirectory = FileSystem.createTempDirectory("sarltests_" + getClass().getSimpleName()+ "_", "tmp");
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
			Files.write(SARL_CODE.getBytes(), sarlFile);
			// Compile
			runBatchCompiler(tempDirectory, sourceDirectory, sarlcOutputDirectory, javacOutputDirectory, buildDirectory);
			// Check result
			File javaFile = makeFolder(sarlcOutputDirectory, "io", "sarl", "lang", "tests", "compiler", "batch", "MyTestAgent.java");
			assertEquals(JAVA_CODE, Strings.concat(getLineSeparator(), Files.readLines(javaFile, Charset.defaultCharset())));
		} finally {
			FileSystem.delete(tempDirectory);
		}
	}

}
