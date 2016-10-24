/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import java.io.File;
import java.nio.charset.Charset;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;
import com.google.common.io.Files;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.util.Strings;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlUiTest;

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
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.core.Agent;",
			"import io.sarl.lang.core.BuiltinCapacitiesProvider;",
			"import java.util.UUID;",
			"import javax.inject.Inject;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SuppressWarnings(\"all\")",
			"public class MyTestAgent extends Agent {",
			"  protected int oneFunction() {",
			"    return 1;",
			"  }",
			"  ",
			"  /**",
			"   * Construct an agent.",
			"   * @param builtinCapacityProvider - provider of the built-in capacities.",
			"   * @param parentID - identifier of the parent. It is the identifier of the parent agent and the enclosing contect, at the same time.",
			"   * @param agentID - identifier of the agent. If <code>null</code> the agent identifier will be computed randomly.",
			"   */",
			"  @Inject",
			"  @SyntheticMember",
			"  public MyTestAgent(final BuiltinCapacitiesProvider builtinCapacityProvider, final UUID parentID, final UUID agentID) {",
			"    super(builtinCapacityProvider, parentID, agentID);",
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
			assertEquals(JAVA_CODE, Strings.concat("\n", Files.readLines(javaFile, Charset.defaultCharset())));
		} finally {
			FileSystem.delete(tempDirectory);
		}
	}

}
