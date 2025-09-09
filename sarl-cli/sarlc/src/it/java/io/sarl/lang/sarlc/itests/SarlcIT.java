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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.sarlc.itests;

import static io.sarl.tests.api.tools.TestAssertions.assertContains;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.file.Files;

import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarlc.Main;
import io.sarl.lang.sarlc.configs.subconfigs.JavaCompiler;
import io.sarl.tests.api.tools.TestAssertions;
import io.sarl.tests.api.tools.TestReflections;
import io.sarl.tests.api.tools.TestShell;

/**
 * @author $Author: sgalland$
 * @version sarlc 0.15.0 20250909-115750
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarlc
 */
@SuppressWarnings("all")
@DisplayName("Run sarlc")
@Tag("sarlc")
@Tag("run")
public class SarlcIT {

	@Test
	@DisplayName("Generate help")
	public void runHelp() throws Exception {
		String stdout = TestReflections.runRun(Main.class.getName(), null, "--help");
		TestAssertions.assertContains("The sarlc command reads definitions, written in the SARL programming", stdout);
	}

	@Test
	@DisplayName("Verify Java compiler type")
	public void javaCompilerType() throws Exception {
		assertSame(JavaCompiler.JAVAC, JavaCompiler.getDefault());
	}

	private void runCompilationTest(Invoker invoker) throws Exception {
		final File tmpDir = FileSystem.createTempDirectory("sarlctest", null);
		FileSystem.deleteOnExit(tmpDir);
		final File currentDir = new File(FileSystem.CURRENT_DIRECTORY).getAbsoluteFile();
		final File sourceCode = FileSystem.join(currentDir, "src", "it", "resources", "test01.sarl");
		final File srcDir = FileSystem.join(tmpDir, "src", "main", "sarl");
		final File srcFile = FileSystem.join(srcDir, "test01.sarl");
		srcFile.getParentFile().mkdirs();
		FileSystem.copy(sourceCode, srcFile);

		String stdout = invoker.run(srcDir);
		assertContains("1 files compiled", stdout);

		final File javaFile = FileSystem.join(tmpDir,
				"src", "main", "generated-sources", "sarl",
				"io", "sarl", "lang", "sarlc", "itests", "MyAgent.java");
		assertTrue(javaFile.exists(), "No generated Java file: " + javaFile.getPath());
		final byte[] javaRawContent = Files.readAllBytes(javaFile.toPath());
		final String javaContent = new String(javaRawContent, "UTF-8");
		assertContains("public class MyAgent extends Agent", javaContent);
		
		final File classFile = FileSystem.join(tmpDir,
				"target", "classes",
				"io", "sarl", "lang", "sarlc", "itests", "MyAgent.class");
		assertTrue(classFile.exists(), "No generated Class file: " + classFile.getPath());
	}

	@Test
	@DisplayName("Compiler in Maven project")
	public void runCompilerInJRE() throws Exception {
		runCompilationTest(srcDir -> TestReflections.runRun(Main.class.getName(), null, srcDir.getAbsolutePath()));
	}

	@Test
	@DisplayName("Compiler as assembly")
	public void runCompilerInProcess() throws Exception {
		final File java = TestShell.findExecutableJava();
		final File cmd = TestShell.findExecutableJar("sarlc", "cli");
		runCompilationTest(srcDir -> {
			final String[] arguments = TestShell.mergeJarArguments(java.getAbsolutePath(), cmd.getAbsolutePath(), srcDir.getAbsolutePath());
			return TestShell.run(arguments);
		});
	}

	@FunctionalInterface
	private interface Invoker {

		String run(File sourceDir) throws Exception;

	}

}
