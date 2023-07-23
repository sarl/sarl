/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
package io.sarl.lang.sarldoc.itests;

import static io.sarl.tests.api.tools.TestAssertions.assertContains;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.nio.file.Files;

import org.arakhne.afc.vmutil.FileSystem;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.docs.sarldoc.Main;
import io.sarl.tests.api.tools.TestAssertions;
import io.sarl.tests.api.tools.TestReflections;
import io.sarl.tests.api.tools.TestShell;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Sarldoc")
@Tag("sarldoc")
@Tag("run")
public class SarldocIT {
	
	private static File java;

	private static File cmd;
	
	@BeforeAll
	public static void setUp() throws Exception {
		java = TestShell.findExecutableJava();
		cmd = TestShell.findExecutableJar("sarldoc", "cli");
	}

	@Test
	@DisplayName("Generate help")
	public void runHelp() throws Exception {
		final String[] arguments = TestShell.mergeJarArguments(java.getAbsolutePath(), cmd.getAbsolutePath(), "--help");
		final String stdout = TestShell.run(arguments);
		TestAssertions.assertContains("Sarldoc is a documentation generator for the SARL language for generating", stdout);
	}

	@Test
	@DisplayName("Run assembly")
	public void runCompilerInProcess() throws Exception {
		final File tmpDir = FileSystem.createTempDirectory("sarldoctest", null);
		FileSystem.deleteOnExit(tmpDir);
		final File currentDir = new File(FileSystem.CURRENT_DIRECTORY).getAbsoluteFile();
		final File sourceCode = FileSystem.join(currentDir, "src", "it", "resources", "test02.sarl");
		final File srcDir = FileSystem.join(tmpDir, "src", "main", "sarl");
		final File srcFile = FileSystem.join(srcDir, "test02.sarl");
		srcFile.getParentFile().mkdirs();
		FileSystem.copy(sourceCode, srcFile);

		final String[] arguments = TestShell.mergeJarArguments(java.getAbsolutePath(), cmd.getAbsolutePath(), srcDir.getAbsolutePath());
		final String stdout = TestShell.run(arguments);

		assertContains("Success of documentation generation", stdout);

		final File htmlFile = FileSystem.join(tmpDir,
				"target", "sarl-api-docs",
				"io", "sarl", "lang", "sarldoc", "itests", "MyAgent.html");
		assertTrue(htmlFile.exists(), "No generated HTML file: " + htmlFile.getPath());
		final byte[] htmlRawContent = Files.readAllBytes(htmlFile.toPath());
		final String htmlContent = new String(htmlRawContent, "UTF-8");
		assertContains("agent <span class=\"doclet-typesignature-typename\">MyAgent</span>", htmlContent);
	}

	@Test
	@DisplayName("Run in current JRE")
	public void runCompilerInJRE() throws Exception {
		final File tmpDir = FileSystem.createTempDirectory("sarldoctest", null);
		FileSystem.deleteOnExit(tmpDir);
		final File currentDir = new File(FileSystem.CURRENT_DIRECTORY).getAbsoluteFile();
		final File sourceCode = FileSystem.join(currentDir, "src", "it", "resources", "test02.sarl");
		final File srcDir = FileSystem.join(tmpDir, "src", "main", "sarl");
		final File srcFile = FileSystem.join(srcDir, "test02.sarl");
		srcFile.getParentFile().mkdirs();
		FileSystem.copy(sourceCode, srcFile);

		final String stdout = TestReflections.runRun(Main.class.getName(), null, srcDir.getAbsolutePath());

		assertContains("Success of documentation generation", stdout);

		final File htmlFile = FileSystem.join(tmpDir,
				"target", "sarl-api-docs",
				"io", "sarl", "lang", "sarldoc", "itests", "MyAgent.html");
		assertTrue(htmlFile.exists(), "No generated HTML file: " + htmlFile.getPath());
		final byte[] htmlRawContent = Files.readAllBytes(htmlFile.toPath());
		final String htmlContent = new String(htmlRawContent, "UTF-8");
		assertContains("agent <span class=\"doclet-typesignature-typename\">MyAgent</span>", htmlContent);
	}

}
