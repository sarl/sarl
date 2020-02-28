/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2020 the original authors or authors.
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

package io.sarl.examples.tests;

import static io.sarl.examples.tests.ExamplesTestUtils.DEFAULT_RELATIVE_PATH;
import static io.sarl.examples.tests.ExamplesTestUtils.assertNoIssue;
import static io.sarl.examples.tests.ExamplesTestUtils.compileFiles;
import static io.sarl.examples.tests.ExamplesTestUtils.compileMaven;
import static io.sarl.examples.tests.ExamplesTestUtils.createProject;
import static io.sarl.examples.tests.ExamplesTestUtils.dynamicTests;
import static io.sarl.examples.tests.ExamplesTestUtils.getSarlBatchCompiler;
import static io.sarl.examples.tests.ExamplesTestUtils.getSourceGenPath;
import static io.sarl.examples.tests.ExamplesTestUtils.getSourcePath;
import static io.sarl.examples.tests.ExamplesTestUtils.isMavenProject;
import static io.sarl.examples.tests.ExamplesTestUtils.readExtensionPointFromXml;
import static io.sarl.examples.tests.ExamplesTestUtils.readXmlNode;
import static io.sarl.examples.tests.ExamplesTestUtils.unpackFiles;
import static io.sarl.examples.tests.ExamplesTestUtils.assertFile;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.LAUNCH_PROPERTY_FILE;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.readLaunchConfigurationFromXml;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.readXmlAttribute;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.readXmlContent;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.util.List;
import java.util.stream.Stream;

import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.util.Strings;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.util.OutParameter;

/** Class for testing the examples.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Testing all the SARL examples")
public class ExamplesTest {

	/** Replies the dynamics tests for the examples' paths.
	 *
	 * @return the dynamic tests.
	 * @throws Exception in case of error for recovering the example descriptions.
	 */
	@TestFactory
	@DisplayName("Archive existence")
	public Stream<DynamicTest> path() throws Exception {
		return dynamicTests(false, example -> {
				assertNotNull(example.archive);
		});
	}
	
	/** Replies the dynamics tests for compiling the examples.
	 *
	 * @return the dynamic tests.
	 * @throws Exception in case of error for recovering the example descriptions.
	 */
	@TestFactory
	@DisplayName("SARL Compilation")
	public Stream<DynamicTest> compilation() throws Exception {
		return dynamicTests(example -> {
			final File projectRoot = createProject(); 
			final List<File> installedFiles = unpackFiles(projectRoot, example.archive);
			assertFalse(installedFiles.isEmpty(), () -> "No installed file in " + projectRoot);
			if (isMavenProject(example.sourceFolder)) {
				// Maven compilation
				final String errors = compileMaven(projectRoot);
				assertTrue(Strings.isEmpty(errors), errors);
			} else {
				// Standard SARL compilation
				List<String> issues = compileFiles(projectRoot, installedFiles);
				assertNoIssue(issues);
			}
		});
	}

	/** Replies the dynamics tests for examples' launch configurations.
	 *
	 * @return the dynamic tests.
	 * @throws Exception in case of error for recovering the example descriptions.
	 */
	@TestFactory
	@DisplayName("Definition of launch configuration")
	public Stream<DynamicTest> launchConfiguration() throws Exception {
		return dynamicTests(example -> {
			final File projectRoot = createProject(); 
			final List<File> installedFiles = unpackFiles(projectRoot, example.archive);
			final File launchConfiguration = new File(projectRoot, LAUNCH_PROPERTY_FILE);
			assumeTrue(launchConfiguration.exists());

			final File relativeLaunchConfiguration = FileSystem.makeRelative(launchConfiguration, projectRoot);
			assertTrue(installedFiles.contains(relativeLaunchConfiguration));

			final File folder;

			if (isMavenProject(example.sourceFolder)) {
				// Maven compilation
				compileMaven(projectRoot);
				folder = FileSystem.join(projectRoot, "src", "main", "generated-sources", "sarl");
			} else {
				// Standard SARL compilation
				compileFiles(projectRoot, installedFiles);
				folder = getSourceGenPath(projectRoot);
			}

			final Document launch = readXmlContent(launchConfiguration);

			readLaunchConfigurationFromXml(launch, folder, (type, name0, isAgent, root) -> {
				final String filename = type.replaceAll("\\.", File.separator).concat(".java");
				File file = FileSystem.convertStringToFile(filename);
				file = FileSystem.join(root, file);
				assertFile(file);
			});
		});
	}

	/** Replies the dynamics tests for examples's files to open in the editor.
	 *
	 * @return the dynamic tests.
	 * @throws Exception in case of error for recovering the example descriptions.
	 */
	@TestFactory
	@DisplayName("Definition of file to open")
	public Stream<DynamicTest> fileToOpenInEditor() throws Exception {
		return dynamicTests(example -> {
			final File projectRoot = createProject(); 
			final List<File> installedFiles = unpackFiles(projectRoot, example.archive);
			final File pluginFile = new File(DEFAULT_RELATIVE_PATH, "plugin.xml");
			final Document document = readXmlContent(pluginFile);
			Node node = readXmlNode(document, "plugin");
			node = readExtensionPointFromXml(node, example.archive);
			assumeTrue(node != null);
			
			String locationStr = readXmlAttribute(node, "location");
			assertNotNull(locationStr);
			IPath location = Path.fromPortableString(locationStr);
		
			// Format 1: <project-name>/<source-folder>/<qualified-filename>
			IPath filePath = location.removeFirstSegments(1);
			File file = FileSystem.join(projectRoot, filePath.toFile());
			if (file == null || !file.exists()) {
				// Format 2: <source-folder>/<qualified-filename>
				file = FileSystem.join(projectRoot, location.toFile());
				if (file == null || !file.exists()) {
					// Format 3: <qualified-filename>
					final File folder = getSourcePath(projectRoot);
					file = FileSystem.join(folder, location.toFile());
					assertFile(file);
				}
			}
		});
	}

}
