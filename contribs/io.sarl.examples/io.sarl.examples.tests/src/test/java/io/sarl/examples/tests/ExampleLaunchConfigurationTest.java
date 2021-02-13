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

package io.sarl.examples.tests;

import static io.sarl.examples.tests.utils.ExamplesTestUtils.assertFile;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.compileFiles;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.compileMaven;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.createProject;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.dynamicTests;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.getSourceGenPath;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.isMavenProject;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.LAUNCH_PROPERTY_FILE;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.readLaunchConfigurationFromXml;
import static io.sarl.examples.wizard.XmlUtils.readXmlContent;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.arakhne.afc.vmutil.FileSystem;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.w3c.dom.Document;

import io.sarl.examples.tests.utils.AbstractExampleTest;

/** Class for testing the examples.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisabledOnOs(OS.WINDOWS)
@SuppressWarnings("all")
@DisplayName("Example launch configurations")
@Tag("examples")
public class ExampleLaunchConfigurationTest extends AbstractExampleTest {

	/** Replies the dynamics tests for examples' launch configurations.
	 *
	 * @return the dynamic tests.
	 * @throws Exception in case of error for recovering the example descriptions.
	 */
	@TestFactory
	@DisplayName("Definition of launch configuration")
	public List<DynamicTest> launchConfiguration() throws Exception {
		return dynamicTests(example -> {
			final File projectRoot = createProject(); 
			final List<File> installedFiles = installFiles(example, projectRoot, true);
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
				final String filename = type.replaceAll(
						Pattern.quote("."), Matcher.quoteReplacement(File.separator))
					.concat(".java");
				File file = FileSystem.convertStringToFile(filename);
				file = FileSystem.join(root, file);
				assertFile(file, projectRoot);
			});
		});
	}

}
