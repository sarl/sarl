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

package io.sarl.eclipse.examples.tests;

import static io.sarl.eclipse.examples.tests.utils.ExamplesTestUtils.assertFile;
import static io.sarl.eclipse.examples.tests.utils.ExamplesTestUtils.compileFiles;
import static io.sarl.eclipse.examples.tests.utils.ExamplesTestUtils.compileMaven;
import static io.sarl.eclipse.examples.tests.utils.ExamplesTestUtils.createProject;
import static io.sarl.eclipse.examples.tests.utils.ExamplesTestUtils.dynamicTests;
import static io.sarl.eclipse.examples.tests.utils.ExamplesTestUtils.getSourceGenPath;
import static io.sarl.eclipse.examples.tests.utils.ExamplesTestUtils.installFiles;
import static io.sarl.eclipse.examples.tests.utils.ExamplesTestUtils.isMavenProject;
import static io.sarl.eclipse.examples.wizard.SarlExampleLaunchConfiguration.LAUNCH_PROPERTY_FILE;
import static io.sarl.eclipse.examples.wizard.SarlExampleLaunchConfiguration.readLaunchConfigurationFromXml;
import static io.sarl.eclipse.examples.wizard.XmlUtils.readXmlContent;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.arakhne.afc.vmutil.FileSystem;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.w3c.dom.Document;

/** Class for testing the examples.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.examples.tests 0.13.0 20230919-093100
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.examples.tests
 */
@DisabledOnOs(OS.WINDOWS)
@SuppressWarnings("all")
@DisplayName("Example launching definition")
@Tag("examples")
public class ExampleLaunchConfigurationTest {

	/** Replies the dynamics tests for examples' launch configurations.
	 *
	 * <p>The example folder could contain a file {@code launch.xml} for the definition of the
	 * example's launching in Eclipse IDE.
	 * Verify if the qualified name of the agent that is specified in the file corresponds
	 * to a valid agent in the example's code.
	 * The qualified name of the agent is given by:
	 * <pre>{@code 
	 * &lt;launchConfigurations&gt;
	 *   &lt;agent class="AGENT_QUALIFIED_NAME" /&gt;
	 * &lt;/launchConfigurations&gt;
	 * }</pre>
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
