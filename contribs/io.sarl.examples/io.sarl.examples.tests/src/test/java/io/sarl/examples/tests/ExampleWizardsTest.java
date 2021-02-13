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

import static io.sarl.examples.tests.utils.ExamplesTestUtils.DEFAULT_RELATIVE_PATH;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.createProject;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.dynamicTests;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.readWizardClassesFromXml;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.readXmlNode;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.LAUNCH_PROPERTY_FILE;
import static io.sarl.examples.wizard.XmlUtils.readXmlAttribute;
import static io.sarl.examples.wizard.XmlUtils.readXmlContent;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.util.List;

import org.eclipse.xtext.util.Strings;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.TestFactory;
import org.opentest4j.AssertionFailedError;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import io.sarl.examples.SARLExampleExecutableExtensionFactory;
import io.sarl.examples.tests.utils.AbstractExampleTest;

/** Class for testing the examples.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Example wizards")
@Tag("examples")
public class ExampleWizardsTest extends AbstractExampleTest {

	/** Replies the dynamics tests for example wizard injection.
	 *
	 * @return the dynamic tests.
	 * @throws Exception in case of error for recovering the plugin's description.
	 * @since 0.11
	 */
	@TestFactory
	@DisplayName("Definition of example wizards")
	public List<DynamicTest> exampleWizards() throws Exception {
		final File pluginFile = new File(DEFAULT_RELATIVE_PATH, "plugin.xml");
		final Document document = readXmlContent(pluginFile);
		assertNotNull(document, "Cannot read XML from the plugin.xml file");
		final String prefix = SARLExampleExecutableExtensionFactory.class.getName() + ":";
		return dynamicTests(false, example -> {
			final File projectRoot = createProject(); 
			final List<File> installedFiles = installFiles(example, projectRoot, false);
			final File launchConfiguration = new File(projectRoot, LAUNCH_PROPERTY_FILE);
			assumeTrue(launchConfiguration.exists());

			final Document launchDocument = readXmlContent(launchConfiguration);
			assertNotNull(launchDocument, "Expecting the example identifier into the launch.xml file");
			final Node configurationNode = readXmlNode(launchDocument, "launchConfigurations");
			assertNotNull(configurationNode, "Expecting the example identifier into the launch.xml file");
			final String exampleId = readXmlAttribute(configurationNode, "id");
			assertFalse(Strings.isEmpty(exampleId), "Expecting the example identifier into the launch.xml file");

			final List<String> classNames = readWizardClassesFromXml(document, exampleId);
			boolean foundOne = false;
			for (final String className : classNames) {
				assertNotNull(className, "Unexpected null value for a wizard class name of " + exampleId);
				if (!className.startsWith(prefix)) {
					throw new AssertionFailedError("Wizard class of " + exampleId + " must be loaded with the extension factory",
							prefix + "package.classname", className);
				}
				foundOne = true;
			}

			assertTrue(foundOne, "Expecting definition of a wizard for the example " + exampleId);
		});
	}

}
