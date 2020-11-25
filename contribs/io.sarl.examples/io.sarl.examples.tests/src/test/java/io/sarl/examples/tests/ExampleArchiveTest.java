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

import static io.sarl.examples.tests.utils.ExamplesTestUtils.DEFAULT_RELATIVE_PATH;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.assertFile;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.assertNoIssue;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.compileFiles;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.compileMaven;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.copySourceFiles;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.createProject;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.dynamicTests;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.getSourceGenPath;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.getSourcePath;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.isMavenProject;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.preparePomFileForTest;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.readFileToOpenFromXml;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.readWizardClassesFromXml;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.readXmlNode;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.unpackFiles;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.LAUNCH_PROPERTY_FILE;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.readLaunchConfigurationFromXml;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.readXmlAttribute;
import static io.sarl.examples.wizard.SarlExampleLaunchConfiguration.readXmlContent;
import static io.sarl.tests.api.tools.TestUtils.isEclipseRuntimeEnvironment;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.File;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.util.Strings;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.TestFactory;
import org.opentest4j.AssertionFailedError;
import org.opentest4j.TestAbortedException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import io.sarl.examples.SARLExampleExecutableExtensionFactory;
import io.sarl.examples.tests.utils.ExampleDescription;

/** Class for testing the examples.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Example archive tests")
@Tag("examples")
public class ExampleArchiveTest {

	/** Replies the dynamics tests for the examples' paths.
	 *
	 * @return the dynamic tests.
	 * @throws Exception in case of error for recovering the example descriptions.
	 */
	@TestFactory
	@DisplayName("Path checking")
	public List<DynamicTest> path() throws Exception {
		return dynamicTests(false, example -> {
				assertNotNull(example.archive);
		});
	}

}
