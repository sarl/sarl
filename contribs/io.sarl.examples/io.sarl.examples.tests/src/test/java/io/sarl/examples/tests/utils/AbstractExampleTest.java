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

package io.sarl.examples.tests.utils;

import static io.sarl.examples.tests.utils.ExamplesTestUtils.copySourceFiles;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.isMavenProject;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.preparePomFileForTest;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.unpackFiles;
import static io.sarl.tests.api.tools.TestUtils.isEclipseRuntimeEnvironment;

import java.io.File;
import java.util.List;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.opentest4j.TestAbortedException;

/** Class for testing the examples.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@SuppressWarnings("all")
@DisplayName("Testing all the SARL examples")
@Tag("examples")
public abstract class AbstractExampleTest {

	/** Install the files of the example in order to be compiled.
	 * 
	 * @param example is the description of the example.
	 * @param projectRoot is the directory in which the files must be installed.
	 * @param skipIfInvalidPom is {@code true} to avoid to check the pom file.
	 * @return the list of the installed files.
	 * @throws Exception if an error occurs during the installation.
	 */
	protected static List<File> installFiles(ExampleDescription example, File projectRoot, boolean skipIfInvalidPom) throws Exception {
		// The behavior is different in Eclipse Junit and Maven surefire.
		final List<File> installedFiles;
		if (isEclipseRuntimeEnvironment()) {
			if (skipIfInvalidPom && isMavenProject(example.sourceFolder)) {
				// Pom file is not valid because Maven has not yet applied the macro replacements into the file.
				throw new TestAbortedException();
			}
			installedFiles = copySourceFiles(projectRoot, example.sourceFolder);
		} else {
			installedFiles = unpackFiles(projectRoot, example.archive);
		}
		preparePomFileForTest(projectRoot);
		return installedFiles;
	}

}
