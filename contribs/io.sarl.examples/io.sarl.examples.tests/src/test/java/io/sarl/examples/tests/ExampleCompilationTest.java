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

import static io.sarl.examples.tests.utils.ExamplesTestUtils.assertNoIssue;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.compileFiles;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.compileMaven;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.createProject;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.dynamicTests;
import static io.sarl.examples.tests.utils.ExamplesTestUtils.isMavenProject;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.util.List;

import org.eclipse.xtext.util.Strings;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

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
@DisplayName("Example compilation")
@Tag("examples")
public class ExampleCompilationTest extends AbstractExampleTest {

	/** Replies the dynamics tests for compiling the examples.
	 *
	 * @return the dynamic tests.
	 * @throws Exception in case of error for recovering the example descriptions.
	 */
	@TestFactory
	@DisplayName("SARL compilation")
	public List<DynamicTest> compilation() throws Exception {
		return dynamicTests(example -> {
			final File projectRoot = createProject();
			final List<File> installedFiles = installFiles(example, projectRoot, true);
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

}
