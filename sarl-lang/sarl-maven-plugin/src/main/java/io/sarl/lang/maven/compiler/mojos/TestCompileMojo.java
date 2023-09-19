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

package io.sarl.lang.maven.compiler.mojos;

import java.io.File;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.slf4j.Logger;

import io.sarl.lang.maven.compiler.abstractmojos.AbstractCompileMojo;

/** Mojo for compiling SARL.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarl-maven-plugin 0.13.0 20230919-093056
 * @mavengroupid io.sarl.lang
 * @mavenartifactid sarl-maven-plugin
 */
@Mojo(name = "testCompile", defaultPhase = LifecyclePhase.TEST_COMPILE,
		requiresDependencyResolution = ResolutionScope.TEST)
public class TestCompileMojo extends AbstractCompileMojo {

	@Override
	protected boolean isTestContext() {
		return true;
	}

	private boolean isValidSourceDirectory(File file, File outputDirectory) {
		return !file.equals(outputDirectory) && !file.equals(getInput());
	}

	private static List<String> getSourceRoots(MavenProject project) {
		return project.getTestCompileSourceRoots();
	}

	@Override
	protected void compileSARL() throws MojoExecutionException, MojoFailureException {
		final Logger log = getLogger();
		File outputDirectory = getTestOutput();
		log.info(Messages.TestCompileMojo_0);
		// If output is not explicitly set try to read SARL prefs from eclipse .settings folder
		if (getDefaultTestOutput().equals(getTestOutput())) {
			final String settingsValue = readSarlEclipseSetting(getProject().getBuild().getTestSourceDirectory());
			if (settingsValue != null && !settingsValue.isEmpty()) {
				outputDirectory = new File(settingsValue);
				log.info(MessageFormat.format(Messages.TestCompileMojo_1, outputDirectory));
			}
		}
		final MavenProject project = getProject();
		final List<File> compileSourceRoots = new ArrayList<>();
		for (final String filename : getSourceRoots(project)) {
			final File file = new File(filename);
			if (isValidSourceDirectory(file, outputDirectory)) {
				compileSourceRoots.add(file);
			}
		}
		final List<File> classPath = getTestClassPath();
		final List<File> modulePath = getTestModulePath();
		project.addTestCompileSourceRoot(outputDirectory.getAbsolutePath());
		compile(classPath, modulePath, compileSourceRoots, outputDirectory,
				makeAbsolute(new File(getProject().getBuild().getTestOutputDirectory())));
	}

}
