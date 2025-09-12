/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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
import java.util.TreeSet;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

import io.sarl.lang.maven.compiler.abstractmojos.AbstractCompileMojo;

/** Mojo for compiling SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Mojo(name = "compile", defaultPhase = LifecyclePhase.COMPILE,
		requiresDependencyResolution = ResolutionScope.COMPILE)
public class CompileMojo extends AbstractCompileMojo {

	@Override
	protected boolean isTestContext() {
		return false;
	}

	private boolean isValidSourceDirectory(File file, File outputDirectory) {
		return !file.equals(outputDirectory) && !file.equals(getTestInput()) && !file.equals(getIntegrationTestInput());
	}

	private static List<String> getSourceRoots(MavenProject project) {
		final var testRoots = new TreeSet<String>();
		for (final var root : project.getTestCompileSourceRoots()) {
			testRoots.add(root);
		}
		final var roots = new ArrayList<String>();
		for (final var root : project.getCompileSourceRoots()) {
			if (!testRoots.contains(root)) {
				roots.add(root);
			}
		}
		return roots;
	}

	@Override
	protected void compileSARL() throws MojoExecutionException, MojoFailureException {
		final var log = getLogger();
		var outputDirectory = getOutput();
		log.info(Messages.CompileMojo_0);
		if (log.isDebugEnabled()) {
			final var properties = new StringBuilder();
			buildPropertyString(properties);
			log.debug(properties.toString());
		}
		// If output is not explicitly set try to read SARL prefs from eclipse .settings folder
		if (getDefaultOutput().equals(getOutput())) {
			final var settingsValue = readSarlEclipseSetting(getProject().getBuild().getSourceDirectory());
			if (settingsValue != null && !settingsValue.isEmpty()) {
				outputDirectory = new File(settingsValue);
				log.info(MessageFormat.format(Messages.CompileMojo_1, outputDirectory));
			}
		}
		final var project = getProject();
		final var compileSourceRoots = new ArrayList<File>();
		for (final var filename : getSourceRoots(project)) {
			final var file = new File(filename);
			if (isValidSourceDirectory(file, outputDirectory)) {
				compileSourceRoots.add(file);
			}
		}
		final var classPath = getClassPath();
		final var modulePath = getModulePath();
		project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
		compile(classPath, modulePath, compileSourceRoots, outputDirectory,
				makeAbsolute(new File(getProject().getBuild().getOutputDirectory())));
	}

}
