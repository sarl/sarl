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

package io.sarl.maven.compiler;

import java.io.File;
import java.text.MessageFormat;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

/** Initialization mojo for the SARL Maven compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Mojo(name = "initialize", defaultPhase = LifecyclePhase.INITIALIZE,
		requiresDependencyResolution = ResolutionScope.COMPILE)
public class InitializeMojo extends AbstractSarlMojo {

	@Override
	protected void executeMojo() throws MojoExecutionException, MojoFailureException, DependencyResolutionRequiredException {
		if (!isSkipped()) {
			final MavenProject project = this.mavenHelper.getSession().getCurrentProject();
			for (final File f : new File[] {getInput(), getOutput()}) {
				final String absPath = f.getAbsolutePath();
				getLog().info(MessageFormat.format(Messages.InitializeMojo_0, absPath));
				project.addCompileSourceRoot(absPath);
			}
			for (final File f : new File[] {getTestInput(), getTestOutput()}) {
				final String absPath = f.getAbsolutePath();
				getLog().info(MessageFormat.format(Messages.InitializeMojo_1, absPath));
				project.addTestCompileSourceRoot(absPath);
			}
		}
	}

}

