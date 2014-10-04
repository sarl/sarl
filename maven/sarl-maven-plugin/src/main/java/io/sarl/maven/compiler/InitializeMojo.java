/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/** Initialization mojo for compiling SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @goal initialize
 * @phase initialize
 * @requiresDependencyResolution compile
 */
public class InitializeMojo extends AbstractSarlMojo {

	@Override
	public void execute() throws MojoExecutionException, MojoFailureException {
		for (File f : new File[] {this.input, this.output}) {
			String absPath = f.getAbsolutePath();
			if (!this.session.getCurrentProject().getCompileSourceRoots().contains(absPath)) {
				getLog().debug("*** SARL *** Adding SARL source folders: " + absPath); //$NON-NLS-1$
				this.session.getCurrentProject().addCompileSourceRoot(absPath);
			}
		}
		for (File f : new File[] {this.testInput, this.testOutput}) {
			String absPath = f.getAbsolutePath();
			if (!this.session.getCurrentProject().getTestCompileSourceRoots().contains(absPath)) {
				getLog().debug("*** SARL *** Adding SARL test source folders: " + absPath); //$NON-NLS-1$
				this.session.getCurrentProject().addTestCompileSourceRoot(absPath);
			}
		}
	}

}
