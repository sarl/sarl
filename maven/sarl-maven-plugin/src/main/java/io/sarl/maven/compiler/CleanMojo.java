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

import java.text.MessageFormat;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/** Cleaning mojo for compiling SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @goal clean
 * @phase clean
 * @requiresDependencyResolution compile
 */
public class CleanMojo extends AbstractSarlMojo {

	private static final String CLEAN_GROUP_ID = "org.apache.maven.plugins"; //$NON-NLS-1$
	private static final String CLEAN_ARTIFACT_ID = "maven-clean-plugin"; //$NON-NLS-1$
	private static final String CLEAN_MOJO = "clean"; //$NON-NLS-1$
	
	@Override
	public void execute() throws MojoExecutionException, MojoFailureException {
		String version = getPluginVersionFromDependencies(CLEAN_GROUP_ID, CLEAN_ARTIFACT_ID);
		executeDelegate(
				CLEAN_GROUP_ID, CLEAN_ARTIFACT_ID, version,
				CLEAN_MOJO,
				MessageFormat.format(
						getConfig(CLEAN_ARTIFACT_ID + ".configuration"), //$NON-NLS-1$
						this.output.getAbsolutePath(),
						this.testOutput.getAbsolutePath()));
	}

}
