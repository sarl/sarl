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

import java.text.MessageFormat;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

/** Cleaning mojo for compiling SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Mojo(name = "clean", defaultPhase = LifecyclePhase.CLEAN,
		requiresDependencyResolution = ResolutionScope.COMPILE)
public class CleanMojo extends AbstractSarlMojo {

	@Override
	protected boolean isSkipped() {
		// Check the general SARL clean skipping flag
		boolean mavenCleanSkip = false;
		try {
			mavenCleanSkip = Boolean.parseBoolean(this.session.getUserProperties().getProperty(SARL_CLEAN_SKIP_NAME, "false")); //$NON-NLS-1$
		} catch (Throwable exception) {
			mavenCleanSkip = false;
		}
		return mavenCleanSkip || super.isSkipped();
	}

	@Override
	protected void executeMojo() throws MojoExecutionException, MojoFailureException, DependencyResolutionRequiredException {
		if (!isSkipped()) {
			final String cleanerGroupId = this.mavenHelper.getConfig("cleaner.groupId"); //$NON-NLS-1$
			final String cleanerArtifactId = this.mavenHelper.getConfig("cleaner.artifactId"); //$NON-NLS-1$
			final String cleanerVersion = this.mavenHelper.getPluginDependencyVersion(cleanerGroupId, cleanerArtifactId);
			final String cleanerMojo = this.mavenHelper.getConfig("cleaner.mojo"); //$NON-NLS-1$
			executeMojo(
					cleanerGroupId, cleanerArtifactId, cleanerVersion, cleanerMojo,
					MessageFormat.format(
					this.mavenHelper.getConfig("cleaner.configuration"), //$NON-NLS-1$
					getOutput().getAbsolutePath(),
					getTestOutput().getAbsolutePath()));
		}
	}

}
