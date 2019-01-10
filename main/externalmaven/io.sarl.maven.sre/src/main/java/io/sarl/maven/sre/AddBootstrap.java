/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.maven.sre;

import java.io.File;
import java.io.FileWriter;

import com.google.common.base.Strings;
import org.apache.maven.model.Build;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.arakhne.afc.vmutil.FileSystem;

import io.sarl.eclipse.runtime.SREConstants;

/** Add the bootstrap service.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Mojo(name = "addbootstrap", requiresProject = true, defaultPhase = LifecyclePhase.PREPARE_PACKAGE,
		requiresDependencyResolution = ResolutionScope.COMPILE)
public class AddBootstrap extends AbstractSREMojo {

	@Override
	protected void executeMojo() throws MojoExecutionException, MojoFailureException {
		final String bootstrap = getBootstrap();
		if (Strings.isNullOrEmpty(bootstrap)) {
			getLog().warn("No SRE bootstrap service to register"); //$NON-NLS-1$
		} else {
			getLog().info("Registering the SRE bootstrap service"); //$NON-NLS-1$
			final Build build = getMavenProject().getBuild();
			final File baseDir = FileSystem.convertStringToFile(build.getOutputDirectory());
			final File localFile = FileSystem.convertStringToFile(SREConstants.SERVICE_SRE_BOOTSTRAP);
			final File serviceFile = FileSystem.makeAbsolute(localFile, baseDir);
			try {
				serviceFile.getParentFile().mkdirs();
				try (FileWriter fw = new FileWriter(serviceFile)) {
					fw.write(bootstrap);
					fw.write("\n"); //$NON-NLS-1$
				}
			} catch (Exception exception) {
				throw new MojoExecutionException(exception.getLocalizedMessage(), exception);
			}
		}
	}

}
