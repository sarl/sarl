/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.eclipse.m2e.sre;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.m2e.core.internal.IMavenConstants;
import org.eclipse.m2e.core.internal.MavenPluginActivator;

import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.apputils.eclipseextensions.sreprovider.ProjectSREProvider;
import io.sarl.apputils.eclipseextensions.sreprovider.ProjectSREProviderFactory;
import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.m2e.SARLMavenEclipsePlugin;
import io.sarl.eclipse.runtime.ManifestBasedSREInstall;
import io.sarl.eclipse.runtime.SARLRuntime;

/** Factory of project SRE provider for the Maven projects.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.m2e 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.m2e
 */
@SuppressWarnings("restriction")
public class MavenProjectSREProviderFactory implements ProjectSREProviderFactory {

	/** Construct the factory.
	 */
	public MavenProjectSREProviderFactory() {
		//
	}

	@Override
	public ProjectSREProvider getProjectSREProvider(IProject project) {
		try {
			if (project.hasNature(IMavenConstants.NATURE_ID)
					&& project.hasNature(JavaCore.NATURE_ID)
					&& project.hasNature(SARLEclipseConfig.NATURE_ID)) {
				final var facade = MavenPluginActivator.getDefault()
						.getMavenProjectManager().getProject(project);
				if (facade == null) {
					return null;
				}
				final var javaProject = JavaCore.create(project);
				final var classpath = javaProject.getResolvedClasspath(true);
				if (classpath == null) {
					return null;
				}
				for (final var dep : classpath) {
					final var depPath = dep.getPath();
					if (SARLRuntime.isPackedSRE(depPath)) {
						return new MavenProjectSREProvider(
								facade.getArtifactKey().toString()
								+ ":" + depPath.lastSegment(), //$NON-NLS-1$
								depPath);
					}
				}
			}
		} catch (CoreException e) {
			SARLMavenEclipsePlugin.getDefault().log(e);
		}
		return null;
	}

	/** Provider of SRE from a maven project.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse.m2e 0.15.1 20250911-224827
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse.m2e
	 */
	private static class MavenProjectSREProvider implements ProjectSREProvider {

		private final String id;

		private final IPath artifactFile;

		/** Constructor.
		 * @param id the identifier of the SRE.
		 * @param artifactFile the file of the artifact.
		 */
		MavenProjectSREProvider(String id, IPath artifactFile) {
			this.id = id;
			this.artifactFile = artifactFile;
		}

		@Override
		public boolean hasProjectSpecificSREConfiguration() {
			return true;
		}

		@Override
		public boolean isSystemSREUsed() {
			return false;
		}

		@Override
		public String getSREInstallIdentifier() {
			return this.id;
		}

		@Override
		public ISREInstall getProjectSREInstall() {
			final var tmpSre = new ManifestBasedSREInstall(this.id);
			tmpSre.setJarFile(this.artifactFile);
			return tmpSre;
		}

	}

}
