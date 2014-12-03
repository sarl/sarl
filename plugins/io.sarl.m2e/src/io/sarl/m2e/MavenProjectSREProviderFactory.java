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
package io.sarl.m2e;

import io.sarl.eclipse.SARLConfig;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.ProjectSREProvider;
import io.sarl.eclipse.runtime.ProjectSREProviderFactory;
import io.sarl.eclipse.runtime.SARLRuntime;
import io.sarl.eclipse.runtime.StandardSREInstall;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.m2e.core.internal.IMavenConstants;
import org.eclipse.m2e.core.internal.MavenPluginActivator;
import org.eclipse.m2e.core.project.IMavenProjectFacade;

/** Factory of project SRE provider for the Maven projects.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MavenProjectSREProviderFactory implements ProjectSREProviderFactory {

	/**
	 */
	public MavenProjectSREProviderFactory() {
		//
	}

	@Override
	public ProjectSREProvider getProjectSREProvider(IProject project) {
		try {
			if (project.hasNature(IMavenConstants.NATURE_ID)
					&& project.hasNature(JavaCore.NATURE_ID)
					&& project.hasNature(SARLConfig.NATURE_ID)) {
				IMavenProjectFacade facade = MavenPluginActivator.getDefault()
						.getMavenProjectManager().getProject(project);
				if (facade == null) {
					return null;
				}
				IJavaProject javaProject = JavaCore.create(project);
				IClasspathEntry[] classpath = javaProject.getResolvedClasspath(true);
				if (classpath == null) {
					return null;
				}
				for (IClasspathEntry dep : classpath) {
					IPath depPath = dep.getPath();
					if (SARLRuntime.isPackedSRE(depPath)) {
						return new MavenProjectSREProvider(
								facade.getArtifactKey().toString()
								+ ":" + depPath.lastSegment(), //$NON-NLS-1$
								depPath);
					}
				}
			}
		} catch (CoreException e) {
			SARLMavenEclipsePlugin.log(e);
		}
		return null;
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class MavenProjectSREProvider implements ProjectSREProvider {

		private final String id;
		private final IPath artifactFile;

		/**
		 * @param id - the identifier of the SRE.
		 * @param artifactFile - the file of the artifact.
		 */
		public MavenProjectSREProvider(String id, IPath artifactFile) {
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
			StandardSREInstall tmpSre = new StandardSREInstall(this.id);
			tmpSre.setJarFile(this.artifactFile);
			return tmpSre;
		}

	}

}
