/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.buildpath;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ClasspathContainerInitializer;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;

import io.sarl.eclipse.SARLEclipsePlugin;

/** Initializer of the classpath container dedicated to the SARL environment.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
public class SARLClasspathContainerInitializer extends ClasspathContainerInitializer {

	/** Identifier of the container.
	 */
	public static final IPath CONTAINER_ID = new Path(SARLEclipsePlugin.PLUGIN_ID + ".launching.SARL_SUPPORT"); //$NON-NLS-1$

	@Override
	public void initialize(IPath containerPath, IJavaProject project)
			throws CoreException {
		if (CONTAINER_ID.equals(containerPath)) {
			final var container = new SARLClasspathContainer(containerPath, project);
			JavaCore.setClasspathContainer(containerPath,
					new IJavaProject[] {project},
					new IClasspathContainer[] {container},
					null);
		}
	}

	@Override
	public boolean canUpdateClasspathContainer(IPath containerPath, IJavaProject project) {
		// always ok to return classpath container
		return true;
	}

	@Override
	public void requestClasspathContainerUpdate(
			final IPath containerPath,
			final IJavaProject javaProject,
			final IClasspathContainer containerSuggestion) throws CoreException {
		if (containerSuggestion instanceof SARLClasspathContainer cvalue) {
			cvalue.reset();
		}
		super.requestClasspathContainerUpdate(containerPath, javaProject, containerSuggestion);
		final var job = new Job(Messages.SARLClasspathContainerInitializer_0) {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					JavaCore.setClasspathContainer(
							containerPath,
							new IJavaProject[] {javaProject},
							new IClasspathContainer[] {containerSuggestion},
							monitor);
				} catch (CoreException ex) {
					return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, ex);
				}
				return SARLEclipsePlugin.getDefault().createOkStatus();
			}
		};
		job.schedule();
	}

}
