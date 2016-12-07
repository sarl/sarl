/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.janusproject.eclipse.buildpath;

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

import io.janusproject.eclipse.JanusEclipsePlugin;

/** Initializer of the classpath container dedicated to the Janus platform.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusClasspathContainerInitializer extends ClasspathContainerInitializer {

	/** Identifier of the container.
	 */
	public static final IPath CONTAINER_ID = new Path(JanusEclipsePlugin.PLUGIN_ID + ".launching.JANUS_SUPPORT"); //$NON-NLS-1$

	@Override
	public void initialize(IPath containerPath, IJavaProject project)
			throws CoreException {
		if (CONTAINER_ID.equals(containerPath)) {
			final IClasspathContainer container = new JanusClasspathContainer(containerPath);
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
		if (containerSuggestion instanceof JanusClasspathContainer) {
			((JanusClasspathContainer) containerSuggestion).reset();
		}
		super.requestClasspathContainerUpdate(containerPath, javaProject, containerSuggestion);
		final Job job = new Job(Messages.JanusClasspathContainerInitializer_0) {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					JavaCore.setClasspathContainer(
							containerPath,
							new IJavaProject[] {javaProject},
							new IClasspathContainer[] {containerSuggestion},
							monitor);
				} catch (CoreException ex) {
					return JanusEclipsePlugin.getDefault().createStatus(IStatus.ERROR, ex);
				}
				return JanusEclipsePlugin.getDefault().createOkStatus();
			}
		};
		job.schedule();
	}

}
