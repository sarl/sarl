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

package io.sarl.m2e.wizards.importproject;

import java.io.File;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.m2e.core.internal.IMavenConstants;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.m2e.SARLMavenEclipsePlugin;

/**
 * Action for enabling a maven-based SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class EnableSarlMavenNatureAction implements IObjectActionDelegate, IExecutableExtension {

	/** Identifier of the action.
	 */
	public static final String ID = "io.sarl.m2e.enableSarlMavenNatureAction"; //$NON-NLS-1$

	private static final String FILENAME_CLASSPATH = ".classpath"; //$NON-NLS-1$

	private ISelection selection;

	/** Constructor.
	 */
	public EnableSarlMavenNatureAction() {
	}

	/** Constructor.
	 *
	 * @param option the options of the action.
	 */
	public EnableSarlMavenNatureAction(String option) {
		setInitializationData(null, null, option);
	}

	@Override
	public void setInitializationData(IConfigurationElement config, String propertyName, Object data) {
		//
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		this.selection = selection;
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		//
	}

	@Override
	public void run(IAction action) {
		if (this.selection instanceof IStructuredSelection) {
			final IStructuredSelection structuredSelection = (IStructuredSelection) this.selection;
			final Iterator<?> it = structuredSelection.iterator();
			while (it.hasNext()) {
				final Object element = it.next();
				IProject project = null;
				if (element instanceof IProject) {
					project = (IProject) element;
				} else if (element instanceof IAdaptable) {
					project = ((IAdaptable) element).getAdapter(IProject.class);
				}
				if (project != null) {
					enableNature(project);
				}
			}
		}
	}

	/** Enable the SARL Maven nature.
	 *
	 * @param project the project.
	 */
	protected void enableNature(IProject project) {
		final IFile pom = project.getFile(IMavenConstants.POM_FILE_NAME);
		final Job job;
		if (pom.exists()) {
			job = createJobForMavenProject(project);
		} else {
			job = createJobForJavaProject(project);
		}
		if (job != null) {
			job.schedule();
		}
	}

	/** Create the configuration job for a Maven project.
	 *
	 * @param project the project to configure.
	 * @return the job.
	 */
	@SuppressWarnings("static-method")
	protected Job createJobForMavenProject(IProject project) {
		return new Job(Messages.EnableSarlMavenNatureAction_0) {

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				final SubMonitor mon = SubMonitor.convert(monitor, 3);
				try {
					// The project should be a Maven project.
					final IPath descriptionFilename = project.getFile(new Path(IProjectDescription.DESCRIPTION_FILE_NAME)).getLocation();
					final File projectDescriptionFile = descriptionFilename.toFile();
					final IPath classpathFilename = project.getFile(new Path(FILENAME_CLASSPATH)).getLocation();
					final File classpathFile = classpathFilename.toFile();
					// Project was open by the super class. Close it because Maven fails when a project already exists.
					project.close(mon.newChild(1));
					// Delete the Eclipse project and classpath definitions because Maven fails when a project already exists.
					project.delete(false, true, mon.newChild(1));
					if (projectDescriptionFile.exists()) {
						projectDescriptionFile.delete();
					}
					if (classpathFile.exists()) {
						classpathFile.delete();
					}
					// Import
					MavenImportUtils.importMavenProject(
							project.getWorkspace().getRoot(),
							project.getName(),
							true,
							mon.newChild(1));
				} catch (CoreException exception) {
					SARLMavenEclipsePlugin.getDefault().log(exception);
				}
				return Status.OK_STATUS;
			}
		};
	}

	/** Create the configuration job for a Java project.
	 *
	 * @param project the project to configure.
	 * @return the job.
	 */
	@SuppressWarnings("static-method")
	protected Job createJobForJavaProject(IProject project) {
		return new Job(Messages.EnableSarlMavenNatureAction_0) {

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				final SubMonitor mon = SubMonitor.convert(monitor, 3);
				// Force the project configuration to SARL.
				SARLProjectConfigurator.configureSARLProject(
						// Project to configure
						project,
						// Add SARL natures
						true,
						// Force java configuration
						true,
						// Create folders
						true,
						// Monitor
						mon.newChild(3));
				return Status.OK_STATUS;
			}
		};
	}

}
