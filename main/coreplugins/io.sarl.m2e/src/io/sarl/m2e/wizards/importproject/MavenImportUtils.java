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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.List;

import com.google.common.io.Files;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.embedder.MavenModelManager;
import org.eclipse.m2e.core.project.AbstractProjectScanner;
import org.eclipse.m2e.core.project.IMavenProjectImportResult;
import org.eclipse.m2e.core.project.IProjectCreationListener;
import org.eclipse.m2e.core.project.LocalProjectScanner;
import org.eclipse.m2e.core.project.MavenProjectInfo;
import org.eclipse.m2e.core.project.ProjectImportConfiguration;

import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.m2e.SARLMavenEclipsePlugin;

/**
 * Utilities for importing a Maven project with SARL plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public final class MavenImportUtils {

	private static final String POM_FILE = "pom.xml"; //$NON-NLS-1$

	private static final String POM_BACKUP_FILE = "pom.xml.forbackup"; //$NON-NLS-1$

	private MavenImportUtils() {
		//
	}

	/** Create a fresh Maven project from existing files, assuming a pom file exists.
	 *
	 * <p>If the Maven project is associated to an existing {@code IProject}, then the source folders
	 * must be configured before the call to this function.
	 *
	 * @param workspaceRoot the workspace root.
	 * @param projectName the name of the project that is also the name of the project's folder.
	 * @param addSarlSpecificSourceFolders indicates if the source folders that are specific to SARL should be added.
	 * @param monitor the progress monitor.
	 * @since 0.8
	 */
	public static void importMavenProject(IWorkspaceRoot workspaceRoot, String projectName,
			boolean addSarlSpecificSourceFolders, IProgressMonitor monitor) {
		// XXX: The m2e plugin seems to have an issue for creating a fresh project with the SARL plugin as an extension.
		// Solution: Create a simple project, and switch to a real SARL project.

		final WorkspaceJob bugFixJob = new WorkspaceJob("Creating Simple Maven project") { //$NON-NLS-1$
			@SuppressWarnings("synthetic-access")
			@Override
			public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
			    try {
			    	final SubMonitor submon = SubMonitor.convert(monitor, 3);
			    	forceSimplePom(workspaceRoot, projectName, submon.newChild(1));
					final AbstractProjectScanner<MavenProjectInfo> scanner = getProjectScanner(workspaceRoot, projectName);
					scanner.run(submon.newChild(1));
					final ProjectImportConfiguration importConfiguration = new ProjectImportConfiguration();
					runFixedImportJob(addSarlSpecificSourceFolders,
							scanner.getProjects(), importConfiguration,
							null, submon.newChild(1));
				} catch (Exception exception) {
					return SARLMavenEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
				}
				return Status.OK_STATUS;
			}
		};
	    bugFixJob.setRule(MavenPlugin.getProjectConfigurationManager().getRule());
	    bugFixJob.schedule();
	}

	/** Run the job that execute a fixed import of a Maven project.
	 *
	 * @param addSarlSpecificSourceFolders indicates if the source folders that are specific to SARL should be added.
	 * @param projectInfos the list of descriptions for each project to import.
	 * @param importConfiguration the import configuration.
	 * @param projectCreationListener the listener on the project creation. Could be {@code null}.
	 * @param monitor the progress monitor.
	 * @return the results of the importation tasks.
	 * @throws CoreException if it is impossible to do the import.
	 * @since 0.8
	 */
	static List<IMavenProjectImportResult> runFixedImportJob(boolean addSarlSpecificSourceFolders,
			Collection<MavenProjectInfo> projectInfos,
			ProjectImportConfiguration importConfiguration, IProjectCreationListener projectCreationListener,
			IProgressMonitor monitor) throws CoreException {
		final List<IMavenProjectImportResult> importResults = MavenPlugin.getProjectConfigurationManager()
				.importProjects(projectInfos, importConfiguration, projectCreationListener, monitor);
		if (addSarlSpecificSourceFolders) {
			final SubMonitor submon = SubMonitor.convert(monitor, importResults.size());
			for (final IMavenProjectImportResult importResult : importResults) {
				SARLProjectConfigurator.configureSARLSourceFolders(
						// Project to configure
						importResult.getProject(),
						// Create folders
						true,
						// Monitor
						submon.newChild(1));
				final WorkspaceJob job = new WorkspaceJob("Creating Maven project") { //$NON-NLS-1$
					@SuppressWarnings("synthetic-access")
					@Override
					public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
						try {
							runBugFix(importResult.getProject(), monitor);
						} catch (Exception exception) {
							return SARLMavenEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
						}
						return Status.OK_STATUS;
					}
				};
			    job.setRule(MavenPlugin.getProjectConfigurationManager().getRule());
				job.schedule();
			}
		}
		return importResults;
	}

	private static void runBugFix(IProject project, IProgressMonitor monitor) throws Exception {
		final SubMonitor submon = SubMonitor.convert(monitor, 2);
    	restorePom(project, submon);
    	MavenPlugin.getProjectConfigurationManager().updateProjectConfiguration(project, monitor);
	}

	private static AbstractProjectScanner<MavenProjectInfo> getProjectScanner(IWorkspaceRoot workspaceRoot, String projectName) {
		final File root = workspaceRoot.getLocation().toFile();
		final String projectPath = new File(root, projectName).getAbsolutePath();
		final MavenModelManager modelManager = MavenPlugin.getMavenModelManager();
		return new LocalProjectScanner(
				root,
				projectPath,
				false,
				modelManager);
	}

	private static void forceSimplePom(IWorkspaceRoot workspaceRoot, String projectName, IProgressMonitor monitor) throws Exception {
		final File projectDir = new File(workspaceRoot.getLocation().toFile(), projectName);
		forceSimplePom(projectDir, monitor);
	}

	/** Force the pom file of a project to be "simple".
	 *
	 * @param projectDir the folder in which the pom file is located.
	 * @param monitor the progress monitor.
	 * @throws IOException if the pom file cannot be changed.
	 */
	static void forceSimplePom(File projectDir, IProgressMonitor monitor) throws IOException {
		final File pomFile = new File(projectDir, POM_FILE);
		if (pomFile.exists()) {
			final SubMonitor submon = SubMonitor.convert(monitor, 4);
			final File savedPomFile = new File(projectDir, POM_BACKUP_FILE);
			if (savedPomFile.exists()) {
				savedPomFile.delete();
			}
			submon.worked(1);
			Files.copy(pomFile, savedPomFile);
			submon.worked(1);
			final StringBuilder content = new StringBuilder();
			try (BufferedReader stream = new BufferedReader(new FileReader(pomFile))) {
				String line = stream.readLine();
				while (line != null) {
					line = line.replaceAll("<extensions>\\s*true\\s*</extensions>", ""); //$NON-NLS-1$ //$NON-NLS-2$
					content.append(line).append("\n"); //$NON-NLS-1$
					line = stream.readLine();
				}
			}
			submon.worked(1);
			Files.write(content.toString().getBytes(), pomFile);
			submon.worked(1);
		}
	}

	/** Restore the original pom file.
	 *
	 * @param project the project in which the pom file is located.
	 * @param monitor the progress monitor.
	 * @throws CoreException if the pom file cannot be changed.
	 */
	static void restorePom(IProject project, IProgressMonitor monitor) throws CoreException {
		final IFile pomFile = project.getFile(POM_FILE);
		final IFile savedPomFile = project.getFile(POM_BACKUP_FILE);
		pomFile.refreshLocal(IResource.DEPTH_ZERO, monitor);
		savedPomFile.refreshLocal(IResource.DEPTH_ZERO, monitor);
		monitor.worked(1);
		if (savedPomFile.exists()) {
			final SubMonitor submon = SubMonitor.convert(monitor, 3);
			if (pomFile.exists()) {
				pomFile.delete(true, false, submon);
			} else {
				submon.worked(1);
			}
			savedPomFile.copy(pomFile.getFullPath(), true, submon);
			savedPomFile.delete(true, false, monitor);
			submon.worked(1);
		} else {
			monitor.worked(1);
		}
	}

	/** Restore the original pom file.
	 *
	 * @param projectDir the folder in which the pom file is located.
	 * @param monitor the progress monitor.
	 * @throws IOException if the pom file cannot be changed.
	 */
	static void restorePom(File projectDir, IProgressMonitor monitor) throws IOException {
		final File pomFile = new File(projectDir, POM_FILE);
		final File savedPomFile = new File(projectDir, POM_BACKUP_FILE);
		if (savedPomFile.exists()) {
			if (pomFile.exists()) {
				pomFile.delete();
			}
			Files.copy(savedPomFile, pomFile);
			savedPomFile.delete();
		}
		monitor.worked(1);
	}

}
