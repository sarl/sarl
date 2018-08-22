/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.m2e.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import com.google.common.io.Files;
import org.apache.maven.artifact.Artifact;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.embedder.MavenModelManager;
import org.eclipse.m2e.core.project.AbstractProjectScanner;
import org.eclipse.m2e.core.project.IMavenProjectImportResult;
import org.eclipse.m2e.core.project.LocalProjectScanner;
import org.eclipse.m2e.core.project.MavenProjectInfo;
import org.eclipse.m2e.core.project.ProjectImportConfiguration;
import org.osgi.framework.Version;

import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.m2e.SARLMavenEclipsePlugin;


/**
 * M2E utilities.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class M2EUtilities {

	/** The qualifier string that is representing a snapshot version
	 * in the OGSi framework.
	 */
	public static final String SNAPSHOT_QUALIFIER = "qualifier"; //$NON-NLS-1$

	/** The pattern that is matching a SNAPSHOT version.
	 */
	public static final Pattern SNAPSHOT_VERSION_PATTERN = Pattern.compile("^(.*)" //$NON-NLS-1$
			+ Pattern.quote("-" + Artifact.SNAPSHOT_VERSION) //$NON-NLS-1$
			+ "$"); //$NON-NLS-1$

	private M2EUtilities() {
		//
	}

	/** Compare two OSGI versions.
	 *
	 * @param v1 first OSGI version.
	 * @param v2 second OSGI version.
	 * @return an integer that the sign indicates if v1 is lower, equal ot greater than v2.
	 */
	public static int compareOsgiVersions(String v1, String v2) {
		return Version.parseVersion(v1).compareTo(Version.parseVersion(v2));
	}

	/** Compare two Maven versions.
	 *
	 * @param v1 first Maven version.
	 * @param v2 second Maven version.
	 * @return an integer that the sign indicates if v1 is lower, equal ot greater than v2.
	 */
	public static int compareMavenVersions(String v1, String v2) {
		return parseMavenVersion(v1).compareTo(parseMavenVersion(v2));
	}

	/** Maven version parser.
	 *
	 * @param version the version string.
	 * @return the version.
	 */
	public static Version parseMavenVersion(String version) {
		if (Strings.isNullOrEmpty(version)) {
			return new Version(0, 0, 0);
		}

		// Detect the snapshot
		final boolean isSnapshot;
		final String coreVersion;
		Matcher matcher = Artifact.VERSION_FILE_PATTERN.matcher(version);
		if (matcher.matches()) {
			coreVersion = matcher.group(1);
			isSnapshot = true;
		} else {
			matcher = SNAPSHOT_VERSION_PATTERN.matcher(version);
			if (matcher.matches()) {
				coreVersion = matcher.group(1);
				isSnapshot = true;
			} else {
				coreVersion = version;
				isSnapshot = false;
			}
		}

		// Parse the numbers
		final String[] parts = coreVersion.split("[.]"); //$NON-NLS-1$
		final int[] numbers = new int[] {0, 0, 0};
		int i = 0;
		while (i < numbers.length && i < parts.length) {
			try {
				numbers[i] = Integer.parseInt(parts[i]);
				++i;
			} catch (Exception exception) {
				// Force the exit of the loop since a number cannot be find.
				i = numbers.length;
			}
		}
		// Reply
		if (isSnapshot) {
			return new Version(numbers[0], numbers[1], numbers[2], SNAPSHOT_QUALIFIER);
		}
		return new Version(numbers[0], numbers[1], numbers[2]);
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
	@SuppressWarnings("synthetic-access")
	public static void importMavenProject(IWorkspaceRoot workspaceRoot, String projectName,
			boolean addSarlSpecificSourceFolders, IProgressMonitor monitor) {
		// TODO: The m2e plugin seems to have an issue for creating a fresh project with the SARL plugin as an extension.
		// Solution: Create a simple project, and switch to a real SARL project.

		final WorkspaceJob bugFixJob = new WorkspaceJob("Creating Simple Maven project") { //$NON-NLS-1$
			@Override
			public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
			    try {
			    	forceSimplePom(workspaceRoot, projectName);
					final AbstractProjectScanner<MavenProjectInfo> scanner = getProjectScanner(workspaceRoot, projectName);
					scanner.run(monitor);
					final ProjectImportConfiguration importConfiguration = new ProjectImportConfiguration();
					final List<IMavenProjectImportResult> importResults = MavenPlugin.getProjectConfigurationManager()
							.importProjects(scanner.getProjects(), importConfiguration, null, monitor);
					if (addSarlSpecificSourceFolders) {
						for (final IMavenProjectImportResult importResult : importResults) {
							SARLProjectConfigurator.configureSARLSourceFolders(
									// Project to configure
									importResult.getProject(),
									// Create folders
									true,
									// Monitor
									monitor);

							final WorkspaceJob job = new WorkspaceJob("Creating Maven project") { //$NON-NLS-1$
								@Override
								public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
								    try {
								    	restorePom(importResult.getProject());
								    	MavenPlugin.getProjectConfigurationManager().updateProjectConfiguration(importResult.getProject(), monitor);
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
				} catch (Exception exception) {
					return SARLMavenEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
				}
				return Status.OK_STATUS;
			}
		};
	    bugFixJob.setRule(MavenPlugin.getProjectConfigurationManager().getRule());
	    bugFixJob.schedule();
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

	private static void restorePom(IProject project) throws Exception {
		final NullProgressMonitor monitor = new NullProgressMonitor();
		final IFile pomFile = project.getFile("pom.xml"); //$NON-NLS-1$
		final IFile savedPomFile = project.getFile("pom.xml.bak"); //$NON-NLS-1$
		pomFile.refreshLocal(IResource.DEPTH_ZERO, monitor);
		savedPomFile.refreshLocal(IResource.DEPTH_ZERO, monitor);
		if (savedPomFile.exists()) {
			if (pomFile.exists()) {
				pomFile.delete(true, false, monitor);
			}
			savedPomFile.copy(pomFile.getFullPath(), true, monitor);
			savedPomFile.delete(true, false, monitor);
		}
	}

	private static void forceSimplePom(IWorkspaceRoot workspaceRoot, String projectName) throws Exception {
		final File projectDir = new File(workspaceRoot.getLocation().toFile(), projectName);
		final File pomFile = new File(projectDir, "pom.xml"); //$NON-NLS-1$
		if (pomFile.exists()) {
			final File savedPomFile = new File(projectDir, "pom.xml.bak"); //$NON-NLS-1$
			if (savedPomFile.exists()) {
				savedPomFile.delete();
			}
			Files.copy(pomFile, savedPomFile);
			final StringBuilder content = new StringBuilder();
			try (BufferedReader stream = new BufferedReader(new FileReader(pomFile))) {
				String line = stream.readLine();
				while (line != null) {
					line = line.replaceAll("<extensions>\\s*true\\s*</extensions>", ""); //$NON-NLS-1$ //$NON-NLS-2$
					content.append(line).append("\n"); //$NON-NLS-1$
					line = stream.readLine();
				}
			}
			Files.write(content.toString().getBytes(), pomFile);
		}
	}

}
