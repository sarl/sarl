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

package io.sarl.eclipse.natures;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.base.Strings;
import com.google.inject.Key;
import com.google.inject.name.Names;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jdt.internal.ui.util.CoreUtility;
import org.eclipse.jdt.internal.ui.wizards.buildpaths.BuildPathsBlock;
import org.eclipse.jdt.internal.ui.wizards.buildpaths.CPListElement;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.internal.ide.StatusUtil;
import org.eclipse.ui.statushandlers.StatusManager;
import org.eclipse.ui.wizards.datatransfer.ProjectConfigurator;
import org.eclipse.xtext.Constants;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipseExecutableExtensionFactory;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.util.BundleUtil;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.ui.preferences.SARLPreferences;

/**
 * Configurator for a SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectConfigurator implements ProjectConfigurator {

	private final String fileExtension;

	private final Collection<String> candidates;

	/** Constructor.
	 */
	public SARLProjectConfigurator() {
		final String fileExtension = SARLEclipseExecutableExtensionFactory.getSARLInjector().getInstance(
				Key.get(String.class, Names.named(Constants.FILE_EXTENSIONS)));
		if (Strings.isNullOrEmpty(fileExtension)) {
			this.fileExtension = null;
		} else {
			this.fileExtension = "." + fileExtension; //$NON-NLS-1$
		}
		this.candidates = new ArrayList<>();
		this.candidates.addAll(Arrays.asList(BundleUtil.SRC_FOLDERS));
		this.candidates.add(SARLConfig.FOLDER_TEST_SOURCE_SARL);
	}

	@Override
	public Set<File> findConfigurableLocations(File root, IProgressMonitor monitor) {
		final Set<File> projectFolders = new LinkedHashSet<>();
		if (this.fileExtension != null) {
			final Set<String> visitedDirectories = new HashSet<>();
			collectProjectFoldersFromDirectory(projectFolders, root, visitedDirectories, true, monitor);
		}
		return projectFolders;
	}

	@Override
	public boolean shouldBeAnEclipseProject(IContainer container, IProgressMonitor monitor) {
		return container.getFile(new Path(IProjectDescription.DESCRIPTION_FILE_NAME)).exists();
	}

	@Override
	public Set<IFolder> getFoldersToIgnore(IProject project, IProgressMonitor monitor) {
		final Set<IFolder> ignoredFolders = new LinkedHashSet<>();
		for (final String binPath : BundleUtil.BIN_FOLDERS) {
			ignoredFolders.add(project.getFolder(Path.fromPortableString(binPath)));
		}
		ignoredFolders.add(project.getFolder(Path.fromPortableString(SARLConfig.FOLDER_RESOURCES)));
		ignoredFolders.add(project.getFolder(Path.fromPortableString(SARLConfig.FOLDER_SOURCE_GENERATED)));
		ignoredFolders.add(project.getFolder(Path.fromPortableString(SARLConfig.FOLDER_SOURCE_GENERATED_XTEXT)));
		ignoredFolders.add(project.getFolder(Path.fromPortableString(SARLConfig.FOLDER_TEST_SOURCE_GENERATED)));
		return ignoredFolders;
	}

	@Override
	public boolean canConfigure(IProject project, Set<IPath> ignoredPaths, IProgressMonitor monitor) {
		return true;
	}

	@Override
	public void configure(IProject project, Set<IPath> ignoredPaths, IProgressMonitor monitor) {
		configureSARLProject(project, true, monitor);
	}

	/** Configure the SARL project.
	 *
	 * @param project the project.
	 * @param configureJavaNature indicates if the Java configuration elements must be configured.
	 * @param monitor the monitor.
	 */
	public static void configureSARLProject(IProject project, boolean configureJavaNature, IProgressMonitor monitor) {
		try {
			project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
			// Add Natures
			final IStatus status = addSarlNatures(project);
			if (status != null && !status.isOK()) {
				SARLEclipsePlugin.getDefault().getLog().log(status);
			}

			// Ensure SARL specific folders.
			final IFolder sourceSarlFolder = ensureSourceFolder(project, SARLConfig.FOLDER_SOURCE_SARL, true, monitor);
			final IFolder sourceJavaFolder = ensureSourceFolder(project, SARLConfig.FOLDER_SOURCE_JAVA, false, monitor);
			final IFolder resourcesFolder = ensureSourceFolder(project, SARLConfig.FOLDER_RESOURCES, false, monitor);
			final IFolder testSourceSarlFolder = ensureSourceFolder(project,
					SARLConfig.FOLDER_TEST_SOURCE_SARL, false, monitor);
			final IFolder generationFolder = ensureGeneratedSourceFolder(project,
					SARLConfig.FOLDER_SOURCE_GENERATED, true, monitor);
			final IFolder testGenerationFolder = ensureGeneratedSourceFolder(project,
					SARLConfig.FOLDER_TEST_SOURCE_GENERATED, false, monitor);
			final IFolder outputFolder = ensureOutputFolder(project, SARLConfig.FOLDER_BIN, true, monitor);

			// SARL specific configuration
			SARLPreferences.setSpecificSARLConfigurationFor(project, generationFolder.getProjectRelativePath());

			// Create the Java project
			if (configureJavaNature) {
				final IJavaProject javaProject = JavaCore.create(project);

				// Build path
				BuildPathsBlock.flush(
						buildClassPathEntries(javaProject,
								new IFolder[] {sourceSarlFolder, sourceJavaFolder, resourcesFolder, testSourceSarlFolder},
								new IFolder[] {generationFolder, testGenerationFolder}),
						outputFolder.getFullPath(), javaProject, null, monitor);
			}

			project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
		} catch (CoreException exception) {
			SARLEclipsePlugin.getDefault().log(exception);
		}
	}

	/** Replies the default source entries for a SARL project.
	 *
	 * @param projectFolder the folder of the project.
	 * @return the classpath entries.
	 */
	public static List<IClasspathEntry> getDefaultSourceClassPathEntries(IPath projectFolder) {
		final IPath srcJava = projectFolder.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_JAVA));
		final IClasspathEntry srcJavaEntry = JavaCore.newSourceEntry(srcJava.makeAbsolute());

		final IPath srcSarl = projectFolder.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_SARL));
		final IClasspathEntry srcSarlEntry = JavaCore.newSourceEntry(srcSarl.makeAbsolute());

		final IPath srcGeneratedSources = projectFolder.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_GENERATED));
		final IClasspathAttribute attr = JavaCore.newClasspathAttribute(
				IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
				Boolean.TRUE.toString());
		final IClasspathEntry srcGeneratedSourcesEntry = JavaCore.newSourceEntry(
				srcGeneratedSources.makeAbsolute(),
				ClasspathEntry.INCLUDE_ALL,
				ClasspathEntry.EXCLUDE_NONE,
				null /*output location*/,
				new IClasspathAttribute[] {attr});

		final IPath srcResources = projectFolder.append(
				Path.fromPortableString(SARLConfig.FOLDER_RESOURCES));
		final IClasspathEntry srcResourcesEntry = JavaCore.newSourceEntry(srcResources.makeAbsolute());

		return Arrays.asList(
				srcSarlEntry,
				srcJavaEntry,
				srcResourcesEntry,
				srcGeneratedSourcesEntry);
	}

	private static List<CPListElement> buildClassPathEntries(IJavaProject project, IFolder[] sourcePaths,
			IFolder[] generationPaths) {
		final List<CPListElement> list = new ArrayList<>();

		for (final IFolder sourcePath : sourcePaths) {
			if (sourcePath != null) {
				list.add(new CPListElement(project, IClasspathEntry.CPE_SOURCE,
						sourcePath.getFullPath().makeAbsolute(), sourcePath));
			}
		}

		for (final IFolder sourcePath : generationPaths) {
			if (sourcePath != null) {
				final IClasspathAttribute attr = JavaCore.newClasspathAttribute(
						IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
						Boolean.TRUE.toString());
				final IClasspathEntry entry = JavaCore.newSourceEntry(
						sourcePath.getFullPath().makeAbsolute(),
						ClasspathEntry.INCLUDE_ALL,
						ClasspathEntry.EXCLUDE_NONE,
						null /*output location*/,
						new IClasspathAttribute[] {attr});
				list.add(CPListElement.create(entry, false, project));
			}
		}

		for (final IClasspathEntry current : PreferenceConstants.getDefaultJRELibrary()) {
			if (current != null) {
				list.add(CPListElement.create(current, true, project));
				break;
			}
		}

		list.add(CPListElement.create(
				JavaCore.newContainerEntry(SARLClasspathContainerInitializer.CONTAINER_ID),
				true, project));

		return list;
	}

	private static IFolder ensureGeneratedSourceFolder(IProject project, String folderPath, boolean force,
			IProgressMonitor monitor) throws CoreException {
		final IFolder folder = project.getFolder(Path.fromPortableString(folderPath));
		if (!folder.exists()) {
			if (force) {
				CoreUtility.createDerivedFolder(folder, true, true, monitor);
			} else {
				return null;
			}
		} else {
			folder.setDerived(true, monitor);
			folder.setHidden(true);
		}
		return folder;
	}

	private static IFolder ensureSourceFolder(IProject project, String folderPath, boolean force,
			IProgressMonitor monitor) throws CoreException {
		final IFolder folder = project.getFolder(Path.fromPortableString(folderPath));
		if (!folder.exists()) {
			if (force) {
				CoreUtility.createFolder(folder, true, true, monitor);
			} else {
				return null;
			}
		} else {
			folder.setDerived(false, monitor);
			folder.setHidden(false);
		}
		return folder;
	}

	private static IFolder ensureOutputFolder(IProject project, String folderPath, boolean force,
			IProgressMonitor monitor) throws CoreException {
		final IFolder folder = project.getFolder(Path.fromPortableString(folderPath));
		if (!folder.exists()) {
			if (force) {
				CoreUtility.createDerivedFolder(folder, true, true, monitor);
			} else {
				return null;
			}
		} else {
			folder.setDerived(true, monitor);
			folder.setHidden(true);
		}
		return folder;
	}

	/** Collect the list of SARL project folders that are under directory into files.
	 *
	 * @param folders the list of folders to fill in.
	 * @param directory the directory to explore.
	 * @param directoriesVisited Set of canonical paths of directories, used as recursion guard.
	 * @param nestedProjects whether to look for nested projects.
	 * @param monitor The monitor to report to.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	protected void collectProjectFoldersFromDirectory(Collection<File> folders, File directory,
			Set<String> directoriesVisited, boolean nestedProjects, IProgressMonitor monitor) {
		if (monitor.isCanceled()) {
			return;
		}
		monitor.subTask(NLS.bind(
				Messages.SARLProjectConfigurator_0,
				directory.getPath()));

		final File[] contents = directory.listFiles();
		if (contents == null) {
			return;
		}

		Set<String> visited = directoriesVisited;
		if (visited == null) {
			visited = new HashSet<>();
			try {
				visited.add(directory.getCanonicalPath());
			} catch (IOException exception) {
				StatusManager.getManager().handle(
						StatusUtil.newStatus(IStatus.ERROR, exception
								.getLocalizedMessage(), exception));
			}
		}

		final Set<File> subdirectories = new LinkedHashSet<>();

		for (final File file : contents) {
			if (file.isDirectory()) {
				subdirectories.add(file);
			} else if (file.getName().endsWith(this.fileExtension)) {
				// Found a SARL file.
				final File rootFile = getProjectFolderForSourceFolder(file.getParentFile());
				if (rootFile != null) {
					folders.add(rootFile);
					return;
				}
			}
		}

		for (final File subdir : subdirectories) {
			try {
				final String canonicalPath = subdir.getCanonicalPath();
				if (!visited.add(canonicalPath)) {
					// already been here --> do not recurse
					continue;
				}
			} catch (IOException exception) {
				StatusManager.getManager().handle(
						StatusUtil.newStatus(IStatus.ERROR, exception
								.getLocalizedMessage(), exception));

			}
			collectProjectFoldersFromDirectory(folders, subdir, visited, nestedProjects, monitor);
		}
	}

	private File getProjectFolderForSourceFolder(File file) {
		final IPath filePath = Path.fromOSString(file.getAbsolutePath());
		for (final String rawPath : this.candidates) {
			final IPath path = Path.fromPortableString(rawPath);

			IPath parent = null;
			IPath fp = (IPath) filePath.clone();
			while (fp != null) {
				if (path.isPrefixOf(fp)) {
					if (parent == null) {
						return file.getParentFile();
					}
					return parent.toFile();
				}
				if (parent == null) {
					parent = Path.fromPortableString("/" + fp.segment(0)); //$NON-NLS-1$
				} else {
					parent = parent.append(fp.segment(0));
				}
				if (fp.segmentCount() > 1) {
					fp = fp.removeFirstSegments(1);
				} else {
					fp = null;
				}
			}
		}
		return null;
	}

	/** Add the SARL natures to the given project.
	 *
	 * @param project the project.
	 * @return the status if the operation.
	 */
	public static IStatus addSarlNatures(IProject project) {
		if (project != null) {
			try {
				final IProjectDescription description = project.getDescription();
				final List<String> natures = new LinkedList<>(Arrays.asList(description.getNatureIds()));
				if (!natures.contains(JavaCore.NATURE_ID)) {
					natures.add(0, JavaCore.NATURE_ID);
				}
				if (!natures.contains(SARLEclipseConfig.XTEXT_NATURE_ID)) {
					natures.add(0, SARLEclipseConfig.XTEXT_NATURE_ID);
				}
				if (!natures.contains(SARLEclipseConfig.NATURE_ID)) {
					natures.add(0, SARLEclipseConfig.NATURE_ID);
				}

				final String[] newNatures = natures.toArray(new String[natures.size()]);
				final IStatus status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);

				// check the status and decide what to do
				if (status.getCode() == IStatus.OK) {
					description.setNatureIds(newNatures);
					final IProgressMonitor monitor = new NullProgressMonitor();
					project.setDescription(description, monitor);
				}
				return status;
			} catch (CoreException exception) {
				return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
			}
		}
		return SARLEclipsePlugin.getDefault().createOkStatus();
	}

}
