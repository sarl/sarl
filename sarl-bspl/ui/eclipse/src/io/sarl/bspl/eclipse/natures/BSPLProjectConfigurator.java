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

package io.sarl.bspl.eclipse.natures;

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
import java.util.TreeSet;

import com.google.common.base.Strings;
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
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jdt.internal.ui.util.CoreUtility;
import org.eclipse.jdt.internal.ui.wizards.buildpaths.BuildPathsBlock;
import org.eclipse.jdt.internal.ui.wizards.buildpaths.CPListElement;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.internal.ide.StatusUtil;
import org.eclipse.ui.statushandlers.StatusManager;
import org.eclipse.ui.wizards.datatransfer.ProjectConfigurator;

import io.sarl.bspl.eclipse.BSPLEclipseConfig;
import io.sarl.bspl.eclipse.BSPLEclipseExecutableExtensionFactory;
import io.sarl.bspl.eclipse.BSPLEclipsePlugin;
import io.sarl.bspl.eclipse.util.BundleUtil;
import io.sarl.bspl.lang.BSPLConfig;
import io.sarl.bspl.lang.ui.preferences.BSPLPreferences;
import io.sarl.lang.core.util.OutParameter;

/**
 * Configurator for a project with BSPL nature.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("restriction")
public class BSPLProjectConfigurator implements ProjectConfigurator, IProjectUnconfigurator {

	private final String fileExtension;

	private final Collection<String> candidates;

	/** Constructor.
	 */
	public BSPLProjectConfigurator() {
		final var injector = BSPLEclipseExecutableExtensionFactory.getBSPLInjector();
		final var fileExtension = BSPLEclipseConfig.BSPL_FILE_EXTENSION;
		if (!fileExtension.startsWith(".")) { //$NON-NLS-1$
			this.fileExtension = "." + fileExtension; //$NON-NLS-1$
		} else {
			this.fileExtension = fileExtension;
		}
		injector.injectMembers(this);
		this.candidates = new ArrayList<>();
		this.candidates.addAll(Arrays.asList(BundleUtil.SRC_FOLDERS));
	}

	@Override
	public Set<File> findConfigurableLocations(File root, IProgressMonitor monitor) {
		final var projectFolders = new LinkedHashSet<File>();
		if (this.fileExtension != null) {
			final var visitedDirectories = new HashSet<String>();
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
		final var ignoredFolders = new LinkedHashSet<IFolder>();
		ignoredFolders.add(project.getFolder(Path.fromPortableString(BSPLConfig.FOLDER_SOURCE_GENERATED)));
		return ignoredFolders;
	}

	@Override
	public boolean canConfigure(IProject project, Set<IPath> ignoredPaths, IProgressMonitor monitor) {
		try {
			return project != null && !project.hasNature(BSPLEclipseConfig.NATURE_ID);
		} catch (CoreException exception) {
			return false;
		} finally {
			monitor.done();
		}
	}

	@Override
	public void configure(IProject project, Set<IPath> ignoredPaths, IProgressMonitor monitor) {
		try {
			configureBSPLProject(project, true, true, true, monitor);
			safeRefresh(project, monitor);
		} finally {
			monitor.done();
		}
	}

	/** Refresh the project file hierarchy.
	 *
	 * @param project the project.
	 * @param monitor the progress monitor.
	 */
	@SuppressWarnings("static-method")
	protected void safeRefresh(IProject project, IProgressMonitor monitor) {
		try {
			project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
		} catch (CoreException exception) {
			BSPLEclipsePlugin.getDefault().log(exception);
		}
	}

	/** Replies the list of the standard source folders for a BSPL project.
	 *
	 * @return the list of the standard source folders.
	 */
	public static String[] getBSPLProjectSourceFolders() {
		return new String[] {
			BSPLConfig.FOLDER_SOURCE_BSPL,
			BSPLConfig.FOLDER_SOURCE_GENERATED,
		};
	}

	/** Replies the list of the standard binary folders for a BSPL project.
	 *
	 * @return the list of the standard binary folders.
	 */
	public static String[] getBSPLProjectBinFolders() {
		return new String[] {
		};
	}

	/** Configure the BSPL project.
	 *
	 * <p>This function does the following:<ul>
	 * <li>Add the BSPL nature to the project;</li>
	 * <li>Create the standard BSPL folders if {@code createFolders} is evaluated to true;</li>
	 * <li>Set the output configuration of the project from the
	 * {@link BSPLPreferences#setSpecificBSPLConfigurationFor(IProject, IPath) general BSPL configuration};</li>
	 * <li>Reset the Java configuration in order to follow the BSPL configuration, if {@code configureJabvaNature}
	 * is evaluated to true.</li>
	 * </ul>
	 *
	 * @param project the project.
	 * @param addNatures indicates if the natures must be added to the project by calling {@link #addBsplNatures(IProject, IProgressMonitor)}.
	 * @param configureJavaNature indicates if the Java configuration elements must be configured.
	 * @param createFolders indicates if the folders must be created or not.
	 * @param monitor the monitor.
	 * @see #addBsplBsplNatures(IProject, IProgressMonitor)
	 * @see #configureBSPLSourceFolders(IProject, boolean, IProgressMonitor)
	 */
	public static void configureBSPLProject(IProject project, boolean addNatures,
			boolean configureJavaNature, boolean createFolders, IProgressMonitor monitor) {
		try {
			final var subMonitor = SubMonitor.convert(monitor, 11);
			// Add Natures
			final var status = Status.OK_STATUS;
			if (addNatures) {
				addBsplNatures(project, subMonitor.newChild(1));
				if (status != null && !status.isOK()) {
					BSPLEclipsePlugin.getDefault().getLog().log(status);
				}
			}

			// Ensure BSPL specific folders.
			final var sourceFolders = new OutParameter<IFolder[]>();
			final var generationFolders = new OutParameter<IFolder[]>();
			final var generationFolder = new OutParameter<IFolder>();
			ensureSourceFolders(project, createFolders, subMonitor,
					sourceFolders, generationFolders, generationFolder);

			// BSPL specific configuration
			BSPLPreferences.setSpecificBSPLConfigurationFor(project,
					generationFolder.get().getProjectRelativePath());
			subMonitor.worked(1);

			// Create the Java project
			if (configureJavaNature) {

				if (!addNatures) {
					addNatures(project, subMonitor.newChild(1), JavaCore.NATURE_ID);
				}

				final var javaProject = JavaCore.create(project);
				subMonitor.worked(1);

				// Build path
				BuildPathsBlock.flush(
						buildClassPathEntries(javaProject,
								sourceFolders.get(),
								generationFolders.get(),
								false, true),
						null, javaProject, null, subMonitor.newChild(1));
			}
			subMonitor.done();
		} catch (CoreException exception) {
			BSPLEclipsePlugin.getDefault().log(exception);
		}
	}

	private static void ensureSourceFolders(IProject project, boolean createFolders, SubMonitor monitor,
			OutParameter<IFolder[]> sourcePaths,
			OutParameter<IFolder[]> generationPaths,
			OutParameter<IFolder> standardGenerationFolder) throws CoreException {
		final var sourceBsplFolder = ensureSourceFolder(project,
				BSPLConfig.FOLDER_SOURCE_BSPL, true, createFolders, monitor.newChild(1));
		final var generationFolder = ensureGeneratedSourceFolder(project,
				BSPLConfig.FOLDER_SOURCE_GENERATED, true, createFolders, monitor.newChild(1));
		if (sourcePaths != null) {
			assert sourceBsplFolder != null : "sourceBsplFolder must not be null"; //$NON-NLS-1$
			sourcePaths.set(new IFolder[] {sourceBsplFolder});
		}
		if (generationPaths != null) {
			assert generationFolder != null : "generationFolder must not be null"; //$NON-NLS-1$
			generationPaths.set(new IFolder[] {generationFolder});
		}
		if (standardGenerationFolder != null) {
			assert generationFolder != null : "generationFolder must not be null"; //$NON-NLS-1$
			standardGenerationFolder.set(generationFolder);
		}
	}

	/** Configure the source folders for a BSPL project.
	 *
	 *
	 * @param project the project.
	 * @param createFolders indicates if the folders must be created or not.
	 * @param monitor the monitor.
	 * @since 0.8
	 * @see #addBsplNatures(IProject, IProgressMonitor)
	 * @see #configureBSPLProject(IProject, boolean, boolean, boolean, IProgressMonitor)
	 */
	public static void configureBSPLSourceFolders(IProject project, boolean createFolders, IProgressMonitor monitor) {
		try {
			final var subMonitor = SubMonitor.convert(monitor, 8);

			final var sourceFolders = new OutParameter<IFolder[]>();
			final var generationFolders = new OutParameter<IFolder[]>();
			ensureSourceFolders(project, createFolders, subMonitor,
					sourceFolders, generationFolders, null);

			final var javaProject = JavaCore.create(project);
			subMonitor.worked(1);

			// Build path
			BuildPathsBlock.flush(
					buildClassPathEntries(javaProject,
							sourceFolders.get(),
							generationFolders.get(),
							true, false),
					javaProject.getOutputLocation(), javaProject, null, subMonitor.newChild(1));

			subMonitor.done();
		} catch (CoreException exception) {
			BSPLEclipsePlugin.getDefault().log(exception);
		}
	}

	/** Replies the default source entries for a BSPL project.
	 *
	 * @param projectFolder the folder of the project.
	 * @return the classpath entries.
	 */
	public static List<IClasspathEntry> getDefaultSourceClassPathEntries(IPath projectFolder) {
		final var srcBspl = projectFolder.append(
				Path.fromPortableString(BSPLConfig.FOLDER_SOURCE_BSPL));
		final var srcBsplEntry = JavaCore.newSourceEntry(srcBspl.makeAbsolute());

		final var srcGeneratedSources = projectFolder.append(
				Path.fromPortableString(BSPLConfig.FOLDER_SOURCE_GENERATED));
		final var attr = JavaCore.newClasspathAttribute(
				IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
				Boolean.TRUE.toString());
		final var srcGeneratedSourcesEntry = JavaCore.newSourceEntry(
				srcGeneratedSources.makeAbsolute(),
				ClasspathEntry.INCLUDE_ALL,
				ClasspathEntry.EXCLUDE_NONE,
				null /*output location*/,
				new IClasspathAttribute[] {attr});

		return Arrays.asList(
				srcBsplEntry,
				srcGeneratedSourcesEntry);
	}

	private static void setDerived(IResource resource) {
		if (resource != null) {
			try {
				resource.setDerived(true, null);
			} catch (CoreException exception) {
				//
			}
		}
	}

	private static List<CPListElement> buildClassPathEntries(IJavaProject project,
			IFolder[] sourcePaths, IFolder[] generationPaths,
			boolean keepProjectClasspath, boolean addBspllLibraries) {
		final var list = new ArrayList<CPListElement>();

		final var added = new TreeSet<String>();

		for (final var sourcePath : sourcePaths) {
			if (sourcePath != null) {
				final var filename = sourcePath.getFullPath().makeAbsolute();
				if (added.add(filename.toPortableString())) {
					final var entry = JavaCore.newSourceEntry(
							filename,
							ClasspathEntry.INCLUDE_ALL,
							ClasspathEntry.EXCLUDE_NONE,
							null /*output location*/);
					list.add(CPListElement.create(entry, false, project));
				}
			}
		}

		for (final IFolder sourcePath : generationPaths) {
			if (sourcePath != null) {
				final var filename = sourcePath.getFullPath().makeAbsolute();
				if (added.add(filename.toPortableString())) {
					final var attr = JavaCore.newClasspathAttribute(
							IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
							Boolean.TRUE.toString());
					final var entry = JavaCore.newSourceEntry(
							filename,
							ClasspathEntry.INCLUDE_ALL,
							ClasspathEntry.EXCLUDE_NONE,
							null /*output location*/,
							new IClasspathAttribute[] {attr});
					list.add(CPListElement.create(entry, false, project));
				}
			}
		}

		if (keepProjectClasspath) {
			try {
				for (final var entry : project.getRawClasspath()) {
					if (added.add(entry.getPath().toPortableString())) {
						list.add(CPListElement.create(entry, false, project));
					}
				}
			} catch (Exception exception) {
				BSPLEclipsePlugin.getDefault().log(exception);
			}
		}

		return list;
	}

	private static IFolder ensureGeneratedSourceFolder(IProject project, String folderPath, boolean isIFolderRequired,
			boolean createFolder, IProgressMonitor monitor) throws CoreException {
		final var folder = project.getFolder(Path.fromPortableString(folderPath));
		if (!folder.exists()) {
			if (createFolder) {
				CoreUtility.createFolder(folder, true, true, monitor);
			} else if (!isIFolderRequired) {
				monitor.done();
				return null;
			}
		}
		setDerived(folder);
		monitor.done();
		return folder;
	}

	private static IFolder ensureSourceFolder(IProject project, String folderPath, boolean isIFolderRequired,
			boolean createFolder, IProgressMonitor monitor) throws CoreException {
		final var folder = project.getFolder(Path.fromPortableString(folderPath));
		if (!folder.exists()) {
			if (createFolder) {
				CoreUtility.createFolder(folder, true, true, monitor);
			} else if (!isIFolderRequired) {
				monitor.done();
				return null;
			}
		}
		monitor.done();
		return folder;
	}

	/** Collect the list of BSPL project folders that are under directory into files.
	 *
	 * @param folders the list of folders to fill in.
	 * @param directory the directory to explore.
	 * @param directoriesVisited Set of canonical paths of directories, used as recursion guard.
	 * @param nestedProjects whether to look for nested projects.
	 * @param monitor The monitor to report to.
	 */
	protected void collectProjectFoldersFromDirectory(Collection<File> folders, File directory,
			Set<String> directoriesVisited, boolean nestedProjects, IProgressMonitor monitor) {
		if (monitor.isCanceled()) {
			return;
		}
		monitor.subTask(NLS.bind(
				Messages.BSPLProjectConfigurator_0,
				directory.getPath()));

		final var contents = directory.listFiles();
		if (contents == null) {
			return;
		}

		var visited = directoriesVisited;
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

		final var subdirectories = new LinkedHashSet<File>();

		for (final var file : contents) {
			if (file.isDirectory()) {
				subdirectories.add(file);
			} else if (file.getName().endsWith(this.fileExtension)) {
				// Found a BSPL file.
				final var rootFile = getProjectFolderForSourceFolder(file.getParentFile());
				if (rootFile != null) {
					folders.add(rootFile);
					return;
				}
			}
		}

		for (final var subdir : subdirectories) {
			try {
				final var canonicalPath = subdir.getCanonicalPath();
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
		final var filePath = Path.fromOSString(file.getAbsolutePath());
		for (final var rawPath : this.candidates) {
			final var path = Path.fromPortableString(rawPath);

			IPath parent = null;
			var fp = (IPath) filePath.clone();
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

	/** Add the natures to the given project.
	 *
	 * @param project the project.
	 * @param monitor the monitor.
	 * @param natureIdentifiers the identifiers of the natures to add to the project.
	 * @return the status if the operation.
	 * @since 0.8
	 */
	public static IStatus addNatures(IProject project, IProgressMonitor monitor, String... natureIdentifiers) {
		if (project != null && natureIdentifiers != null && natureIdentifiers.length > 0) {
			try {
				final var subMonitor = SubMonitor.convert(monitor, natureIdentifiers.length + 2);
				final var description = project.getDescription();
				final var natures = new LinkedList<>(Arrays.asList(description.getNatureIds()));

				for (final var natureIdentifier : natureIdentifiers) {
					if (!Strings.isNullOrEmpty(natureIdentifier) && !natures.contains(natureIdentifier)) {
						natures.add(0, natureIdentifier);
					}
					subMonitor.worked(1);
				}

				final var newNatures = natures.toArray(new String[natures.size()]);
				final var status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);
				subMonitor.worked(1);

				// check the status and decide what to do
				if (status.getCode() == IStatus.OK) {
					description.setNatureIds(newNatures);
					project.setDescription(description, subMonitor.newChild(1));
				}
				subMonitor.done();
				return status;
			} catch (CoreException exception) {
				return BSPLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
			}
		}
		return BSPLEclipsePlugin.getDefault().createOkStatus();
	}

	/** Replies the BSPL natures that could be added to a project.
	 *
	 * @return the BSPL project natures.
	 */
	public static String[] getBsplNatures() {
		return new String[] {BSPLEclipseConfig.NATURE_ID};
	}

	/** Add the BSPL natures to the given project.
	 *
	 * @param project the project.
	 * @param monitor the monitor.
	 * @return the status if the operation.
	 */
	public static IStatus addBsplNatures(IProject project, IProgressMonitor monitor) {
		return addNatures(project, monitor, getBsplNatures());
	}

	@Override
	public boolean canUnconfigure(IProject project, IProgressMonitor monitor) {
		try {
			return project != null && project.hasNature(BSPLEclipseConfig.NATURE_ID);
		} catch (CoreException exception) {
			return false;
		} finally {
			monitor.done();
		}
	}

	@Override
	public void unconfigure(IProject project, IProgressMonitor monitor) throws CoreException {
		try {
			final var mon = SubMonitor.convert(monitor, 4);
			final var description = project.getDescription();
			final var natures = new LinkedList<>(Arrays.asList(description.getNatureIds()));
			natures.remove(BSPLEclipseConfig.XTEXT_NATURE_ID);
			natures.remove(BSPLEclipseConfig.NATURE_ID);
			final var newNatures = natures.toArray(new String[natures.size()]);
			mon.worked(1);
			final var status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);
			mon.worked(1);
			if (status.getCode() == IStatus.OK) {
				description.setNatureIds(newNatures);
				project.setDescription(description, mon.newChild(1));
				safeRefresh(project, mon.newChild(1));
			} else {
				throw new CoreException(status);
			}
		} finally {
			monitor.done();
		}
	}

}
