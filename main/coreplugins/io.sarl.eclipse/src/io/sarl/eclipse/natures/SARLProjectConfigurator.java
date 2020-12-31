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
import java.util.TreeSet;

import com.google.common.base.Strings;
import com.google.inject.Injector;
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
import io.sarl.lang.util.OutParameter;

/**
 * Configurator for a SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectConfigurator implements ProjectConfigurator, IProjectUnconfigurator {

	private final String fileExtension;

	private final Collection<String> candidates;

	/** Constructor.
	 */
	public SARLProjectConfigurator() {
		final Injector injector = SARLEclipseExecutableExtensionFactory.getSARLInjector();
		final String fileExtension = injector.getInstance(
				Key.get(String.class, Names.named(Constants.FILE_EXTENSIONS)));
		if (Strings.isNullOrEmpty(fileExtension)) {
			this.fileExtension = null;
		} else {
			this.fileExtension = "." + fileExtension; //$NON-NLS-1$
		}
		injector.injectMembers(this);
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
		try {
			return project != null && !project.hasNature(SARLEclipseConfig.NATURE_ID);
		} catch (CoreException exception) {
			return false;
		} finally {
			monitor.done();
		}
	}

	@Override
	public void configure(IProject project, Set<IPath> ignoredPaths, IProgressMonitor monitor) {
		try {
			configureSARLProject(project, true, true, true, monitor);
			safeRefresh(project, monitor);
		} finally {
			monitor.done();
		}
	}

	@Override
	public boolean canUnconfigure(IProject project, IProgressMonitor monitor) {
		try {
			return project != null && project.hasNature(SARLEclipseConfig.NATURE_ID);
		} catch (CoreException exception) {
			return false;
		} finally {
			monitor.done();
		}
	}

	@Override
	public void unconfigure(IProject project, IProgressMonitor monitor) throws CoreException {
		try {
			final SubMonitor mon = SubMonitor.convert(monitor, 4);
			final IProjectDescription description = project.getDescription();
			final List<String> natures = new LinkedList<>(Arrays.asList(description.getNatureIds()));
			natures.remove(SARLEclipseConfig.XTEXT_NATURE_ID);
			natures.remove(SARLEclipseConfig.NATURE_ID);
			final String[] newNatures = natures.toArray(new String[natures.size()]);
			mon.worked(1);
			final IStatus status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);
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
			SARLEclipsePlugin.getDefault().log(exception);
		}
	}

	/** Replies the list of the standard source folders for a SARL project.
	 *
	 * @return the list of the standard source folders.
	 * @since 0.8
	 */
	public static String[] getSARLProjectSourceFolders() {
		return new String[] {
			SARLConfig.FOLDER_SOURCE_SARL,
			SARLConfig.FOLDER_SOURCE_JAVA,
			SARLConfig.FOLDER_RESOURCES,
			SARLConfig.FOLDER_TEST_SOURCE_SARL,
			SARLConfig.FOLDER_SOURCE_GENERATED,
			SARLConfig.FOLDER_TEST_SOURCE_GENERATED,
		};
	}

	/** Replies the list of the standard binary folders for a SARL project.
	 *
	 * @return the list of the standard binary folders.
	 * @since 0.8
	 */
	public static String[] getSARLProjectBinFolders() {
		return new String[] {
			SARLConfig.FOLDER_BIN,
			SARLConfig.FOLDER_TEST_BIN,
		};
	}

	/** Configure the SARL project.
	 *
	 * <p>This function does the following:<ul>
	 * <li>Add the SARL nature to the project;</li>
	 * <li>Create the standard SARL folders if {@code createFolders} is evaluated to true;</li>
	 * <li>Set the output configuration of the project from the
	 * {@link SARLPreferences#setSpecificSARLConfigurationFor(IProject, IPath) general SARL configuration};</li>
	 * <li>Reset the Java configuration in order to follow the SARL configuration, if {@code configureJabvaNature}
	 * is evaluated to true.</li>
	 * </ul>
	 *
	 * @param project the project.
	 * @param addNatures indicates if the natures must be added to the project by calling {@link #addSarlNatures(IProject, IProgressMonitor)}.
	 * @param configureJavaNature indicates if the Java configuration elements must be configured.
	 * @param createFolders indicates if the folders must be created or not.
	 * @param monitor the monitor.
	 * @see #addSarlNatures(IProject, IProgressMonitor)
	 * @see #configureSARLSourceFolders(IProject, boolean, IProgressMonitor)
	 */
	public static void configureSARLProject(IProject project, boolean addNatures,
			boolean configureJavaNature, boolean createFolders, IProgressMonitor monitor) {
		try {
			final SubMonitor subMonitor = SubMonitor.convert(monitor, 11);
			// Add Natures
			final IStatus status = Status.OK_STATUS;
			if (addNatures) {
				addSarlNatures(project, subMonitor.newChild(1));
				if (status != null && !status.isOK()) {
					SARLEclipsePlugin.getDefault().getLog().log(status);
				}
			}

			// Ensure SARL specific folders.
			final OutParameter<IFolder[]> sourceFolders = new OutParameter<>();
			final OutParameter<IFolder[]> testSourceFolders = new OutParameter<>();
			final OutParameter<IFolder[]> generationFolders = new OutParameter<>();
			final OutParameter<IFolder[]> testGenerationFolders = new OutParameter<>();
			final OutParameter<IFolder> generationFolder = new OutParameter<>();
			final OutParameter<IFolder> testGenerationFolder = new OutParameter<>();
			final OutParameter<IFolder> outputFolder = new OutParameter<>();
			final OutParameter<IFolder> testOutputFolder = new OutParameter<>();
			ensureSourceFolders(project, createFolders, subMonitor,
					sourceFolders, testSourceFolders,
					generationFolders, testGenerationFolders,
					generationFolder, testGenerationFolder,
					outputFolder, testOutputFolder);

			// SARL specific configuration
			final IFolder testGenerationFolderFolder = testGenerationFolder.get();
			final IPath testGenerationFolderPath = testGenerationFolderFolder == null ? null
					: testGenerationFolderFolder.getProjectRelativePath();
			SARLPreferences.setSpecificSARLConfigurationFor(project,
					generationFolder.get().getProjectRelativePath(),
					testGenerationFolderPath);
			subMonitor.worked(1);

			// Create the Java project
			if (configureJavaNature) {

				if (!addNatures) {
					addNatures(project, subMonitor.newChild(1), JavaCore.NATURE_ID);
				}

				final IJavaProject javaProject = JavaCore.create(project);
				subMonitor.worked(1);

				// Build path
				BuildPathsBlock.flush(
						buildClassPathEntries(javaProject,
								sourceFolders.get(),
								testSourceFolders.get(),
								generationFolders.get(),
								testGenerationFolders.get(),
								testOutputFolder.get().getFullPath(),
								false, true),
						outputFolder.get().getFullPath(), javaProject, null, subMonitor.newChild(1));
			}
			subMonitor.done();
		} catch (CoreException exception) {
			SARLEclipsePlugin.getDefault().log(exception);
		}
	}

	@SuppressWarnings({"checkstyle:parameternumber", "checkstyle:npathcomplexity"})
	private static void ensureSourceFolders(IProject project, boolean createFolders, SubMonitor monitor,
			OutParameter<IFolder[]> sourcePaths,
			OutParameter<IFolder[]> testSourcePaths,
			OutParameter<IFolder[]> generationPaths,
			OutParameter<IFolder[]> testGenerationPaths,
			OutParameter<IFolder> standardGenerationFolder,
			OutParameter<IFolder> testingGenerationFolder,
			OutParameter<IFolder> classOutput,
			OutParameter<IFolder> testClassOutput) throws CoreException {
		final IFolder sourceSarlFolder = ensureSourceFolder(project,
				SARLConfig.FOLDER_SOURCE_SARL, true, createFolders, monitor.newChild(1));
		final IFolder sourceJavaFolder = ensureSourceFolder(project,
				SARLConfig.FOLDER_SOURCE_JAVA, false, createFolders, monitor.newChild(1));
		final IFolder resourcesFolder = ensureSourceFolder(project,
				SARLConfig.FOLDER_RESOURCES, false, createFolders, monitor.newChild(1));
		final IFolder testSourceSarlFolder = ensureSourceFolder(project,
				SARLConfig.FOLDER_TEST_SOURCE_SARL, false, createFolders, monitor.newChild(1));
		final IFolder generationFolder = ensureGeneratedSourceFolder(project,
				SARLConfig.FOLDER_SOURCE_GENERATED, true, createFolders, monitor.newChild(1));
		final IFolder testGenerationFolder = ensureGeneratedSourceFolder(project,
				SARLConfig.FOLDER_TEST_SOURCE_GENERATED, false, createFolders, monitor.newChild(1));
		final IFolder outputFolder = ensureOutputFolder(project,
				SARLConfig.FOLDER_BIN, true, createFolders, monitor.newChild(1));
		final IFolder testOutputFolder = ensureOutputFolder(project,
				SARLConfig.FOLDER_TEST_BIN, true, createFolders, monitor.newChild(1));
		if (sourcePaths != null) {
			assert sourceSarlFolder != null : "sourceSarlFolder must not be null";
			if (sourceJavaFolder != null) {
				if (resourcesFolder != null) {
					sourcePaths.set(new IFolder[] {sourceSarlFolder, sourceJavaFolder, resourcesFolder});
				} else {
					sourcePaths.set(new IFolder[] {sourceSarlFolder, sourceJavaFolder});
				}
			} else if (resourcesFolder != null) {
				sourcePaths.set(new IFolder[] {sourceSarlFolder, resourcesFolder});
			} else {
				sourcePaths.set(new IFolder[] {sourceSarlFolder});
			}
		}
		if (testSourcePaths != null) {
			if (testSourceSarlFolder != null) {
				testSourcePaths.set(new IFolder[] {testSourceSarlFolder});
			} else {
				testSourcePaths.set(new IFolder[] {});
			}
		}
		if (generationPaths != null) {
			assert generationFolder != null : "generationFolder must not be null";
			generationPaths.set(new IFolder[] {generationFolder});
		}
		if (testGenerationPaths != null) {
			if (testGenerationFolder != null) {
				testGenerationPaths.set(new IFolder[] {testGenerationFolder});
			} else {
				testGenerationPaths.set(new IFolder[] {});
			}
		}
		if (standardGenerationFolder != null) {
			assert generationFolder != null : "generationFolder must not be null";
			standardGenerationFolder.set(generationFolder);
		}
		if (testingGenerationFolder != null && testGenerationFolder != null) {
			testingGenerationFolder.set(testGenerationFolder);
		}
		if (classOutput != null) {
			assert outputFolder != null : "outputFolder must not be null";
			classOutput.set(outputFolder);
		}
		if (testClassOutput != null) {
			assert testOutputFolder != null : "testOutputFolder must not be null";
			testClassOutput.set(testOutputFolder);
		}
	}

	/** Configure the source folders for a SARL project.
	 *
	 *
	 * @param project the project.
	 * @param createFolders indicates if the folders must be created or not.
	 * @param monitor the monitor.
	 * @since 0.8
	 * @see #addSarlNatures(IProject, IProgressMonitor)
	 * @see #configureSARLProject(IProject, boolean, boolean, boolean, IProgressMonitor)
	 */
	public static void configureSARLSourceFolders(IProject project, boolean createFolders, IProgressMonitor monitor) {
		try {
			final SubMonitor subMonitor = SubMonitor.convert(monitor, 8);

			final OutParameter<IFolder[]> sourceFolders = new OutParameter<>();
			final OutParameter<IFolder[]> testSourceFolders = new OutParameter<>();
			final OutParameter<IFolder[]> generationFolders = new OutParameter<>();
			final OutParameter<IFolder[]> testGenerationFolders = new OutParameter<>();
			final OutParameter<IFolder> testOutputFolder = new OutParameter<>();
			ensureSourceFolders(project, createFolders, subMonitor,
					sourceFolders, testSourceFolders,
					generationFolders, testGenerationFolders,
					null,
					null,
					null,
					testOutputFolder);

			final IJavaProject javaProject = JavaCore.create(project);
			subMonitor.worked(1);

			// Build path
			BuildPathsBlock.flush(
					buildClassPathEntries(javaProject,
							sourceFolders.get(),
							testSourceFolders.get(),
							generationFolders.get(),
							testGenerationFolders.get(),
							testOutputFolder.get().getFullPath(),
							true, false),
					javaProject.getOutputLocation(), javaProject, null, subMonitor.newChild(1));

			subMonitor.done();
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

	private static void setDerived(IResource resource) {
		if (resource != null) {
			try {
				resource.setDerived(true, null);
			} catch (CoreException exception) {
				//
			}
		}
	}

	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	private static List<CPListElement> buildClassPathEntries(IJavaProject project,
			IFolder[] sourcePaths, IFolder[] testSourcePaths,
			IFolder[] generationPaths, IFolder[] testGenerationPaths,
			IPath testOutputPath,
			boolean keepProjectClasspath, boolean addSarllLibraries) {
		final List<CPListElement> list = new ArrayList<>();

		final Set<String> added = new TreeSet<>();

		for (final IFolder sourcePath : sourcePaths) {
			if (sourcePath != null) {
				final IPath filename = sourcePath.getFullPath().makeAbsolute();
				if (added.add(filename.toPortableString())) {
					final IClasspathEntry entry = JavaCore.newSourceEntry(
							filename,
							ClasspathEntry.INCLUDE_ALL,
							ClasspathEntry.EXCLUDE_NONE,
							null /*output location*/);
					list.add(CPListElement.create(entry, false, project));
				}
			}
		}

		for (final IFolder sourcePath : testSourcePaths) {
			if (sourcePath != null) {
				final IPath filename = sourcePath.getFullPath().makeAbsolute();
				if (added.add(filename.toPortableString())) {
					final IClasspathAttribute attr0 = JavaCore.newClasspathAttribute(
							IClasspathAttribute.TEST,
							Boolean.TRUE.toString());
					final IClasspathEntry entry = JavaCore.newSourceEntry(
							filename,
							ClasspathEntry.INCLUDE_ALL,
							ClasspathEntry.EXCLUDE_NONE,
							testOutputPath,
							new IClasspathAttribute[] {attr0});
					list.add(CPListElement.create(entry, false, project));
				}
			}
		}

		for (final IFolder sourcePath : generationPaths) {
			if (sourcePath != null) {
				final IPath filename = sourcePath.getFullPath().makeAbsolute();
				if (added.add(filename.toPortableString())) {
					final IClasspathAttribute attr = JavaCore.newClasspathAttribute(
							IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
							Boolean.TRUE.toString());
					final IClasspathEntry entry = JavaCore.newSourceEntry(
							filename,
							ClasspathEntry.INCLUDE_ALL,
							ClasspathEntry.EXCLUDE_NONE,
							null /*output location*/,
							new IClasspathAttribute[] {attr});
					list.add(CPListElement.create(entry, false, project));
				}
			}
		}

		for (final IFolder sourcePath : testGenerationPaths) {
			if (sourcePath != null) {
				final IPath filename = sourcePath.getFullPath().makeAbsolute();
				if (added.add(filename.toPortableString())) {
					final IClasspathAttribute attr0 = JavaCore.newClasspathAttribute(
							IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
							Boolean.TRUE.toString());
					final IClasspathAttribute attr1 = JavaCore.newClasspathAttribute(
							IClasspathAttribute.TEST,
							Boolean.TRUE.toString());
					final IClasspathEntry entry = JavaCore.newSourceEntry(
							filename,
							ClasspathEntry.INCLUDE_ALL,
							ClasspathEntry.EXCLUDE_NONE,
							testOutputPath,
							new IClasspathAttribute[] {attr0, attr1});
					list.add(CPListElement.create(entry, false, project));
				}
			}
		}

		if (addSarllLibraries) {
			for (final IClasspathEntry current : PreferenceConstants.getDefaultJRELibrary()) {
				if (current != null && added.add(current.getPath().toPortableString())) {
					list.add(CPListElement.create(current, true, project));
					break;
				}
			}

			if (added.add(SARLClasspathContainerInitializer.CONTAINER_ID.toPortableString())) {
				list.add(CPListElement.create(
						JavaCore.newContainerEntry(SARLClasspathContainerInitializer.CONTAINER_ID),
						true, project));
			}
		}

		if (keepProjectClasspath) {
			try {
				for (final IClasspathEntry entry : project.getRawClasspath()) {
					if (added.add(entry.getPath().toPortableString())) {
						list.add(CPListElement.create(entry, false, project));
					}
				}
			} catch (Exception exception) {
				SARLEclipsePlugin.getDefault().log(exception);
			}
		}

		return list;
	}

	private static IFolder ensureGeneratedSourceFolder(IProject project, String folderPath, boolean isIFolderRequired,
			boolean createFolder, IProgressMonitor monitor) throws CoreException {
		final IFolder folder = project.getFolder(Path.fromPortableString(folderPath));
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
		final IFolder folder = project.getFolder(Path.fromPortableString(folderPath));
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

	private static IFolder ensureOutputFolder(IProject project, String folderPath, boolean isIFolderRequired,
			boolean createFolder, IProgressMonitor monitor) throws CoreException {
		final IFolder folder = project.getFolder(Path.fromPortableString(folderPath));
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
				final SubMonitor subMonitor = SubMonitor.convert(monitor, natureIdentifiers.length + 2);
				final IProjectDescription description = project.getDescription();
				final List<String> natures = new LinkedList<>(Arrays.asList(description.getNatureIds()));

				for (final String natureIdentifier : natureIdentifiers) {
					if (!Strings.isNullOrEmpty(natureIdentifier) && !natures.contains(natureIdentifier)) {
						natures.add(0, natureIdentifier);
					}
					subMonitor.worked(1);
				}

				final String[] newNatures = natures.toArray(new String[natures.size()]);
				final IStatus status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);
				subMonitor.worked(1);

				// check the status and decide what to do
				if (status.getCode() == IStatus.OK) {
					description.setNatureIds(newNatures);
					project.setDescription(description, subMonitor.newChild(1));
				}
				subMonitor.done();
				return status;
			} catch (CoreException exception) {
				return SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception);
			}
		}
		return SARLEclipsePlugin.getDefault().createOkStatus();
	}

	/** Replies the SARL natures that could be added to a project.
	 *
	 * @return the SARL project natures.
	 */
	public static String[] getSarlNatures() {
		return new String[] {JavaCore.NATURE_ID, SARLEclipseConfig.XTEXT_NATURE_ID, SARLEclipseConfig.NATURE_ID};
	}

	/** Add the SARL natures to the given project.
	 *
	 * @param project the project.
	 * @param monitor the monitor.
	 * @return the status if the operation.
	 */
	public static IStatus addSarlNatures(IProject project, IProgressMonitor monitor) {
		return addNatures(project, monitor, getSarlNatures());
	}

}
