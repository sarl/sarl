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

package io.sarl.eclipse.natures;

import static io.sarl.apputils.uiextensions.ProjectNatures.addNatures;

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
import java.util.SortedSet;
import java.util.TreeSet;

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
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jdt.internal.ui.wizards.buildpaths.BuildPathsBlock;
import org.eclipse.jdt.internal.ui.wizards.buildpaths.CPListElement;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.internal.ide.StatusUtil;
import org.eclipse.ui.statushandlers.StatusManager;
import org.eclipse.ui.wizards.datatransfer.ProjectConfigurator;
import org.eclipse.xtext.Constants;

import io.sarl.apputils.eclipseextensions.buildpath.SARLFolderComparator;
import io.sarl.apputils.eclipseextensions.projectconfig.ProjectConfigurationFragments;
import io.sarl.apputils.uiextensions.Bundles;
import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipseExecutableExtensionFactory;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.core.util.OutParameter;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.ui.preferences.SARLPreferences;

/**
 * Configurator for a SARL project.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
@SuppressWarnings("restriction")
public class SARLProjectConfigurator implements ProjectConfigurator, IProjectUnconfigurator {

	private final String fileExtension;

	private final Collection<String> candidates;

	/** Constructor.
	 */
	public SARLProjectConfigurator() {
		final var injector = SARLEclipseExecutableExtensionFactory.getSARLInjector();
		final var fileExtension = SarlUtils.getMajorFileExtension(injector.getInstance(
				Key.get(String.class, Names.named(Constants.FILE_EXTENSIONS))));
		if (Strings.isNullOrEmpty(fileExtension)) {
			this.fileExtension = null;
		} else {
			this.fileExtension = "." + fileExtension; //$NON-NLS-1$
		}
		injector.injectMembers(this);
		this.candidates = new ArrayList<>();
		this.candidates.addAll(Arrays.asList(Bundles.SRC_FOLDERS));
		this.candidates.add(SARLConfig.FOLDER_TEST_SOURCE_SARL);
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
		for (final var binPath : Bundles.BIN_FOLDERS) {
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
			// Get the configuration fragments
			final var fragments = ProjectConfigurationFragments.getConfigurationFragmentsFromExtension();

			final var mon = SubMonitor.convert(monitor, 4 + fragments.size());

			final var description = project.getDescription();
			final var natures = new LinkedList<>(Arrays.asList(description.getNatureIds()));
			natures.remove(SARLEclipseConfig.XTEXT_NATURE_ID);
			natures.remove(SARLEclipseConfig.NATURE_ID);

			for (final var fragment : fragments) {
				fragment.unconfigure(project, mon.newChild(1), natures);
			}

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
			SARLConfig.FOLDER_INTEGRATION_TEST_SOURCE_SARL,
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
	 * @param createFolders indicates if the folders must be created or not.Collection
	 * @param monitor the monitor.
	 * @see #addSarlNatures(IProject, IProgressMonitor)
	 * @see #configureSARLSourceFolders(IProject, boolean, IProgressMonitor)
	 */
	public static void configureSARLProject(IProject project, boolean addNatures,
			boolean configureJavaNature, boolean createFolders, IProgressMonitor monitor) {
		try {
			// Get the configuration fragments
			final var fragments = ProjectConfigurationFragments.getConfigurationFragmentsFromExtension();
			
			final var subMonitor = SubMonitor.convert(monitor, 12);
			// Add Natures
			final var status = Status.OK_STATUS;
			if (addNatures) {
				addSarlNatures(project, subMonitor.newChild(1));
				if (status != null && !status.isOK()) {
					SARLEclipsePlugin.getDefault().getLog().log(status);
				}
				if (!fragments.isEmpty()) {
					final var subprogress = subMonitor.newChild(1);
					subprogress.setWorkRemaining(fragments.size());
					for (final var fragment : fragments) {
						fragment.updateNatures(project, subprogress.newChild(1));
					}
					subprogress.worked(fragments.size());
				}
			}

			// Ensure SARL specific folders.
			final var sourceFolders = new TreeSet<>(SARLFolderComparator.getSingleton());
			final var testSourceFolders = new TreeSet<>(SARLFolderComparator.getSingleton());
			final var generationFolders = new TreeSet<>(SARLFolderComparator.getSingleton());
			final var testGenerationFolders = new TreeSet<>(SARLFolderComparator.getSingleton());
			final var generationFolder = new OutParameter<IFolder>();
			final var testGenerationFolder = new OutParameter<IFolder>();
			final var outputFolder = new OutParameter<IFolder>();
			final var testOutputFolder = new OutParameter<IFolder>();
			ensureSourceFolders(project, createFolders, subMonitor,
					sourceFolders, testSourceFolders,
					generationFolders, testGenerationFolders,
					generationFolder, testGenerationFolder,
					outputFolder, testOutputFolder);
			if (!fragments.isEmpty()) {
				final var subprogress = subMonitor.newChild(1);
				subprogress.setWorkRemaining(fragments.size());
				for (final var fragment : fragments) {
					fragment.updateSourceFolders(project, createFolders, subprogress.newChild(1),
							sourceFolders, testSourceFolders,
							generationFolders, testGenerationFolders);
				}
				subprogress.worked(fragments.size());
			}

			// SARL specific configuration
			final var testGenerationFolderFolder = testGenerationFolder.get();
			final var testGenerationFolderPath = testGenerationFolderFolder == null ? null
					: testGenerationFolderFolder.getProjectRelativePath();
			SARLPreferences.setSpecificSARLConfigurationFor(project,
					generationFolder.get().getProjectRelativePath(),
					testGenerationFolderPath);
			subMonitor.worked(1);
			if (!fragments.isEmpty()) {
				final var subprogress = subMonitor.newChild(1);
				subprogress.setWorkRemaining(fragments.size());
				for (final var fragment : fragments) {
					fragment.updateSpecificConfiguration(project, subprogress.newChild(1),
							generationFolder.get().getProjectRelativePath(), testGenerationFolderPath);
				}
				subprogress.worked(fragments.size());
			}

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
								sourceFolders.toArray(size -> new IFolder[size]),
								testSourceFolders.toArray(size -> new IFolder[size]),
								generationFolders.toArray(size -> new IFolder[size]),
								testGenerationFolders.toArray(size -> new IFolder[size]),
								testOutputFolder.get().getFullPath(),
								false, true),
						outputFolder.get().getFullPath(), javaProject, null, subMonitor.newChild(1));
			}
			subMonitor.done();
		} catch (CoreException exception) {
			SARLEclipsePlugin.getDefault().log(exception);
		}
	}

	private static void ensureSourceFolders(IProject project, boolean createFolders, SubMonitor monitor,
			SortedSet<IFolder> sourcePaths,
			SortedSet<IFolder> testSourcePaths,
			SortedSet<IFolder> generationPaths,
			SortedSet<IFolder> testGenerationPaths,
			OutParameter<IFolder> standardGenerationFolder,
			OutParameter<IFolder> testingGenerationFolder,
			OutParameter<IFolder> classOutput,
			OutParameter<IFolder> testClassOutput) throws CoreException {
		final var sourceSarlFolder = ProjectConfigurationFragments.ensureSourceFolder(project,
				SARLConfig.FOLDER_SOURCE_SARL, true, createFolders, monitor.newChild(1));
		final var sourceJavaFolder = ProjectConfigurationFragments.ensureSourceFolder(project,
				SARLConfig.FOLDER_SOURCE_JAVA, false, createFolders, monitor.newChild(1));
		final var resourcesFolder = ProjectConfigurationFragments.ensureSourceFolder(project,
				SARLConfig.FOLDER_RESOURCES, false, createFolders, monitor.newChild(1));
		final var testSourceSarlFolder = ProjectConfigurationFragments.ensureSourceFolder(project,
				SARLConfig.FOLDER_TEST_SOURCE_SARL, false, createFolders, monitor.newChild(1));
		final var generationFolder = ProjectConfigurationFragments.ensureGeneratedSourceFolder(project,
				SARLConfig.FOLDER_SOURCE_GENERATED, true, createFolders, monitor.newChild(1));
		final var testGenerationFolder = ProjectConfigurationFragments.ensureGeneratedSourceFolder(project,
				SARLConfig.FOLDER_TEST_SOURCE_GENERATED, false, createFolders, monitor.newChild(1));
		final var outputFolder = ProjectConfigurationFragments.ensureOutputFolder(project,
				SARLConfig.FOLDER_BIN, true, createFolders, monitor.newChild(1));
		final var testOutputFolder = ProjectConfigurationFragments.ensureOutputFolder(project,
				SARLConfig.FOLDER_TEST_BIN, true, createFolders, monitor.newChild(1));
		if (sourcePaths != null) {
			assert sourceSarlFolder != null : "sourceSarlFolder must not be null"; //$NON-NLS-1$
			if (sourceJavaFolder != null) {
				if (resourcesFolder != null) {
					sourcePaths.add(sourceSarlFolder);
					sourcePaths.add(sourceJavaFolder);
					sourcePaths.add(resourcesFolder);
				} else {
					sourcePaths.add(sourceSarlFolder);
					sourcePaths.add(sourceJavaFolder);
				}
			} else if (resourcesFolder != null) {
				sourcePaths.add(sourceSarlFolder);
				sourcePaths.add(resourcesFolder);
			} else {
				sourcePaths.add(sourceSarlFolder);
			}
		}
		if (testSourcePaths != null && testSourceSarlFolder != null) {
			testSourcePaths.add(testSourceSarlFolder);
		}
		if (generationPaths != null) {
			assert generationFolder != null : "generationFolder must not be null"; //$NON-NLS-1$
			generationPaths.add(generationFolder);
		}
		if (testGenerationPaths != null && testGenerationFolder != null) {
			testGenerationPaths.add(testGenerationFolder);
		}
		if (standardGenerationFolder != null) {
			assert generationFolder != null : "generationFolder must not be null"; //$NON-NLS-1$
			standardGenerationFolder.set(generationFolder);
		}
		if (testingGenerationFolder != null && testGenerationFolder != null) {
			testingGenerationFolder.set(testGenerationFolder);
		}
		if (classOutput != null) {
			assert outputFolder != null : "outputFolder must not be null"; //$NON-NLS-1$
			classOutput.set(outputFolder);
		}
		if (testClassOutput != null) {
			assert testOutputFolder != null : "testOutputFolder must not be null"; //$NON-NLS-1$
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
			final var subMonitor = SubMonitor.convert(monitor, 8);

			final var sourceFolders = new TreeSet<>(SARLFolderComparator.getSingleton());
			final var testSourceFolders = new TreeSet<>(SARLFolderComparator.getSingleton());
			final var generationFolders = new TreeSet<>(SARLFolderComparator.getSingleton());
			final var testGenerationFolders = new TreeSet<>(SARLFolderComparator.getSingleton());
			final var testOutputFolder = new OutParameter<IFolder>();
			ensureSourceFolders(project, createFolders, subMonitor,
					sourceFolders, testSourceFolders,
					generationFolders, testGenerationFolders,
					null,
					null,
					null,
					testOutputFolder);

			final var javaProject = JavaCore.create(project);
			subMonitor.worked(1);

			// Build path
			BuildPathsBlock.flush(
					buildClassPathEntries(javaProject,
							sourceFolders.toArray(size -> new IFolder[size]),
							testSourceFolders.toArray(size -> new IFolder[size]),
							generationFolders.toArray(size -> new IFolder[size]),
							testGenerationFolders.toArray(size -> new IFolder[size]),
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
		final var srcJava = projectFolder.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_JAVA));
		final var srcJavaEntry = JavaCore.newSourceEntry(srcJava.makeAbsolute());

		final var srcSarl = projectFolder.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_SARL));
		final var srcSarlEntry = JavaCore.newSourceEntry(srcSarl.makeAbsolute());

		final var srcGeneratedSources = projectFolder.append(
				Path.fromPortableString(SARLConfig.FOLDER_SOURCE_GENERATED));
		final var attr = JavaCore.newClasspathAttribute(
				IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
				Boolean.TRUE.toString());
		final var srcGeneratedSourcesEntry = JavaCore.newSourceEntry(
				srcGeneratedSources.makeAbsolute(),
				ClasspathEntry.INCLUDE_ALL,
				ClasspathEntry.EXCLUDE_NONE,
				null /*output location*/,
				new IClasspathAttribute[] {attr});

		final var srcResources = projectFolder.append(
				Path.fromPortableString(SARLConfig.FOLDER_RESOURCES));
		final var srcResourcesEntry = JavaCore.newSourceEntry(srcResources.makeAbsolute());

		return Arrays.asList(
				srcSarlEntry,
				srcJavaEntry,
				srcResourcesEntry,
				srcGeneratedSourcesEntry);
	}

	private static List<CPListElement> buildClassPathEntries(IJavaProject project,
			IFolder[] sourcePaths, IFolder[] testSourcePaths,
			IFolder[] generationPaths, IFolder[] testGenerationPaths,
			IPath testOutputPath,
			boolean keepProjectClasspath, boolean addSarllLibraries) {
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

		for (final var sourcePath : testSourcePaths) {
			if (sourcePath != null) {
				final var filename = sourcePath.getFullPath().makeAbsolute();
				if (added.add(filename.toPortableString())) {
					final var attr0 = JavaCore.newClasspathAttribute(
							IClasspathAttribute.TEST,
							Boolean.TRUE.toString());
					final var entry = JavaCore.newSourceEntry(
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

		for (final IFolder sourcePath : testGenerationPaths) {
			if (sourcePath != null) {
				final var filename = sourcePath.getFullPath().makeAbsolute();
				if (added.add(filename.toPortableString())) {
					final var attr0 = JavaCore.newClasspathAttribute(
							IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS,
							Boolean.TRUE.toString());
					final var attr1 = JavaCore.newClasspathAttribute(
							IClasspathAttribute.TEST,
							Boolean.TRUE.toString());
					final var entry = JavaCore.newSourceEntry(
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
			for (final var current : PreferenceConstants.getDefaultJRELibrary()) {
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
				for (final var entry : project.getRawClasspath()) {
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

	/** Collect the list of SARL project folders that are under directory into files.
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
				Messages.SARLProjectConfigurator_0,
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
				// Found a SARL file.
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
