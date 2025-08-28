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

import static io.sarl.apputils.eclipseextensions.projectconfig.ProjectConfigurationFragments.ensureGeneratedSourceFolder;
import static io.sarl.apputils.eclipseextensions.projectconfig.ProjectConfigurationFragments.ensureSourceFolder;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.core.ClasspathEntry;

import io.sarl.apputils.eclipseextensions.projectconfig.ProjectConfigurationFragment;
import io.sarl.apputils.uiextensions.ProjectNatures;
import io.sarl.bspl.eclipse.BSPLEclipseConfig;
import io.sarl.bspl.eclipse.BSPLEclipsePlugin;
import io.sarl.bspl.eclipse.buildpath.BSPLClasspathContainerInitializer;
import io.sarl.bspl.lang.BSPLConfig;
import io.sarl.bspl.lang.ui.preferences.BSPLPreferences;

/**
 * Fragment of a project configurator dedicated to a project with BSPL nature.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class BSPLProjectConfigurationFragment implements ProjectConfigurationFragment {

	@Override
	public String getId() {
		return BSPLEclipsePlugin.PLUGIN_ID;
	}

	@Override
	public boolean isActiveByDefault() {
		// While this contribution is marked as experimental feature, it should be not activated by default
		return false;
	}

	@Override
	public String getLabel() {
		return Messages.BSPLProjectConfigurationFragment_0;
	}

	/** Replies the BSPL natures that could be added to a project.
	 *
	 * @return the BSPL project natures.
	 */
	public static String[] getBsplNatures() {
		return new String[] {BSPLEclipseConfig.NATURE_ID};
	}

	@Override
	public void updateNatures(IProject project, IProgressMonitor monitor) {
		ProjectNatures.addNatures(project, monitor, getBsplNatures());
	}

	@Override
	public void updateSourceFolders(IProject project, boolean createFolders, IProgressMonitor monitor,
			SortedSet<IFolder> sourcePaths, SortedSet<IFolder> testSourcePaths, SortedSet<IFolder> generationPaths,
			SortedSet<IFolder> testGenerationPaths) throws CoreException {
		final var subMonitor = SubMonitor.convert(monitor, 2);
		final var sourceBsplFolder = ensureSourceFolder(project,
				BSPLConfig.FOLDER_SOURCE_BSPL, true, createFolders, subMonitor.newChild(1));
		final var generationFolder = ensureGeneratedSourceFolder(project,
				BSPLConfig.FOLDER_SOURCE_GENERATED, true, createFolders, subMonitor.newChild(1));
		if (sourcePaths != null) {
			assert sourceBsplFolder != null : "sourceBsplFolder must not be null"; //$NON-NLS-1$
			sourcePaths.add(sourceBsplFolder);
		}
		if (generationPaths != null) {
			assert generationFolder != null : "generationFolder must not be null"; //$NON-NLS-1$
			generationPaths.add(generationFolder);
		}
	}

	@Override
	public void updateSpecificConfiguration(IProject project, IProgressMonitor monitor, IPath generationFolderPath,
			IPath testGenerationFolderPath) {
		BSPLPreferences.setSpecificBSPLConfigurationFor(project, generationFolderPath);
	}

	@Override
	public void unconfigure(IProject project, IProgressMonitor monitor, List<String> natures) {
		natures.removeAll(Arrays.asList(getBsplNatures()));
	}

	@SuppressWarnings("restriction")
	@Override
	public List<IClasspathEntry> getDefaultSourceClassPathEntries(IPath projectFolder) {
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

	@Override
	public List<IClasspathEntry> getBinaryClassPathEntries(IPath projectFolder) {
		final var library = JavaCore.newContainerEntry(
				BSPLClasspathContainerInitializer.CONTAINER_ID,
				new IAccessRule[0],
				new IClasspathAttribute[0],
				true);
		return Collections.singletonList(library);
	}

}
