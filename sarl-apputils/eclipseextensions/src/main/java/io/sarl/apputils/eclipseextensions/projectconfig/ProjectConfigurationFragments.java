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

package io.sarl.apputils.eclipseextensions.projectconfig;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.internal.ui.util.CoreUtility;

import io.sarl.apputils.eclipseextensions.EclipseExtensionsPlugin;
import io.sarl.apputils.eclipseextensions.Extensions;

/**
 * Tools for the fragments of the configuration for a SARL project.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version eclipseextensions 0.15.0 20250909-115749
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid eclipseextensions
 * @since 0.15
 */
public final class ProjectConfigurationFragments {

	/**
	 * Name of the extension points for the configuration fragments.
	 */
	public static final String EXTENSION_POINT_PROJECT_CONFIGURATION_FRAGMENT = "projectConfigurationFragment"; //$NON-NLS-1$

	/** Constructor.
	 */
	private ProjectConfigurationFragments() {
		//
	}

	/** Replies the fragments that are defined as extensions in the given Eclipse plugin.
	 *
	 * @return the list of the fragment.
	 */
	public static List<ProjectConfigurationFragment> getConfigurationFragmentsFromExtension() {
		return getConfigurationFragmentStreamFromExtension().collect(Collectors.toList());
	}

	/** Replies the fragments that are defined as extensions in the given Eclipse plugin.
	 *
	 * @return the list of the fragment.
	 */
	public static Stream<ProjectConfigurationFragment> getConfigurationFragmentStreamFromExtension() {
		return Extensions.getExtensions(
				EclipseExtensionsPlugin.PLUGIN_ID, EXTENSION_POINT_PROJECT_CONFIGURATION_FRAGMENT,
				"class", ProjectConfigurationFragment.class); //$NON-NLS-1$
	}

	/** Create a source folder.
	 * 
	 * @param project the project in which a source folder must be created.
	 * @param folderPath the path of the folder to create.
	 * @param isIFolderRequired is {@code true} if the folder is required for the project.
	 * @param createFolder indicates if the source folder must be created on the hard-disk.
	 * @param monitor the progress monitor.
	 * @return the created folder
	 * @throws CoreException is thrown if the folder cannot be created.
	 */
	public static IFolder ensureSourceFolder(IProject project, String folderPath, boolean isIFolderRequired,
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

	/** Create a generated source folder.
	 * 
	 * @param project the project in which a source folder must be created.
	 * @param folderPath the path of the folder to create.
	 * @param isIFolderRequired is {@code true} if the folder is required for the project.
	 * @param createFolder indicates if the source folder must be created on the hard-disk.
	 * @param monitor the progress monitor.
	 * @return the created folder
	 * @throws CoreException is thrown if the folder cannot be created.
	 */
	public static IFolder ensureGeneratedSourceFolder(IProject project, String folderPath, boolean isIFolderRequired,
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
		try {
			folder.setDerived(true, null);
		} catch (CoreException exception) {
			//
		}
		monitor.done();
		return folder;
	}

	/** Create a folder for the output.
	 * 
	 * @param project the project in which a source folder must be created.
	 * @param folderPath the path of the folder to create.
	 * @param isIFolderRequired is {@code true} if the folder is required for the project.
	 * @param createFolder indicates if the source folder must be created on the hard-disk.
	 * @param monitor the progress monitor.
	 * @return the created folder
	 * @throws CoreException is thrown if the folder cannot be created.
	 */
	public static IFolder ensureOutputFolder(IProject project, String folderPath, boolean isIFolderRequired,
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
		try {
			folder.setDerived(true, null);
		} catch (CoreException exception) {
			//
		}
		monitor.done();
		return folder;
	}

}
