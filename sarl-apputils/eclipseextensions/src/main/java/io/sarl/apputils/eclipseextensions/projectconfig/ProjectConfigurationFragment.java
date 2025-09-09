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
import java.util.SortedSet;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;

/**
 * Fragment of the configuration for a SARL project.
 *
 * <p>This fragment may be used to add specific configuration elements to a SARL project.
 *
 * @author $Author: sgalland$
 * @version eclipseextensions 0.15.0 20250909-115749
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid eclipseextensions
 * @since 0.15
 */
public interface ProjectConfigurationFragment {

	/** Replies the identifier for this project fragment
	 *
	 * @return the identifier, never {@code null}.
	 */
	String getId();

	/** Replies if the contribution of this fragment should be active by default when opening the project configurator.
	 *
	 * @return {@code true} if the fragment is active by default in the configurator interface.
	 */
	boolean isActiveByDefault();

	/** Replies the label that is briefly describing this project configuration fragment.
	 *
	 * @return the label, never {@code null}.
	 */
	String getLabel();
	
	/** Update the natures of the given project.
	 *
	 * @param project the project to update.
	 * @param monitor the progress monitor.
	 */
	void updateNatures(IProject project, IProgressMonitor monitor);

	/** Update the lists of source folders.
	 *
	 * @param project the project to update.
	 * @param createFolders indicates if the source folder must be created on the hard-disk.
	 * @param monitor the progress monitor.
	 * @param sourcePaths the list of the regular source folders.
	 * @param testSourcePaths the list of the test source folders.
	 * @param generationPaths the list of the regular source folders in which generated code is written.
	 * @param testGenerationPaths the list of the test source folders in which generated code is written.
	 * @throws CoreException if there is an error related to one of the source folders.
	 */
	void updateSourceFolders(IProject project, boolean createFolders, IProgressMonitor monitor,
			SortedSet<IFolder> sourcePaths,
			SortedSet<IFolder> testSourcePaths,
			SortedSet<IFolder> generationPaths,
			SortedSet<IFolder> testGenerationPaths) throws CoreException;

	/** Update the specific configuration of the project.
	 *
	 * @param project the project to update.
	 * @param monitor the progress monitor.
	 * @param generationFolderPath the folder in which the generated regular code is written.
	 * @param testGenerationFolderPath the folder in which the generated test code is written.
	 */
	void updateSpecificConfiguration(IProject project, IProgressMonitor monitor, IPath generationFolderPath,
			IPath testGenerationFolderPath);

	/** Unconfigure the given project.
	 *
	 * @param project the project to update.
	 * @param monitor the progress monitor.
	 * @param natures the natures to associate to the project after unconfiguring it.
	 */
	void unconfigure(IProject project, IProgressMonitor monitor, List<String> natures);

	/** Replies the default source entries for the project configurator.
	 *
	 * @param projectFolder the folder of the project.
	 * @return the classpath entries.
	 */
	List<IClasspathEntry> getDefaultSourceClassPathEntries(IPath projectFolder);

	/** Replies the default binary entries or libraries for the project configurator.
	 *
	 * @param projectFolder the folder of the project.
	 * @return the classpath entries.
	 */
	List<IClasspathEntry> getBinaryClassPathEntries(IPath projectFolder);

}
