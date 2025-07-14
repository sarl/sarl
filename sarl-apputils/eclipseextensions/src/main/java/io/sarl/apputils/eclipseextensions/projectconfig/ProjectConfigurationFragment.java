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

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Fragment of the configuration for a SARL project.
 *
 * <p>This fragment may be used to add specific configuration elements to a SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public interface ProjectConfigurationFragment {

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
			List<IFolder> sourcePaths,
			List<IFolder> testSourcePaths,
			List<IFolder> generationPaths,
			List<IFolder> testGenerationPaths) throws CoreException;

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

}
