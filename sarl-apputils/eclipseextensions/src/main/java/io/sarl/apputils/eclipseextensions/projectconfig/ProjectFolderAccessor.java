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

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Tools for managing the project folders.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version eclipseextensions 0.15.1 20250911-224825
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid eclipseextensions
 * @since 0.15.1
 */
public interface ProjectFolderAccessor {

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
	IFolder ensureSourceFolder(IProject project, String folderPath, boolean isIFolderRequired,
			boolean createFolder, IProgressMonitor monitor) throws CoreException;

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
	IFolder ensureGeneratedSourceFolder(IProject project, String folderPath, boolean isIFolderRequired,
			boolean createFolder, IProgressMonitor monitor) throws CoreException;

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
	IFolder ensureOutputFolder(IProject project, String folderPath, boolean isIFolderRequired,
			boolean createFolder, IProgressMonitor monitor) throws CoreException;

}
