/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
package io.sarl.eclipse.tests.api;

import java.util.List;

import com.google.inject.Injector;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.xtext.ui.util.PluginProjectFactory;
import org.eclipse.xtext.util.JavaVersion;

/** Factory of a project.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.tests.api.ui 0.13.0 20230919-093100
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.tests.api.ui
 */
public interface ProjectCreator {

	/** Replies the injector.
	 * 
	 * @return the injector.
	 */
	Injector getInjector();
	
	/** Replies the project factory.
	 *
	 * @return the factory.
	 */
	PluginProjectFactory getProjectFactory();
	
	/** Replies the version of Java that is supported by the project.
	 *
	 * @return the java version.
	 */
	JavaVersion getJavaVersion();

	/** Replies the source folders.
	 *
	 * @return the source folders.
	 */
	List<String> getSourceFolders();
	
	/** Replies the folder in which the sources are generated.
	 *
	 * @return the generation folder.
	 */
	String getGenerationFolder();

	/** Replies the identifiers of the builders.
	 *
	 * @return the identifiers.
	 */
	String[] getBuilderIds();

	/** Replies the identifiers of the natures
	 *
	 * @return the identifiers.
	 */
	String[] getNatures();

	/** Add a JRE library to the class path.
	 *
	 * @param javaProject the proejct to update.
	 * @throws JavaModelException
	 */
	void addJreClasspathEntry(IJavaProject javaProject) throws JavaModelException;
	
	/** Add a library to the class path.
	 *
	 * @param javaProject the proejct to update.
	 * @param newClassPathEntry the entry to add.
	 * @throws JavaModelException
	 */
	void addToClasspath(IJavaProject javaProject, IClasspathEntry newClassPathEntry) throws JavaModelException;

	/** Add libraries to the class path.
	 *
	 * @param javaProject the proejct to update.
	 * @param autobuild indicates if the function should wait for end of autobuild.
	 * @param newClassPathEntry the entry to add.
	 * @throws JavaModelException
	 */
	void addToClasspath(IJavaProject javaProject,
			boolean autobuild,
			Iterable<IClasspathEntry> newClassPathEntry) throws JavaModelException;

}
