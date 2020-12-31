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

package io.sarl.eclipse.launching.sreproviding;

import java.util.Collections;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.launching.RuntimeClasspathEntry;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.xtext.util.Strings;

import io.sarl.eclipse.runtime.ProjectSREProvider;
import io.sarl.eclipse.runtime.ProjectSREProviderFactory;
import io.sarl.eclipse.runtime.SARLRuntime;

/** Factory of the default implementation of a project SRE provider.
 * This provider is reading the JRE service definitions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see EclipseIDEProjectSREProvider
 */
public class JreServiceProjectSREProviderFactory implements ProjectSREProviderFactory {

	/** Construct a factory of SRE provider.
	 */
	public JreServiceProjectSREProviderFactory() {
		//
	}

	@Override
	public ProjectSREProvider getProjectSREProvider(IProject project) {
		final IJavaProject javaProject = JavaCore.create(project);
		return findOnClasspath(javaProject);
	}

	private static ProjectSREProvider findOnClasspath(IJavaProject project) {
		try {
			final IClasspathEntry[] classpath = project.getResolvedClasspath(true);
			for (final IClasspathEntry entry : classpath) {
				final IPath path;
				final String id;
				final String name;
				switch (entry.getEntryKind()) {
				case IClasspathEntry.CPE_LIBRARY:
					path = entry.getPath();
					if (path != null) {
						id = path.makeAbsolute().toPortableString();
						name = path.lastSegment();
					} else {
						id  = null;
						name = null;
					}
					break;
				case IClasspathEntry.CPE_PROJECT:
					final IResource res = project.getProject().getParent().findMember(entry.getPath());
					if (res instanceof IProject) {
						final IProject dependency = (IProject) res;
						final IJavaProject javaDependency = JavaCore.create(dependency);
						path = javaDependency.getOutputLocation();
						id = javaDependency.getHandleIdentifier();
						name = javaDependency.getElementName();
						break;
					}
					continue;
				default:
					continue;
				}
				if (path != null && !SARLRuntime.isPackedSRE(path) && !SARLRuntime.isUnpackedSRE(path)) {
					final String bootstrap = SARLRuntime.getDeclaredBootstrap(path);
					if (!Strings.isEmpty(bootstrap)) {
						final List<IRuntimeClasspathEntry> 	classpathEntries = Collections.singletonList(new RuntimeClasspathEntry(entry));
						return new JreServiceProjectSREProvider(
								id, name,
								project.getPath().toPortableString(),
								bootstrap,
								classpathEntries);
					}
				}
			}
		} catch (JavaModelException exception) {
			//
		}
		return null;
	}

}
