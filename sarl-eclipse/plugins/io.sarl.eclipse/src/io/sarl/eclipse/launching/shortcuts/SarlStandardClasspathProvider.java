/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.launching.shortcuts;

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.StandardClasspathProvider;

import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.runtime.SARLRuntime;

/** Classpath provider for SARL. This provider add the SRE classpath to the project's classpath.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlStandardClasspathProvider extends StandardClasspathProvider {

	/** Identifier of the classpath provider.
	 */
	public static final String EXTENSION_IDENTIFIER = SarlStandardClasspathProvider.class.getName();

	@Override
	public IRuntimeClasspathEntry[] computeUnresolvedClasspath(ILaunchConfiguration configuration)
			throws CoreException {
		var classpath = super.computeUnresolvedClasspath(configuration);
		final var sre = SARLRuntime.getDefaultSREInstall();
		if (sre != null) {
			final var containerId = sre.getPreferredClassPathContainerPath();
			if (containerId != null && !containerId.equals(SARLClasspathContainerInitializer.CONTAINER_ID)) {
				final var library = JavaCore.newContainerEntry(containerId);
				final var runtimeEntry = JavaRuntime.newRuntimeContainerClasspathEntry(
						library.getPath(), IRuntimeClasspathEntry.USER_CLASSES);
				final var newClasspath = new IRuntimeClasspathEntry[classpath.length + 1];
				System.arraycopy(classpath, 0, newClasspath, 0, classpath.length);
				newClasspath[classpath.length] = runtimeEntry;
				classpath = newClasspath;
			} else {
				final var entries = sre.getClassPathEntries();
				final var newClasspath = new IRuntimeClasspathEntry[classpath.length + entries.size()];
				System.arraycopy(classpath, 0, newClasspath, 0, classpath.length);
				final var iterator = entries.iterator();
				for (var j = classpath.length; j < newClasspath.length && iterator.hasNext(); ++j) {
					newClasspath[j] = iterator.next();
				}
				classpath = newClasspath;
			}
		}
		return classpath;
	}

}
