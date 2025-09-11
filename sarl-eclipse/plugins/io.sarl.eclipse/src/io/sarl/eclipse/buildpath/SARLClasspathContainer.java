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

package io.sarl.eclipse.buildpath;

import static io.sarl.apputils.eclipseextensions.buildpath.SARLBundleBuildPath.getSarlDependencyBundleNames;

import java.text.MessageFormat;
import java.util.Comparator;
import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;

import io.sarl.apputils.eclipseextensions.buildpath.AbstractSARLBasedClasspathContainer;
import io.sarl.apputils.uiextensions.Bundles;
import io.sarl.eclipse.SARLEclipsePlugin;

/** Classpath container dedicated to the SARL environment.
 *
 * <p>The SARL classpath container is a system library, i.e. it will not be included into the run-time
 * classpath.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLClasspathContainer extends AbstractSARLBasedClasspathContainer {

	/** Constructor.
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the reference to the containing Java project
	 * @since 0.12
	 */
	public SARLClasspathContainer(IPath containerPath, IJavaProject javaProject) {
		this(containerPath, javaProject, null);
	}

	/** Constructor.
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the reference to the containing Java project
	 * @param entryComparator the comparator of classpath entries to use.
	 * @since 0.15
	 */
	public SARLClasspathContainer(IPath containerPath, IJavaProject javaProject, Comparator<IClasspathEntry> entryComparator) {
		super(containerPath, javaProject, entryComparator);
	}

	@Override
	public int getKind() {
		// In modular Java (9 or higher): Must be K_APPLICATION in order to be included into the modulepath
		// or the classpath.
		//
		// In not modular Java (8): Must be K_SYSTEM in order to let the run-configuration launcher to replace the SARL
		// libraries by the SRE libraries.
		if (isModular()) {
			return K_APPLICATION;
		}
		return K_SYSTEM;
	}

	@Override
	protected void updateBundleList(Set<String> entries) {
		for (final var rootBundleName : getSarlDependencyBundleNames()) {
			final var bundle = Platform.getBundle(rootBundleName);
			if (bundle != null) {
				for (final var symbolicName : Bundles.resolveBundleDependencies(bundle).getTransitiveSymbolicNames(true)) {
					entries.add(symbolicName);
				}
			} else {
				SARLEclipsePlugin.getDefault().logErrorMessage(MessageFormat.format(
						Messages.SARLClasspathContainer_1, rootBundleName));
			}
		}
	}

	@Override
	protected void updateClasspathEntries(Set<IClasspathEntry> entries) {
		for (final var rootBundleName : getSarlDependencyBundleNames()) {
			final var bundle = Platform.getBundle(rootBundleName);
			if (bundle != null) {
				for (final var entry : Bundles.resolveBundleDependencies(bundle).getTransitiveClasspathEntries(true)) {
					entries.add(entry);
				}
			} else {
				SARLEclipsePlugin.getDefault().logErrorMessage(MessageFormat.format(
						Messages.SARLClasspathContainer_1, rootBundleName));
			}
		}
	}

	@Override
	public String getDescription() {
		return Messages.SARLClasspathContainer_0;
	}

}
