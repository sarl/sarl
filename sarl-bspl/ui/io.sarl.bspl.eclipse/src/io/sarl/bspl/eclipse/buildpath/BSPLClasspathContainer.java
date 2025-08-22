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

package io.sarl.bspl.eclipse.buildpath;

import java.util.Comparator;
import java.util.HashSet;
import java.util.ResourceBundle;
import java.util.Set;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;

import io.sarl.apputils.eclipseextensions.buildpath.AbstractSARLBasedClasspathContainer;
import io.sarl.apputils.eclipseextensions.buildpath.SARLBundleBuildPath;
import io.sarl.apputils.uiextensions.Bundles;
import io.sarl.apputils.uiextensions.Bundles.IBundleDependencies;

/** Classpath container dedicated to the BSPL API.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class BSPLClasspathContainer extends AbstractSARLBasedClasspathContainer {

	/** Name of the property file that contains the reference libraries that are required run
	 * within Eclipse IDE any BSPL-based Java code.
	 */
	public static final String BSPL_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE;

	/** Names of the reference libraries that are required to run within Eclipse IDE any BSPL-based Java code.
	 */
	public static final String[] BSPL_DEPENDENCY_BUNDLE_NAMES;

	/** Identifier of the main bundle of the BSPL.
	 */
	public static final String BSPL_MAIN_BUNDLE_ID;

	private static final String BUNDLE_PROPERTY_BASENAME = "bspl-bundles"; //$NON-NLS-1$

	private static final String SLASH = "/"; //$NON-NLS-1$

	private static final String POINT = "."; //$NON-NLS-1$

	private static final String VALUE_SEPARATOR = "[ \\t\\n\\r\\f]*,[ \\\\t\\\\n\\\\r\\\\f]*"; //$NON-NLS-1$

	private static final String MAIN_BUNDLE_PROPERTY_NAME = "BSPL_MAIN_BUNDLE"; //$NON-NLS-1$

	private static final String BUNDLES_PROPERTY_NAME = "BSPL_BUNDLES"; //$NON-NLS-1$

	static {
		BSPL_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE = SLASH
				+ BSPLClasspathContainer.class.getPackage().getName().replace(POINT, SLASH)
				+ SLASH + BUNDLE_PROPERTY_BASENAME;
		final var bundle = ResourceBundle.getBundle(BSPL_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE);

		final var libs = new HashSet<String>();
		
		for (final var lib : SARLBundleBuildPath.getSarlDependencyBundleNames()) {
			libs.add(lib);
		}

		BSPL_MAIN_BUNDLE_ID = bundle.getString(MAIN_BUNDLE_PROPERTY_NAME);
		if (Strings.isNullOrEmpty(BSPL_MAIN_BUNDLE_ID)) {
			throw new IllegalStateException(MAIN_BUNDLE_PROPERTY_NAME);
		}

		for (final var lib : bundle.getString(BUNDLES_PROPERTY_NAME).split(VALUE_SEPARATOR)) {
			libs.add(lib.trim());
		}
		if (!libs.contains(BSPL_MAIN_BUNDLE_ID)) {
			throw new IllegalStateException(BUNDLES_PROPERTY_NAME);
		}

		var allLibs = new String[libs.size()];
		allLibs = libs.toArray(allLibs);
		BSPL_DEPENDENCY_BUNDLE_NAMES = allLibs;
	}

	/** Constructor.
	 *
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the associated JAva project.
	 */
	public BSPLClasspathContainer(IPath containerPath, IJavaProject javaProject) {
		super(containerPath, javaProject, null);
	}

	/** Constructor.
	 *
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the associated JAva project.
	 * @param entryComparator the comparator of classpath entries to use.
	 */
	public BSPLClasspathContainer(IPath containerPath, IJavaProject javaProject, Comparator<IClasspathEntry> entryComparator) {
		super(containerPath, javaProject, entryComparator);
	}

	@Override
	public int getKind() {
		if (isModular()) {
			return K_SYSTEM;
		}
		return K_APPLICATION;
	}

	/** Replies the standard classpath for running the BSPL API.
	 *
	 * @return the classpath.
	 */
	public static IBundleDependencies getBsplClasspath() {
		final var bundle = Platform.getBundle(BSPL_MAIN_BUNDLE_ID);
		final var resolvedBundles = Bundles.resolveBundleDependencies(bundle);
		return resolvedBundles;
	}

	@Override
	protected void updateBundleList(Set<String> entries) {
		for (final var symbolicName : getBsplClasspath().getTransitiveSymbolicNames(false)) {
			entries.add(symbolicName);
		}
	}

	@Override
	protected void updateClasspathEntries(Set<IClasspathEntry> entries) {
		for (final var cpe : getBsplClasspath().getTransitiveClasspathEntries(false)) {
			entries.add(cpe);
		}
	}

	@Override
	public String getDescription() {
		return Messages.BSPLClasspathContainer_0;
	}

}
