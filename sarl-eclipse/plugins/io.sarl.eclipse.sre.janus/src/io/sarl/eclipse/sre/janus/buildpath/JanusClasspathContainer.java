/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.eclipse.sre.janus.buildpath;

import java.util.Comparator;
import java.util.HashSet;
import java.util.ResourceBundle;
import java.util.Set;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.osgi.framework.BundleException;

import io.sarl.apputils.eclipseextensions.buildpath.AbstractSARLBasedClasspathContainer;
import io.sarl.apputils.eclipseextensions.buildpath.SARLBundleBuildPath;
import io.sarl.apputils.uiextensions.Bundles;
import io.sarl.apputils.uiextensions.Bundles.IBundleDependencies;

/** Classpath container dedicated to the Janus platform.
 *
 * <p>The Janus classpath library is an application library, i.e. it is included into the run-time classpath.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusClasspathContainer extends AbstractSARLBasedClasspathContainer {

	/** Name of the property file that contains the reference libraries that are required run
	 * within Eclipse IDE any SARL-based Java code based on Janus.
	 */
	public static final String JANUS_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE;

	/** Names of the reference libraries that are required to run within Eclipse IDE any SARL-based Java code based on Janus.
	 */
	public static final String[] JANUS_DEPENDENCY_BUNDLE_NAMES;

	/** Identifier of the main bundle of the Janus SRE.
	 */
	public static final String JANUS_MAIN_BUNDLE_ID;

	private static final String BUNDLE_PROPERTY_BASENAME = "janus-bundles"; //$NON-NLS-1$

	private static final String SLASH = "/"; //$NON-NLS-1$

	private static final String POINT = "."; //$NON-NLS-1$

	private static final String VALUE_SEPARATOR = "[ \\t\\n\\r\\f]*,[ \\\\t\\\\n\\\\r\\\\f]*"; //$NON-NLS-1$

	private static final String MAIN_BUNDLE_PROPERTY_NAME = "JANUS_MAIN_BUNDLE"; //$NON-NLS-1$

	private static final String BUNDLES_PROPERTY_NAME = "JANUS_BUNDLES"; //$NON-NLS-1$

	static {
		JANUS_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE = SLASH
				+ JanusClasspathContainer.class.getPackage().getName().replace(POINT, SLASH)
				+ SLASH + BUNDLE_PROPERTY_BASENAME;
		final var bundle = ResourceBundle.getBundle(JANUS_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE);

		final var libs = new HashSet<String>();
		
		for (final var lib : SARLBundleBuildPath.getSarlDependencyBundleNames()) {
			libs.add(lib);
		}

		JANUS_MAIN_BUNDLE_ID = bundle.getString(MAIN_BUNDLE_PROPERTY_NAME);
		if (Strings.isNullOrEmpty(JANUS_MAIN_BUNDLE_ID)) {
			throw new IllegalStateException(MAIN_BUNDLE_PROPERTY_NAME);
		}

		for (final var lib : bundle.getString(BUNDLES_PROPERTY_NAME).split(VALUE_SEPARATOR)) {
			libs.add(lib.trim());
		}
		if (!libs.contains(JANUS_MAIN_BUNDLE_ID)) {
			throw new IllegalStateException(BUNDLES_PROPERTY_NAME);
		}

		var allLibs = new String[libs.size()];
		allLibs = libs.toArray(allLibs);
		JANUS_DEPENDENCY_BUNDLE_NAMES = allLibs;
	}

	/** Constructor.
	 *
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the associated JAva project.
	 */
	public JanusClasspathContainer(IPath containerPath, IJavaProject javaProject) {
		this(containerPath, javaProject, null);
	}

	/** Constructor.
	 *
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the associated JAva project.
	 * @param entryComparator the comparator of classpath entries to use.
	 */
	public JanusClasspathContainer(IPath containerPath, IJavaProject javaProject, Comparator<IClasspathEntry> entryComparator) {
		super(containerPath, javaProject, entryComparator);
	}

	@Override
	public int getKind() {
		return K_APPLICATION;
	}

	/** Replies the standard classpath for running the Janus platform.
	 *
	 * @return the classpath.
	 * @throws BundleException if a bundle cannot be resolved.
	 */
	public static IBundleDependencies getJanusPlatformClasspath() throws BundleException {
		final var bundle = Platform.getBundle(JANUS_MAIN_BUNDLE_ID);
		final var resolvedBundles = Bundles.resolveBundleDependencies(bundle);
		return resolvedBundles;
	}

	@Override
	protected void updateBundleList(Set<String> entries) throws BundleException {
		for (final var symbolicName : getJanusPlatformClasspath().getTransitiveSymbolicNames(true)) {
			entries.add(symbolicName);
		}
	}

	@Override
	protected void updateClasspathEntries(Set<IClasspathEntry> entries) throws BundleException {
		for (final var cpe : getJanusPlatformClasspath().getTransitiveClasspathEntries(true)) {
			entries.add(cpe);
		}
	}

	@Override
	public String getDescription() {
		return Messages.JanusClasspathContainer_0;
	}

}
