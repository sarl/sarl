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

package io.sarl.sre.eclipse.buildpath;

import java.util.HashSet;
import java.util.ResourceBundle;
import java.util.Set;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.osgi.framework.Bundle;

import io.sarl.eclipse.buildpath.AbstractSARLBasedClasspathContainer;
import io.sarl.eclipse.buildpath.SARLClasspathContainer;
import io.sarl.eclipse.util.BundleUtil;
import io.sarl.eclipse.util.BundleUtil.IBundleDependencies;
import io.sarl.eclipse.util.Utilities.SARLBundleJavadocURLMappings;

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

	private static final String ECLIPSE_BUNDLES_PROPERTY_NAME = "JANUS_ECLIPSE_BUNDLES"; //$NON-NLS-1$

	private static final String JAVADOC_URL = "http://www.sarl.io/docs/api/"; //$NON-NLS-1$

	static {
		JANUS_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE = SLASH
				+ JanusClasspathContainer.class.getPackage().getName().replace(POINT, SLASH)
				+ SLASH + BUNDLE_PROPERTY_BASENAME;
		final ResourceBundle bundle = ResourceBundle.getBundle(JANUS_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE);

		final Set<String> libs = new HashSet<>();

		for (final String lib : SARLClasspathContainer.SARL_DEPENDENCY_BUNDLE_NAMES) {
			libs.add(lib);
		}

		JANUS_MAIN_BUNDLE_ID = bundle.getString(MAIN_BUNDLE_PROPERTY_NAME);
		if (Strings.isNullOrEmpty(JANUS_MAIN_BUNDLE_ID)) {
			throw new IllegalStateException(MAIN_BUNDLE_PROPERTY_NAME);
		}

		for (final String lib : bundle.getString(BUNDLES_PROPERTY_NAME).split(VALUE_SEPARATOR)) {
			libs.add(lib.trim());
		}
		if (!libs.contains(JANUS_MAIN_BUNDLE_ID)) {
			throw new IllegalStateException(BUNDLES_PROPERTY_NAME);
		}

		for (final String lib : bundle.getString(ECLIPSE_BUNDLES_PROPERTY_NAME).split(VALUE_SEPARATOR)) {
			libs.add(lib.trim());
		}

		String[] allLibs = new String[libs.size()];
		allLibs = libs.toArray(allLibs);
		JANUS_DEPENDENCY_BUNDLE_NAMES = allLibs;
	}

	/** Constructor.
	 *
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the associated JAva project.
	 */
	public JanusClasspathContainer(IPath containerPath, IJavaProject javaProject) {
		super(containerPath, javaProject);
	}

	@Override
	public int getKind() {
		return K_APPLICATION;
	}

	/** Replies the standard classpath for running the Janus platform.
	 *
	 * @return the classpath.
	 */
	public static IBundleDependencies getJanusPlatformClasspath() {
		final Bundle bundle = Platform.getBundle(JANUS_MAIN_BUNDLE_ID);
		final IBundleDependencies resolvedBundles = BundleUtil.resolveBundleDependencies(bundle,
				new JanusBundleJavadocURLMappings(),
				JANUS_DEPENDENCY_BUNDLE_NAMES);
		return resolvedBundles;
	}

	@Override
	protected void updateBundleList(Set<String> entries) {
		for (final String symbolicName : getJanusPlatformClasspath().getTransitiveSymbolicNames(true)) {
			entries.add(symbolicName);
		}
	}

	@Override
	protected void updateClasspathEntries(Set<IClasspathEntry> entries) {
		for (final IClasspathEntry cpe : getJanusPlatformClasspath().getTransitiveClasspathEntries(true)) {
			entries.add(cpe);
		}
	}

	@Override
	public String getDescription() {
		return Messages.JanusClasspathContainer_0;
	}

	/** Define a mapping from bundles to URLs.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class JanusBundleJavadocURLMappings extends SARLBundleJavadocURLMappings {

		private static final String JANUS_PREFIX = "io.janusproject."; //$NON-NLS-1$

		@Override
		public String getURLForBundle(Bundle bundle) {
			if (bundle.getSymbolicName().startsWith(JANUS_PREFIX)) {
				return JAVADOC_URL;
			}
			return super.getURLForBundle(bundle);
		}

	}

}
