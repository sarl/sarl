/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.janusproject.eclipse.buildpath;

import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.osgi.framework.Bundle;

import io.janusproject.eclipse.JanusEclipsePlugin;

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

	/** Names of the reference libraries that are required to compile the Janus
	 * code and run any SARL-based Java code.
	 */
	public static final String[] JANUS_ROOT_BUNDLE_NAMES;

	static {
		final String[] array1 = new String[] {
			JanusEclipsePlugin.JANUS_KERNEL_PLUGIN_ID,
			"com.google.code.gson", //$NON-NLS-1$
			"com.google.code.gson", //$NON-NLS-1$
			"com.google.inject", //$NON-NLS-1$
			"com.hazelcast", //$NON-NLS-1$
			"org.apache.commons.cli", //$NON-NLS-1$
			"org.arakhne.afc.core.vmutils", //$NON-NLS-1$
			"org.arakhne.afc.core.util", //$NON-NLS-1$
			"org.eclipse.osgi", //$NON-NLS-1$
			"org.zeromq.jeromq", //$NON-NLS-1$
		};
		final String[] array = new String[SARLClasspathContainer.SARL_ROOT_BUNDLE_NAMES.length + array1.length];
		System.arraycopy(SARLClasspathContainer.SARL_ROOT_BUNDLE_NAMES, 0, array, 0, SARLClasspathContainer.SARL_ROOT_BUNDLE_NAMES.length);
		System.arraycopy(array1, 0, array, SARLClasspathContainer.SARL_ROOT_BUNDLE_NAMES.length, array1.length);
		JANUS_ROOT_BUNDLE_NAMES = array;
	}

	private static final String JAVADOC_URL = "http://www.janusproject.io/apidocs/"; //$NON-NLS-1$

	/** Constructor.
	 *
	 * @param containerPath the path of the container, e.g. the project.
	 */
	public JanusClasspathContainer(IPath containerPath) {
		super(containerPath);
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
		final Bundle bundle = Platform.getBundle(JanusEclipsePlugin.JANUS_KERNEL_PLUGIN_ID);
		return BundleUtil.resolveBundleDependencies(bundle,
				new JanusBundleJavadocURLMappings(),
				JANUS_ROOT_BUNDLE_NAMES);
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
