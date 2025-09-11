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

package io.sarl.eclipse.sre.janus.network;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.ResourceBundle;
import java.util.TreeSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.StandardClasspathProvider;

import io.sarl.apputils.uiextensions.Bundles;

/** Classpath provider dedicated to the Janus networking features.
 *
 * <p>The Janus networking library is an application library, i.e. it is included into the run-time classpath.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.sre.janus 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.sre.janus
 * @since 0.12
 */
public class JanusNetworkClasspathProvider extends StandardClasspathProvider {

	/** Name of the property file that contains the reference libraries that are required to run
	 * within Eclipse IDE any SARL-based Java code based on Janus.
	 */
	public static final String JANUS_NETWORK_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE;

	/** Names of the reference libraries that are required to run within Eclipse IDE any SARL-based Java code based on Janus.
	 */
	public static final String[] JANUS_NETWORK_DEPENDENCY_BUNDLE_NAMES;

	private IRuntimeClasspathEntry[] unresolved;

	static {
		JANUS_NETWORK_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE = "/" //$NON-NLS-1$
				+ JanusNetworkClasspathProvider.class.getPackage().getName().replace(".", "/") //$NON-NLS-1$//$NON-NLS-2$
				+ "/janus-network-bundles"; //$NON-NLS-1$
		final var bundle = ResourceBundle.getBundle(JANUS_NETWORK_DEPENDENCY_BUNDLE_NAMES_PROPERTY_FILE);

		final var libs = new HashSet<String>();

		for (final var lib : bundle.getString("JANUS_NETWORK_BUNDLES").split("[ \t\n\r\f]*,[ \\t\\n\\r\\f]*")) { //$NON-NLS-1$//$NON-NLS-2$
			libs.add(lib.trim());
		}

		var allLibs = new String[libs.size()];
		allLibs = libs.toArray(allLibs);
		JANUS_NETWORK_DEPENDENCY_BUNDLE_NAMES = allLibs;
	}

	@Override
	public IRuntimeClasspathEntry[] computeUnresolvedClasspath(ILaunchConfiguration configuration)
			throws CoreException {
		if (this.unresolved == null) {
			final var entries = new ArrayList<IRuntimeClasspathEntry>();
			final var added = new TreeSet<String>();
			for (final var bundleId : JANUS_NETWORK_DEPENDENCY_BUNDLE_NAMES) {
				final var bundle = Platform.getBundle(bundleId);
				if (bundle != null) {
					final var resolvedBundles = Bundles.resolveBundleDependencies(bundle);
					if (resolvedBundles != null) {
						for (final var entry : resolvedBundles.getTransitiveRuntimeClasspathEntries(true)) {
							final var location = entry.getLocation();
							if (added.add(location)) {
								entries.add(entry);
							}
						}
					}
				}
			}
			this.unresolved = entries.toArray(new IRuntimeClasspathEntry[entries.size()]);
		}
		return this.unresolved;
	}

}
