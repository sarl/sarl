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

import java.util.LinkedList;
import java.util.ResourceBundle;
import java.util.TreeSet;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.Platform;
import org.osgi.framework.BundleException;

import io.sarl.apputils.eclipseextensions.buildpath.SARLBundleBuildPath;
import io.sarl.apputils.uiextensions.Bundles;
import io.sarl.apputils.uiextensions.Bundles.IBundleDependencies;

/** Classpath dedicated to the Janus SRE environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public class JanusBundleBuildPath {

	/** Character used to separate paths on an resource name.
	 */
	private static final String NAME_SEPARATOR = "/"; //$NON-NLS-1$

	private static final String MAIN_BUNDLE_PROPERTY_NAME = "JANUS_MAIN_BUNDLE"; //$NON-NLS-1$

	private static final String BUNDLES_PROPERTY_NAME = "JANUS_BUNDLES"; //$NON-NLS-1$

	private static final String BUNDLE_PROPERTY_BASENAME = "janus-bundles"; //$NON-NLS-1$

	/** Names of the property file that contains the names of the libraries
	 * that are required to compile the SARL code and the generated Java code.
	 */
	private static String JANUS_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE;

	/** Identifier of the main bundle of the Janus SRE.
	 */
	private static String JANUS_MAIN_BUNDLE_ID;

	/** Names of the direct referenced libraries that are required to compile the Janus
	 * code and the generated Java code.
	 */
	private static String[] JANUS_ALL_DEPENDENCY_BUNDLE_NAMES;

	/** Names of the direct referenced libraries that are required to compile the Janus
	 * code and the generated Java code.
	 */
	private static String[] JANUS_DEPENDENCY_BUNDLE_NAMES;

	private static void ensureLists() {
		synchronized (JanusBundleBuildPath.class) {
			if (JANUS_ALL_DEPENDENCY_BUNDLE_NAMES == null) {
				JANUS_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE = NAME_SEPARATOR
						+ JanusBundleBuildPath.class.getPackage().getName().replace(".", NAME_SEPARATOR) //$NON-NLS-1$
						+ NAME_SEPARATOR + BUNDLE_PROPERTY_BASENAME;

				final var done0 = new TreeSet<String>();
				final var allBundleList = new LinkedList<String>();
				final var done1 = new TreeSet<String>();
				final var depsBundleList = new LinkedList<String>();

				final var jbundle = ResourceBundle.getBundle(JANUS_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE);

				JANUS_MAIN_BUNDLE_ID = jbundle.getString(MAIN_BUNDLE_PROPERTY_NAME);
				if (Strings.isNullOrEmpty(JANUS_MAIN_BUNDLE_ID)) {
					throw new IllegalStateException(MAIN_BUNDLE_PROPERTY_NAME);
				}

				boolean containsMainId = false;

				for (final var lib : jbundle.getString(BUNDLES_PROPERTY_NAME).split("[ \t\n\r\f]*,[ \\t\\n\\r\\f]*")) { //$NON-NLS-1$
					final var tlib = lib.trim();
					if (done0.add(tlib)) {
						allBundleList.add(tlib);
					}
					if (JANUS_MAIN_BUNDLE_ID.equals(tlib)) {
						containsMainId = true;
					} else if (done1.add(tlib)) {
						depsBundleList.add(tlib);
					}
				}

				for (final var lib : SARLBundleBuildPath.getSarlDependencyBundleNames()) {
					final var tlib = lib.trim();
					if (done0.add(tlib)) {
						allBundleList.add(tlib);
					}
					if (JANUS_MAIN_BUNDLE_ID.equals(tlib)) {
						containsMainId = true;
					} else if (done1.add(tlib)) {
						depsBundleList.add(tlib);
					}
				}

				if (!containsMainId) {
					throw new IllegalStateException(BUNDLES_PROPERTY_NAME);
				}

				JANUS_ALL_DEPENDENCY_BUNDLE_NAMES = new String[allBundleList.size()];
				allBundleList.toArray(JANUS_ALL_DEPENDENCY_BUNDLE_NAMES);

				JANUS_DEPENDENCY_BUNDLE_NAMES = new String[depsBundleList.size()];
				depsBundleList.toArray(JANUS_DEPENDENCY_BUNDLE_NAMES);
			}
		}
	}

	/** Replies the list of the standard Janus dependencies.
	 *
	 * @return the list of dependencies.
	 * @see #getJanusPlatformClasspath()
	 */
	public static String[] getJanusDependencyBundleNames() {
		ensureLists();
		return JANUS_ALL_DEPENDENCY_BUNDLE_NAMES;
	}

	/** Replies the standard classpath for running the Janus platform.
	 *
	 * @return the classpath.
	 * @throws BundleException if a bundle cannot be resolved.
	 * @see #getJanusDependencyBundleNames()
	 */
	public static IBundleDependencies getJanusPlatformClasspath() throws BundleException {
		ensureLists();
		final var bundle = Platform.getBundle(JANUS_MAIN_BUNDLE_ID);
		final var resolvedBundles = Bundles.resolveBundleDependenciesWithExtras(bundle, JANUS_ALL_DEPENDENCY_BUNDLE_NAMES);
		return resolvedBundles;
	}

	/** Replies the name of the main bundle for the standard Janus SRE.
	 *
	 * @return the mname of the main Janus bundle.
	 */
	public static String getJanusMainBundleName() {
		ensureLists();
		return JANUS_MAIN_BUNDLE_ID;
	}

}
