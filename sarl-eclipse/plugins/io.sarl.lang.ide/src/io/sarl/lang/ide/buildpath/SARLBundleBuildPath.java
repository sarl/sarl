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

package io.sarl.lang.ide.buildpath;

import java.util.LinkedList;
import java.util.ResourceBundle;
import java.util.TreeSet;

/** Classpath dedicated to the SARL environment that is independent of the UI.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since0.13
 */
public class SARLBundleBuildPath {

	/** Character used to separate paths on an resource name.
	 */
	private static final String NAME_SEPARATOR = "/"; //$NON-NLS-1$

	/** Names of the property file that contains the names of the libraries
	 * that are required to compile the SARL code and the generated Java code.
	 */
	private static String SARL_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE;

	/** Names of the direct referenced libraries that are required to compile the SARL
	 * code and the generated Java code.
	 */
	private static String[] SARL_DEPENDENCY_BUNDLE_NAMES;

	private static void ensureLists() {
		synchronized (SARLBundleBuildPath.class) {
			if (SARL_DEPENDENCY_BUNDLE_NAMES == null) {
				SARL_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE = NAME_SEPARATOR
						+ SARLBundleBuildPath.class.getPackage().getName().replace(".", NAME_SEPARATOR) //$NON-NLS-1$
						+ NAME_SEPARATOR + "sarl-bundles"; //$NON-NLS-1$

				final var done = new TreeSet<String>();
				final var bundleList = new LinkedList<String>();

				final var jbundle = ResourceBundle.getBundle(SARL_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE);
				for (final var lib : jbundle.getString("SARL_BUNDLES").split("[ \t\n\r\f]*,[ \\t\\n\\r\\f]*")) { //$NON-NLS-1$ //$NON-NLS-2$
					if (done.add(lib)) {
						bundleList.add(lib);
					}
				}

				SARL_DEPENDENCY_BUNDLE_NAMES = new String[bundleList.size()];
				bundleList.toArray(SARL_DEPENDENCY_BUNDLE_NAMES);
			}
		}
	}

	/** Replies the list of the standard SARL dependencies for SDK.
	 *
	 * @return the list of dependencies.
	 */
	public static String[] getSarlDependencyBundleNames() {
		ensureLists();
		return SARL_DEPENDENCY_BUNDLE_NAMES;
	}

}
