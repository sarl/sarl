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

package io.sarl.eclipse.buildpath;

import java.text.MessageFormat;
import java.util.ResourceBundle;
import java.util.Set;

import org.arakhne.afc.vmutil.Resources;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.osgi.framework.Bundle;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.BundleUtil;

/** Classpath container dedicated to the SARL environment.
 *
 * <p>The SARL classpath container is a system library, i.e. it will not be included into the run-time
 * classpath.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLClasspathContainer extends AbstractSARLBasedClasspathContainer {

	/** Names of the property file that contains the names of the libraries
	 * that are required to compile the SARL code and the generated Java code.
	 */
	public static final String SARL_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE;

	/** Names of the direct referenced libraries that are required to compile the SARL
	 * code and the generated Java code.
	 */
	public static final String[] SARL_DEPENDENCY_BUNDLE_NAMES;

	static {
		SARL_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE = Resources.translateResourceName(
				Resources.NAME_SEPARATOR
				+ SARLClasspathContainer.class.getPackage().getName().replace(".", Resources.NAME_SEPARATOR) //$NON-NLS-1$
				+ Resources.NAME_SEPARATOR + "sarl-bundles"); //$NON-NLS-1$
		final ResourceBundle bundle = ResourceBundle.getBundle(SARL_DEPENDENCY_BUNDLE_NAMES_RESOURCE_FILE);
		final String[] libs = bundle.getString("SARL_BUNDLES").split("[ \t\n\r\f]*,[ \\t\\n\\r\\f]*"); //$NON-NLS-1$
		for (int i = 0; i < libs.length; ++i) {
			libs[i] = libs[i].trim();
		}
		SARL_DEPENDENCY_BUNDLE_NAMES = libs;
	}

	/** Constructor.
	 * @param containerPath the path of the container, e.g. the project.
	 * @param javaProject the reference to the containing Java project
	 * @since 0.12
	 */
	public SARLClasspathContainer(IPath containerPath, IJavaProject javaProject) {
		super(containerPath, javaProject);
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
		for (final String rootBundleName : SARL_DEPENDENCY_BUNDLE_NAMES) {
			final Bundle bundle = Platform.getBundle(rootBundleName);
			if (bundle != null) {
				for (final String symbolicName : BundleUtil.resolveBundleDependencies(bundle).getTransitiveSymbolicNames(true)) {
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
		for (final String rootBundleName : SARL_DEPENDENCY_BUNDLE_NAMES) {
			final Bundle bundle = Platform.getBundle(rootBundleName);
			if (bundle != null) {
				for (final IClasspathEntry entry : BundleUtil.resolveBundleDependencies(bundle).getTransitiveClasspathEntries(true)) {
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
