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

package io.sarl.eclipse.buildpath;

import java.text.MessageFormat;
import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathEntry;
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

	/** Names of the root reference libraries that are required to compile the SARL
	 * code and the generated Java code.
	 */
	public static final String[] SARL_ROOT_BUNDLE_NAMES = {
		"io.sarl.lang.core", //$NON-NLS-1$
		"io.sarl.core", //$NON-NLS-1$
		"io.sarl.util", //$NON-NLS-1$
		"io.sarl.javafx", //$NON-NLS-1$
		// For active annotations
		"org.eclipse.xtend.lib", //$NON-NLS-1$
	};

	/** Constructor.
	 * @param containerPath the path of the container, e.g. the project.
	 */
	public SARLClasspathContainer(IPath containerPath) {
		super(containerPath);
	}

	@Override
	public int getKind() {
		// Must be K_SYSTEM in order to let the run-configuration launcher to replace the SARL
		// libraries by the SRE libraries.
		return K_SYSTEM;
	}

	@Override
	protected void updateBundleList(Set<String> entries) {
		for (final String rootBundleName : SARL_ROOT_BUNDLE_NAMES) {
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
		for (final String rootBundleName : SARL_ROOT_BUNDLE_NAMES) {
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
