/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.util.ArrayList;
import java.util.List;

import javax.naming.NameNotFoundException;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.osgi.framework.Bundle;

import io.sarl.eclipse.util.BundleUtil;

/** Classpath container dedicated to the SARL environment.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLClasspathContainer implements IClasspathContainer {

	/** Names of the reference libraries that are required to compile the SARL
	 * code and the generated Java code.
	 */
	public static final String[] SARL_REFERENCE_LIBRARIES = {
		"org.eclipse.xtext.xbase.lib", //$NON-NLS-1$
		"com.google.guava", //$NON-NLS-1$
		"javax.inject", //$NON-NLS-1$
		"io.sarl.lang.core", //$NON-NLS-1$
		"io.sarl.util", //$NON-NLS-1$
		"io.sarl.core", //$NON-NLS-1$
	};

	/** URL of the Javadoc for SARL API.
	 */
	public static final String JAVADOC_URL = "http://www.sarl.io/docs/api/"; //$NON-NLS-1$

	private IClasspathEntry[] entries;

	private final IPath containerPath;

	/**
	 * @param containerPath - the path of the container, e.g. the project.
	 */
	public SARLClasspathContainer(IPath containerPath) {
		this.containerPath = containerPath;
	}

	@Override
	public synchronized IClasspathEntry[] getClasspathEntries() {
		if (this.entries == null) {
			try {
				updateEntries();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
		return this.entries;
	}

	private void updateEntries() throws Exception {
		List<IClasspathEntry> newEntries = new ArrayList<>();
		for (String referenceLibrary : SARL_REFERENCE_LIBRARIES) {
			// Retreive the bundle
			Bundle bundle = Platform.getBundle(referenceLibrary);
			if (bundle == null) {
				throw new NameNotFoundException("No bundle found for: " + referenceLibrary); //$NON-NLS-1$
			}

			IPath bundlePath = BundleUtil.getBundlePath(bundle);
			IPath sourceBundlePath = BundleUtil.getSourceBundlePath(bundle, bundlePath);

			IClasspathAttribute[] extraAttributes = null;
			if (referenceLibrary.startsWith("io.sarl")) { //$NON-NLS-1$
				IPath javadocPath = BundleUtil.getJavadocBundlePath(bundle, bundlePath);
				IClasspathAttribute attr;
				if (javadocPath == null) {
					attr = JavaCore.newClasspathAttribute(
							IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME,
							JAVADOC_URL);
				} else {
					attr = JavaCore.newClasspathAttribute(
							IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME,
							javadocPath.makeAbsolute().toOSString());
				}
				extraAttributes = new IClasspathAttribute[] {attr};
			}

			newEntries.add(JavaCore.newLibraryEntry(
					bundlePath,
					sourceBundlePath,
					null,
					new IAccessRule[] {},
					extraAttributes,
					false));
		}
		this.entries = newEntries.toArray(new IClasspathEntry[newEntries.size()]);
	}

	/** Reset the container.
	 */
	synchronized void reset() {
		this.entries = null;
	}

	@Override
	public String getDescription() {
		return Messages.SARLClasspathContainer_0;
	}

	@Override
	public int getKind() {
		return K_SYSTEM;
	}

	@Override
	public IPath getPath() {
		return this.containerPath;
	}

}
