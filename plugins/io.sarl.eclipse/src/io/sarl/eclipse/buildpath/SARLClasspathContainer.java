/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.naming.NameNotFoundException;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IAccessRule;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.osgi.framework.Bundle;

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
		"io.sarl.lang.core", //$NON-NLS-1$
		"io.sarl.util", //$NON-NLS-1$
		"io.sarl.core", //$NON-NLS-1$
	};

	/** OS-independent paths of the bin folders.
	 */
	public static final String[] BIN_FOLDERS = {
		"target/classes", //$NON-NLS-1$
		"bin", //$NON-NLS-1$
	};

	/** OS-independent paths of the source folders.
	 */
	public static final String[] SRC_FOLDERS = {
		"src/main/java", //$NON-NLS-1$
		"src/main/sarl", //$NON-NLS-1$
		"src", //$NON-NLS-1$
	};

	/** URL of the Javadoc for SARL API.
	 */
	public static final String JAVADOC_URL = "http://www.sarl.io/docs/api/"; //$NON-NLS-1$

	private static final String SOURCE_SUFIX = ".source"; //$NON-NLS-1$

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

			IPath bundlePath = getBundlePath(bundle);
			IPath sourceBundlePath = getSourceBundlePath(bundle, bundlePath);

			IClasspathAttribute[] extraAttributes = null;
			if (referenceLibrary.startsWith("io.sarl")) { //$NON-NLS-1$
				extraAttributes = new IClasspathAttribute[] {
						JavaCore.newClasspathAttribute(
								IClasspathAttribute.JAVADOC_LOCATION_ATTRIBUTE_NAME,
								JAVADOC_URL),
					};
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

	/**
	 * We can't use P2Utils and we can't use SimpleConfiguratorManipulator because of
	 * API breakage between 3.5 and 4.2.
	 * So we do a bit EDV (Computer data processing) ;-)
	 *
	 * FIXME: Use P2Utils or SimpleConfiguratorManipulator
	 */
	private static IPath getSourceBundlePath(Bundle bundle, IPath bundleLocation) {
		IPath sourcesPath = null;
		// Not an essential functionality, make it robust
		try {
			IPath srcFolderPath = getSrcFolderPath(bundle);
			if (srcFolderPath == null) {
				//common case, jar file.
				IPath bundlesParentFolder = bundleLocation.removeLastSegments(1);
				String binaryJarName = bundleLocation.lastSegment();
				String symbolicName = bundle.getSymbolicName();
				String sourceJarName = binaryJarName.replace(symbolicName,
						symbolicName.concat(SOURCE_SUFIX));
				IPath potentialSourceJar = bundlesParentFolder.append(sourceJarName);
				if (potentialSourceJar.toFile().exists()) {
					sourcesPath = potentialSourceJar;
				}
			} else {
				sourcesPath = srcFolderPath.removeLastSegments(1);
			}
		} catch (Throwable t) {
			throw new RuntimeException(t);
		}
		return sourcesPath;
	}

	private static IPath getBinFolderPath(Bundle bundle) {
		for (String binFolder : BIN_FOLDERS) {
			URL binFolderURL = FileLocator.find(bundle, Path.fromPortableString(binFolder), null);
			if (binFolderURL != null) {
				try {
					URL binFolderFileURL = FileLocator.toFileURL(binFolderURL);
					return new Path(binFolderFileURL.getPath()).makeAbsolute();
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			}
		}
		return null;
	}

	private static IPath getSrcFolderPath(Bundle bundle) {
		for (String srcFolder : SRC_FOLDERS) {
			URL srcFolderURL = FileLocator.find(bundle, Path.fromPortableString(srcFolder), null);
			if (srcFolderURL != null) {
				try {
					URL srcFolderFileURL = FileLocator.toFileURL(srcFolderURL);
					return new Path(srcFolderFileURL.getPath()).makeAbsolute();
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			}
		}
		return null;
	}

	private static IPath getBundlePath(Bundle bundle) {
		IPath path = getBinFolderPath(bundle);
		if (path == null) {
			// common jar file case, no bin folder
			try {
				path = new Path(FileLocator.getBundleFile(bundle).getAbsolutePath());
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		return path;
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
