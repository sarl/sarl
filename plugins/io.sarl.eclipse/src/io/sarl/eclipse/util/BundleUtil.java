/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.eclipse.util;

import java.io.IOException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.osgi.framework.Bundle;

/** Utilities around bundles. It should be replaced
 * by the OSGi, Eclipse and Xtext API.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class BundleUtil {

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

	private static final String SOURCE_SUFIX = ".source"; //$NON-NLS-1$

	private static final String JAVADOC_SUFIX = ".javadoc"; //$NON-NLS-1$

	private BundleUtil() {
		//
	}

	/** Replies the source location for the given bundle.
	 *
	 * <p>We can't use P2Utils and we can't use SimpleConfiguratorManipulator because of
	 * API breakage between 3.5 and 4.2.
	 * So we do a bit EDV (Computer data processing) ;-)
	 *
	 * <p>FIXME: Use P2Utils or SimpleConfiguratorManipulator.
	 *
	 * @param bundle - the bundle for which the source location must be computed.
	 * @param bundleLocation - the location of the bundle, as replied by {@link #getBundlePath(Bundle)}.
	 * @return the path to the source folder of the bundle, or <code>null</code> if undefined.
	 * @see #getBundlePath(Bundle)
	 */
	public static IPath getSourceBundlePath(Bundle bundle, IPath bundleLocation) {
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

	/** Replies the path of the binary files of the given bundle.
	 *
	 * @param bundle - the bundle for which the path must be retreived.
	 * @return the path to the binaries of the bundle.
	 * @see #getSourceBundlePath(Bundle, IPath)
	 */
	public static IPath getBundlePath(Bundle bundle) {
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

	/** Replies the javadoc location for the given bundle.
	 *
	 * <p>We can't use P2Utils and we can't use SimpleConfiguratorManipulator because of
	 * API breakage between 3.5 and 4.2.
	 * So we do a bit EDV (Computer data processing) ;-)
	 *
	 * <p>FIXME: Use P2Utils or SimpleConfiguratorManipulator.
	 *
	 * @param bundle - the bundle for which the javadoc location must be computed.
	 * @param bundleLocation - the location of the bundle, as replied by {@link #getBundlePath(Bundle)}.
	 * @return the path to the javadoc folder of the bundle, or <code>null</code> if undefined.
	 * @see #getBundlePath(Bundle)
	 */
	public static IPath getJavadocBundlePath(Bundle bundle, IPath bundleLocation) {
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
						symbolicName.concat(JAVADOC_SUFIX));
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

}
