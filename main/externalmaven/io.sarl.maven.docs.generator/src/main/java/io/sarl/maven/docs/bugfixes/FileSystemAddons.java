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

package io.sarl.maven.docs.bugfixes;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.URISchemeType;
import org.eclipse.xtext.xbase.lib.Pure;

/** Functions that are missed in {@link FileSystem}, but planned to be added.
 *
 * <p>Each function inside this class is marked with the AFC issue that is associated to.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class FileSystemAddons {

	private FileSystemAddons() {
		//
	}

	/** Replies an URL for the given file and translate it into a
	 * resource URL if the given file is inside the classpath.
	 *
	 * @param file is the filename to translate.
	 * @param allowRelativePath indicate if a relative path is allowed in the returned URL.
	 * @return the URL which is corresponding to file, or {@code null} if
	 *     the url cannot be computed.
	 * @see "https://github.com/gallandarakhneorg/afc/issues/173"
	 */
	@Pure
	public static URL convertFileToURL(File file, boolean allowRelativePath) {
		if (file == null) {
			return null;
		}
		try {
			File thefile = file;
			if (FileSystem.isWindowsNativeFilename(file.toString())) {
				thefile = FileSystem.normalizeWindowsNativeFilename(file.toString());
				if (thefile == null) {
					thefile = file;
				}
			}
			final URL url;
			if (thefile.isAbsolute() || !allowRelativePath) {
				url = thefile.toURI().toURL();
			} else {
				final String[] elements = FileSystem.split(thefile);
				final StringBuilder path = new StringBuilder();
				for (final String element : elements) {
					if (path.length() > 0) {
						path.append(FileSystem.URL_PATH_SEPARATOR);
					}
					path.append(element);
				}
				url = new URL(URISchemeType.FILE.name().toLowerCase(), null, path.toString());
			}
			return FileSystem.toShortestURL(url);
		} catch (MalformedURLException e) {
			return null;
		}
	}

}
