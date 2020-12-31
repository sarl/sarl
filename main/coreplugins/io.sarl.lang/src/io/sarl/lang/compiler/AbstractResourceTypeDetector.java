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

package io.sarl.lang.compiler;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.eclipse.emf.common.util.URI;

/** Detect the type of a folder.
 *
 * <p>A resource may be inside a standard source folder or a folder that is dedicated
 * to test code. Detecting in which case a resource is is the purpose of this interface.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public abstract class AbstractResourceTypeDetector implements IResourceTypeDetector {

	/** Replies if the given URI corresponds to a test resource.
	 *
	 * @param uri the uri to test against the test folder name.
	 * @param testFolderName the name of the test folder.
	 * @return {@code true} if the URI is for a test resource.
	 */
	protected static boolean isTestURI(URI uri, List<String> testFolderName) {
		if (uri.isArchive() || uri.isEmpty() || uri.isCurrentDocumentReference()) {
			return false;
		}
		final List<String> segments = new ArrayList<>(uri.segmentsList());
		if (segments.isEmpty()) {
			return false;
		}
		if (uri.isPlatformResource() || uri.isPlatformPlugin()) {
			segments.remove(0);
		}
		// Usually, the first segment is the project's folder
		if (segments.isEmpty()) {
			return false;
		}
		segments.remove(0);
		for (int i = 0; i < testFolderName.size() && i < segments.size(); ++i) {
			final String expected = testFolderName.get(i);
			final String actual = segments.get(i);
			if (!Objects.equals(expected, actual)) {
				return false;
			}
		}
		return true;
	}

}
