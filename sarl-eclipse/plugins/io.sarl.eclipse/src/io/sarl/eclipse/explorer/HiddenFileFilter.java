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

package io.sarl.eclipse.explorer;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

import io.sarl.lang.SARLConfig;

/**
 * View filter for hidden files.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class HiddenFileFilter extends ViewerFilter {

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (element instanceof IResource resource) {
			final var path2 = resource.getRawLocation();
			if ((resource.isHidden() || resource.isDerived() || resource.isPhantom()) && !isGenerationFolder(path2)) {
				return false;
			}
			if (path2 != null) {
				final var rawFile = path2.toFile();
				return rawFile != null && !rawFile.isHidden();
			}
		}
		return true;
	}

	private static boolean isGenerationFolder(IPath path) {
		if (path != null) {
			final var max = path.segmentCount();
			for (var i = 0; i < max; ++i) {
				final var segment = path.segment(i);
				if (SARLConfig.GENERATED_SOURCE_ROOT_FOLDER_SIMPLENAME.equals(segment)) {
					return true;
				}
			}
		}
		return false;
	}

}
