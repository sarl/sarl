/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

import java.io.File;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

/**
 * View filter for hidden files.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.13.0 20230919-093100
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.6
 */
public class HiddenFileFilter extends ViewerFilter {

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (element instanceof IResource) {
			final IResource resource = (IResource) element;
			if (resource.isHidden() || resource.isDerived() || resource.isPhantom()) {
				return false;
			}
			final IPath path2 = resource.getRawLocation();
			if (path2 != null) {
				final File rawFile = path2.toFile();
				return rawFile != null && !rawFile.isHidden();
			}
		}
		return true;
	}

}
