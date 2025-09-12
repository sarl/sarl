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

package io.sarl.apputils.eclipseextensions.buildpath;

import java.util.Comparator;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;

/** Comparator of folders according to the standard SARL project structure.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class SARLFolderComparator implements Comparator<IFolder> {

	private static SARLFolderComparator singleton;

	private final Comparator<? super IPath> pathComparator;

	/** Create the comparator using the {@link SARLPathComparator} for comparing paths.
	 */
	public SARLFolderComparator() {
		this(null);
	}

	/** Create the comparator using the given comparator for comparing paths.
	 *
	 * @param pathComparator the comparator of paths to be used. If it is {@code null}, a {@link SARLPathComparator} is used.
	 */
	public SARLFolderComparator(Comparator<? super IPath> pathComparator) {
		if (pathComparator == null) {
			this.pathComparator = SARLPathComparator.getSingleton();
		} else {
			this.pathComparator = pathComparator;
		}
	}
	
	/** Replies the singleton instance for this comparator.
	 *
	 * @return the comparator, never {@code null}.
	 */
	public static SARLFolderComparator getSingleton() {
		if (singleton == null) {
			singleton = new SARLFolderComparator();
		}
		return singleton;
	}

	@Override
	public int compare(IFolder folder1, IFolder folder2) {
		if (folder1 == folder2) {
			return 0;
		}
		if (folder1 == null) {
			return Integer.MIN_VALUE;
		}
		if (folder2 == null) {
			return Integer.MAX_VALUE;
		}
		final var path1 = folder1.getFullPath();
		final var path2 = folder2.getFullPath();
		return this.pathComparator.compare(path1, path2);
	}

}
