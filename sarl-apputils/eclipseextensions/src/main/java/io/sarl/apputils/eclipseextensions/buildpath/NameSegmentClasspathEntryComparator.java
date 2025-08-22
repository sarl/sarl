/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import org.eclipse.jdt.core.IClasspathEntry;

/** Comparator of classpath entries based on the lexicographic order of the names' segments.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class NameSegmentClasspathEntryComparator implements Comparator<IClasspathEntry> {

	private static NameSegmentClasspathEntryComparator singleton;
	
	/** Replies the singleton instance for this comparator.
	 *
	 * @return the comparator, never {@code null}.
	 */
	public static NameSegmentClasspathEntryComparator getSingleton() {
		if (singleton == null) {
			singleton = new NameSegmentClasspathEntryComparator();
		}
		return singleton;
	}
	
	@Override
	public int compare(IClasspathEntry entry1, IClasspathEntry entry2) {
		if (entry1 == entry2) {
			return 0;
		}
		if (entry1 == null) {
			return Integer.MAX_VALUE;
		}
		if (entry2 == null) {
			return Integer.MIN_VALUE;
		}
		final var path1 = entry1.getPath();
		final var path2 = entry2.getPath();
		final var segs1 = path1.segmentCount();
		final var segs2 = path2.segmentCount();
		final var segs = segs1 <= segs2 ? segs1 : segs2;
		for (int i = 0; i < segs; ++i) {
			final var seg1 = path1.segment(i);
			final var seg2 = path2.segment(i);
			final var cmp = seg1.compareTo(seg2);
			if (cmp != 0) {
				return cmp;
			}
		}
		return Integer.compare(segs1, segs2);
	}

}
