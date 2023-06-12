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

package io.sarl.docs.doclet2.html.framework;

import io.sarl.docs.doclet2.html.taglets.block.AuthorTaglet;
import io.sarl.docs.doclet2.html.taglets.block.DeprecatedTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ExceptionTaglet;
import io.sarl.docs.doclet2.html.taglets.block.GeneratedTaglet;
import io.sarl.docs.doclet2.html.taglets.block.MavenArtifactIdTaglet;
import io.sarl.docs.doclet2.html.taglets.block.MavenGroupIdTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ParamTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ProvidesTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ReturnTaglet;
import io.sarl.docs.doclet2.html.taglets.block.SeeTaglet;
import io.sarl.docs.doclet2.html.taglets.block.SinceTaglet;
import io.sarl.docs.doclet2.html.taglets.block.ThrowsTaglet;
import io.sarl.docs.doclet2.html.taglets.block.UsesTaglet;
import io.sarl.docs.doclet2.html.taglets.block.VersionTaglet;

/** Tool for sorting the block tags.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class BlockTagSorterImpl implements BlockTagSorter {

	private static final String[] TAGS_ORDER = new String[] {
			ParamTaglet.TAGLET_NAME,
			ReturnTaglet.TAGLET_NAME,
			ThrowsTaglet.TAGLET_NAME,
			ExceptionTaglet.TAGLET_NAME,
			DeprecatedTaglet.TAGLET_NAME,
			ProvidesTaglet.TAGLET_NAME,
			UsesTaglet.TAGLET_NAME,
			MavenGroupIdTaglet.TAGLET_NAME,
			MavenArtifactIdTaglet.TAGLET_NAME,
			VersionTaglet.TAGLET_NAME,
			AuthorTaglet.TAGLET_NAME,
			SinceTaglet.TAGLET_NAME,
			SeeTaglet.TAGLET_NAME,
			GeneratedTaglet.TAGLET_NAME,
	};
	
	private static int indexOf(String name) {
		for (int i = 0; i < TAGS_ORDER.length; ++i) {
			if (TAGS_ORDER[i].equals(name)) {
				return i;
			}
		}
		return -1;
	}
	@Override
	public int compare(String o1, String o2) {
		final int idx1 = indexOf(o1);
		final int idx2 = indexOf(o2);
		return idx1 - idx2;
	}

}
