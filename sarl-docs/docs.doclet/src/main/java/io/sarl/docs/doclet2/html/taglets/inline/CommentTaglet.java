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

package io.sarl.docs.doclet2.html.taglets.inline;

import java.util.List;

import javax.lang.model.element.Element;

import com.sun.source.doctree.DocTree;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code @comment} tag that denote a HTML comment (not shown on the generated documentation page).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class CommentTaglet extends AbstractSarlTaglet {

	/** Constructor.
	 */
	public CommentTaglet() {
		super("comment", true); //$NON-NLS-1$
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor referenceExtractor) {
		if (referenceExtractor.getContext().getCliOptions().isHtmlCommentsEnabled()) {
			final org.jsoup.nodes.Element commentTag0 = getHtmlFactory().createSpanTag(null, null);
			if (appendCommentText(commentTag0, tags, element, " ", false, null, referenceExtractor.getContext())) { //$NON-NLS-1$
				getHtmlFactory().createHtmlComment(parent, commentTag0.html());
				return true;
			}
			return false;
		}
		// Generation of a DIV that is supposed to be hidden
		return appendCommentTextWithSpace(parent, tags, element, CssStyles.TAG_COMMENT, referenceExtractor.getContext());
	}

}
