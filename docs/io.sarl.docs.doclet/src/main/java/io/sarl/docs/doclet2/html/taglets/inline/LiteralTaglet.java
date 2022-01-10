/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2022 the original authors or authors.
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
import com.sun.source.doctree.TextTree;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code @literal} tag that forces the text to not be interpreted as HTML code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class LiteralTaglet extends AbstractSarlTaglet {

	/** Name of the taglet.
	 */
	public static final String TAGLET_NAME = "literal";

	/** Constructor.
	 */
	public LiteralTaglet() {
		super(TAGLET_NAME.toLowerCase(), true);
	}

	/** Replies the CSS style for the see text.
	 *
	 * @param original the original CSS style.
	 * @return the CSS style.
	 */
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_LITERAL_COMMENT;
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor referenceExtractor) {
		final StringBuilder buffer = new StringBuilder();
		for (final DocTree tree : tags) {
			if (tree instanceof TextTree) {
				buffer.append(((TextTree) tree).getBody());
			} else {
				buffer.append(tree.toString());
			}
		}
		if (buffer.length() > 0) {
			if (style != null) {
				final org.jsoup.nodes.Element literalTag = getHtmlFactory().createSpanTag(parent, style);
				literalTag.appendText(buffer.toString());
			} else {
				parent.appendText(buffer.toString());
			}
			return true;
		}
		return false;
	}

}
