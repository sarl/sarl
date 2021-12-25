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

package io.sarl.docs.doclet2.html.taglets.block;

import java.util.List;

import javax.lang.model.element.Element;

import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.UnknownBlockTagTree;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code generated} tag.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class GeneratedTaglet extends AbstractSarlTaglet {

	/** Name of the tag.
	 */
	public static final String TAGLET_NAME = "generated";

	public GeneratedTaglet() {
		super(TAGLET_NAME.toLowerCase(), false, Location.values());
	}

	/** Replies the CSS style for the generated info text.
	 *
	 * @param original the original CSS style.
	 * @return the CSS style.
	 */
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_GENERATED_COMMENT;
	}
	
	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor referenceExtractor) {
		final org.jsoup.nodes.Element prefix = getHtmlFactory().createSpanTag(parent, getTextCssStyle(style));
		prefix.appendText(Messages.GeneratedTaglet_0);
		getHtmlFactory().createSecableSpace(parent);
		
		boolean first = true;
		for (final DocTree tr : tags) {
			if (first) {
				first = false;
			} else {
				if (!parent.html().toString().endsWith(".")) {
					parent.appendText(".");
				}
				getHtmlFactory().createSecableSpace(parent);
			}
			if (tr instanceof UnknownBlockTagTree) {
				final UnknownBlockTagTree ubtt = (UnknownBlockTagTree) tr;
		    	appendCommentTextWithSpace(parent, ubtt.getContent(), element, getTextCssStyle(style), referenceExtractor.getContext());
			}
		}
		return true;
	}

}
