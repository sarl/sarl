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

package io.sarl.docs.doclet2.html.taglets.block;

import java.util.List;

import javax.lang.model.element.Element;
import javax.tools.Diagnostic.Kind;

import com.google.common.collect.Iterables;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.ReturnTree;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code @return} tag.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class ReturnTaglet extends AbstractSarlTaglet {

	/** Name of the tag.
	 */
	public static final String TAGLET_NAME = "return";

	public ReturnTaglet() {
		super(TAGLET_NAME.toLowerCase(), false, Location.METHOD);
	}

	/** Replies the CSS style for the return text.
	 *
	 * @param original the original CSS style.
	 * @return the CSS style.
	 */
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_RETURN_COMMENT;
	}

	@Override
	public String getTagBlockLabel() {
		return Messages.ReturnTaglet_0;
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor context) {
		final Iterable<ReturnTree> returns = Iterables.filter(tags, ReturnTree.class);
		final CssStyles rstyle = getTextCssStyle(style);
		boolean changed = false;
		for (final ReturnTree returnEntry : returns) {
			if (changed) {
				parent.appendChild(getHtmlFactory().createNewLineTag());
			}
			;
			final boolean hasDescription = appendCommentTextWithSpace(parent, returnEntry.getDescription(),
					element, rstyle, context.getContext());
			if (!hasDescription) {
				context.getContext().getReporter().print(Kind.ERROR, Messages.ReturnTaglet_1);
			}
			changed = changed || hasDescription;
		}
		return changed;
	}

}
