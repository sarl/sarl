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

package io.sarl.docs.doclet2.html.taglets.block;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.List;

import javax.lang.model.element.Element;
import javax.tools.Diagnostic.Kind;

import com.google.common.collect.Iterables;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.ReferenceTree;
import com.sun.source.doctree.ThrowsTree;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code @throws} tag.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class ThrowsTaglet extends AbstractSarlTaglet {

	/** Name of the tag.
	 */
	public static final String TAGLET_NAME = "throws";

	/** Constructor.
	 */
	public ThrowsTaglet() {
		this(TAGLET_NAME.toLowerCase());
	}

	/** Constructor.
	 *
	 * @param name the tag name.
	 */
	public ThrowsTaglet(String name) {
		super(name, false, Location.CONSTRUCTOR, Location.METHOD);
	}

	/** Replies the CSS style for the see text.
	 *
	 * @param original the original CSS style.
	 * @return the CSS style.
	 */
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_THROWS_COMMENT;
	}

	@Override
	public String getTagBlockLabel() {
		return Messages.ThrowsTaglet_0;
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor context) {
		final Iterable<ThrowsTree> throwsList0 = Iterables.filter(tags, ThrowsTree.class);
		final Iterable<ThrowsTree> throwsList = Iterables.filter(throwsList0, it -> {
			final ReferenceTree reference = it.getExceptionName();
			if (reference == null) {
				context.getContext().getReporter().print(Kind.ERROR, Messages.ThrowsTaglet_1);
				return false;
			}
			return true;
		});
		final CssStyles rstyle = getTextCssStyle(style);
		boolean changed = false;
		if (throwsList.iterator().hasNext()) {
			final org.jsoup.nodes.Element ulElement = getHtmlFactory().createUlTag(parent, rstyle);
			for (final ThrowsTree throwsEntry : throwsList) {
				final ReferenceTree reference = throwsEntry.getExceptionName();
				final org.jsoup.nodes.Element tmpElement0 = getHtmlFactory().createSpanTag(null, null);
				appendCommentTextWithSpace(tmpElement0, Collections.singletonList(reference), element, rstyle, context.getContext());
				final org.jsoup.nodes.Element tmpElement1 = getHtmlFactory().createSpanTag(null, null);
				final boolean hasDescription = appendCommentTextWithSpace(tmpElement1, throwsEntry.getDescription(),
						element, rstyle, context.getContext());
				if (!hasDescription) {
					context.getContext().getReporter().print(Kind.ERROR, MessageFormat.format(Messages.ThrowsTaglet_2, reference.toString()));
				}
				final String text = MessageFormat.format(Messages.ThrowsTaglet_3,
						tmpElement0.html(),
						tmpElement1.html());
				final org.jsoup.nodes.Element liElement = getHtmlFactory().createLiTag(ulElement, rstyle);
				liElement.append(text);
				changed = true;
			}
		}
		return changed;
	}

}
