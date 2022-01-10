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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.lang.model.element.Element;
import javax.tools.Diagnostic.Kind;

import com.google.common.base.Strings;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.ParamTree;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContext;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code @param} tag.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class ParamTaglet extends AbstractSarlTaglet {

	/** Name of the tag.
	 */
	public static final String TAGLET_NAME = "param";

	/** Constructor.
	 */
	public ParamTaglet() {
		super(TAGLET_NAME.toLowerCase(), false, Location.TYPE, Location.CONSTRUCTOR, Location.METHOD);
	}

	/** Replies the CSS style for the param text.
	 *
	 * @param original the original CSS style.
	 * @return the CSS style.
	 */
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_PARAM_COMMENT;
	}

	@Override
	public String getTagBlockLabel() {
		return Messages.ParamTaglet_4;
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor referenceExtractor) {
		final List<ParamTree> genericParameters = new ArrayList<>();
		final List<ParamTree> regularParameters = new ArrayList<>();
		for (final DocTree doc : tags) {
			if (doc instanceof ParamTree) {
				final ParamTree paramTree = (ParamTree) doc;
				final String name = paramTree.getName().getName().toString();
				if (Strings.isNullOrEmpty(name)) {
					referenceExtractor.getContext().getReporter().print(Kind.ERROR, MessageFormat.format(Messages.ParamTaglet_0, element.getSimpleName().toString()));
				} else  if (paramTree.isTypeParameter()) {
					genericParameters.add(paramTree);
				} else {
					regularParameters.add(paramTree);
				}
			}
		}
		final CssStyles rstyle = getTextCssStyle(style);
		boolean changed = appendParameters(false, genericParameters, Messages.ParamTaglet_1, parent, element, rstyle, referenceExtractor.getContext());
		changed = appendParameters(changed, regularParameters, Messages.ParamTaglet_2, parent, element, rstyle, referenceExtractor.getContext());
		return changed;
	}

	private boolean appendParameters(boolean previousChange, List<ParamTree> parameters, String headModel, org.jsoup.nodes.Element parent, Element element,
			CssStyles style, HtmlFactoryContext context) {
		boolean changed = previousChange;
		for (final ParamTree parameter : parameters) {
			if (changed) {
				parent.appendChild(getHtmlFactory().createNewLineTag());
			}
			final String paramName = parameter.getName().getName().toString();
			parent.appendText(MessageFormat.format(headModel, paramName));
			getHtmlFactory().createSecableSpace(parent);
			final boolean hasDescription = appendCommentTextWithSpace(parent, Collections.singletonList(parameter),
					element, getTextCssStyle(style), context);
			if (!hasDescription) {
				context.getReporter().print(Kind.ERROR, MessageFormat.format(Messages.ParamTaglet_3, paramName, element.getSimpleName().toString()));
			}
			changed = true;
		}
		return changed;
	}

}
