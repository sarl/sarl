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
 *
 *------- FORKED SOURCE CODE:
 *
 * THIS CODE IS FORKED FROM JDK.JAVADOC INTERNAL PACKAGE AND ADAPTED TO THE SARL PURPOSE.
 * THE FORK WAS NECESSARY BECAUSE IT IS IMPOSSIBLE TO SUBCLASS THE TYPES FOR THE.
 * STANDARD HTML DOCLET THAT IS PROVIDED BY JDK.JAVADOC MODULE.
 *
 * Copyright (c) 2003, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
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

/** Taglet for {@code @optionalparam} tag.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class OptionalParamTaglet extends AbstractSarlTaglet {

	/** Name of the tag.
	 */
	public static final String TAGLET_NAME = "optionalparam"; //$NON-NLS-1$

	/** Constructor.
	 */
	public OptionalParamTaglet() {
		super(TAGLET_NAME.toLowerCase(), false, Location.TYPE, Location.CONSTRUCTOR, Location.METHOD);
	}

	/** Replies the CSS style for the optional param text.
	 *
	 * @param original the original CSS style.
	 * @return the CSS style.
	 */
	@SuppressWarnings("static-method")
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_OPTIONALPARAM_COMMENT;
	}

	@Override
	public String getTagBlockLabel() {
		return Messages.OptionalParamTaglet_0;
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
					referenceExtractor.getContext().getReporter().print(Kind.ERROR, MessageFormat.format(Messages.OptionalParamTaglet_1, element.getSimpleName().toString()));
				} else  if (paramTree.isTypeParameter()) {
					genericParameters.add(paramTree);
				} else {
					regularParameters.add(paramTree);
				}
			}
		}
		final CssStyles rstyle = getTextCssStyle(style);
		boolean changed = appendParameters(false, genericParameters, Messages.OptionalParamTaglet_2, parent, element, rstyle, referenceExtractor.getContext());
		changed = appendParameters(changed, regularParameters, Messages.OptionalParamTaglet_3, parent, element, rstyle, referenceExtractor.getContext());
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
				context.getReporter().print(Kind.ERROR, MessageFormat.format(Messages.OptionalParamTaglet_1, paramName, element.getSimpleName().toString()));
			}
			changed = true;
		}
		return changed;
	}

}