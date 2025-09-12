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
import java.util.Collections;
import java.util.List;

import javax.lang.model.element.Element;
import javax.tools.Diagnostic.Kind;

import com.google.common.collect.Iterables;
import com.sun.source.doctree.DocTree;
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
	public static final String TAGLET_NAME = "throws"; //$NON-NLS-1$

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
	@SuppressWarnings("static-method")
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_THROWS_COMMENT;
	}

	@Override
	public String getTagBlockLabel() {
		return Messages.ThrowsTaglet_0;
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor context) {
		final var throwsList0 = Iterables.filter(tags, ThrowsTree.class);
		final var throwsList = Iterables.filter(throwsList0, it -> {
			final var reference = it.getExceptionName();
			if (reference == null) {
				context.getContext().getReporter().print(Kind.ERROR, Messages.ThrowsTaglet_1);
				return false;
			}
			return true;
		});
		final var rstyle = getTextCssStyle(style);
		var changed = false;
		if (throwsList.iterator().hasNext()) {
			final var ulElement = getHtmlFactory().createUlTag(parent, rstyle);
			for (final var throwsEntry : throwsList) {
				final var reference = throwsEntry.getExceptionName();
				final var tmpElement0 = getHtmlFactory().createSpanTag(null, null);
				appendCommentTextWithSpace(tmpElement0, Collections.singletonList(reference), element, rstyle, context.getContext());
				final var tmpElement1 = getHtmlFactory().createSpanTag(null, null);
				final var hasDescription = appendCommentTextWithSpace(tmpElement1, throwsEntry.getDescription(),
						element, rstyle, context.getContext());
				if (!hasDescription) {
					context.getContext().getReporter().print(Kind.ERROR, MessageFormat.format(Messages.ThrowsTaglet_2, reference.toString()));
				}
				final var text = MessageFormat.format(Messages.ThrowsTaglet_3,
						tmpElement0.html(),
						tmpElement1.html());
				final var liElement = getHtmlFactory().createLiTag(ulElement, rstyle);
				liElement.append(text);
				changed = true;
			}
		}
		return changed;
	}

}
