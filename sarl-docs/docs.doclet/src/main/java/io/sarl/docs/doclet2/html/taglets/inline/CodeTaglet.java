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

package io.sarl.docs.doclet2.html.taglets.inline;

import java.util.List;

import javax.lang.model.element.Element;

import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.TextTree;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** An inline Taglet used to denote literal code fragments.
 * The enclosed text is interpreted as not containing HTML markup or
 * nested doc tags, and is rendered in a font suitable for code.
 *
 * <p>
 * The tag {@code {@code ...}} is equivalent to {@code {@code {@literal ...}}}.
 * For example, the text:
 * <blockquote>The type {@code {@code List<P>}}</blockquote>
 * displays as:
 * <blockquote>The type {@code List<P>}</blockquote>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class CodeTaglet extends AbstractSarlTaglet {

	/** Name of the taglet.
	 */
	public static final String TAGLET_NAME = "code"; //$NON-NLS-1$
	
	/** Constructor.
	 */
	public CodeTaglet() {
		super(TAGLET_NAME.toLowerCase(), true);
	}

	/** Replies the CSS style for the code text.
	 *
	 * @param original the original CSS style.
	 * @return the CSS style.
	 */
	@SuppressWarnings("static-method")
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_CODE_COMMENT;
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
		final org.jsoup.nodes.Element codeTag = getHtmlFactory().createCodeTag(parent, buffer.toString());
		if (style != null) {
			codeTag.addClass(style.getCssClassname());
		}
		return true;
	}

}
