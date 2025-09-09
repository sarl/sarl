/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import java.util.Collections;
import java.util.List;

import javax.lang.model.element.Element;
import javax.tools.Diagnostic.Kind;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.LinkTree;
import com.sun.source.doctree.ReferenceTree;
import org.jsoup.nodes.Node;
import org.jsoup.nodes.TextNode;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code @link} or {@code @linkplain} tags that are used for referencing elements with hyperlinks.
 *
 * @author $Author: sgalland$
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class LinkTaglet extends AbstractSarlTaglet {

	/** Name of the taglet.
	 */
	public static final String TAGLET_NAME = "link"; //$NON-NLS-1$

	/** Constructor.
	 */
	public LinkTaglet() {
		super(TAGLET_NAME.toLowerCase(), true);
	}

	private static String formatLinkLabel(ReferenceTree referenceTree) {
		final String sig;
		if (Strings.isNullOrEmpty(referenceTree.getSignature())) {
			sig = referenceTree.toString();
		} else {
			sig = referenceTree.getSignature();
		}
		final var idx = sig.indexOf("#"); //$NON-NLS-1$
		if (idx >= 0 && idx < (sig.length() - 1)) {
			return sig.substring(idx + 1);
		}
		return sig;
	}
	
	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element,
			DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor contentExtractor) {
		final var node = (LinkTree) Iterables.find(tags, it -> it instanceof LinkTree);

		final var labelElement = contentExtractor.extractSimpleText(node.getLabel());

		if (node.getReference() == null) {
			appendChildren(parent, labelElement.childNodes());
			return true;
		}

		final List<Node> labelNode;
		if (labelElement.childNodeSize() == 0) {
			labelNode = Collections.singletonList(new TextNode(formatLinkLabel(node.getReference())));
		} else {
			labelNode = labelElement.childNodes();
		}

		final var isplain = node.getKind() == com.sun.source.doctree.DocTree.Kind.LINK_PLAIN;

		final var referenceNode = contentExtractor.extractReference(node.getReference(), labelNode, isplain);
		if (referenceNode == null) {
			appendChildren(parent, labelElement.childNodes());
			contentExtractor.getContext().getReporter().print(Kind.ERROR, Messages.LinkTaglet_0);
			return false;
		}

		appendChildren(parent, referenceNode);
		return true;
	}

}
