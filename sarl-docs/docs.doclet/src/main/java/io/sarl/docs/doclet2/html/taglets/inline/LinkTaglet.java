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
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class LinkTaglet extends AbstractSarlTaglet {

	/** Name of the taglet.
	 */
	public static final String TAGLET_NAME = "link";

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
		final int idx = sig.indexOf("#");
		if (idx >= 0 && idx < (sig.length() - 1)) {
			return sig.substring(idx + 1);
		}
		return sig;
	}
	
	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element,
			DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor contentExtractor) {
		final LinkTree node = (LinkTree) Iterables.find(tags, it -> it instanceof LinkTree);

		final org.jsoup.nodes.Element labelElement = contentExtractor.extractSimpleText(node.getLabel());

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

		final boolean isplain = node.getKind() == com.sun.source.doctree.DocTree.Kind.LINK_PLAIN;

		final List<Node> referenceNode = contentExtractor.extractReference(node.getReference(), labelNode, isplain);
		if (referenceNode == null) {
			appendChildren(parent, labelElement.childNodes());
			contentExtractor.getContext().getReporter().print(Kind.ERROR, Messages.LinkTaglet_0);
			return false;
		}

		appendChildren(parent, referenceNode);
		return true;
	}

}
