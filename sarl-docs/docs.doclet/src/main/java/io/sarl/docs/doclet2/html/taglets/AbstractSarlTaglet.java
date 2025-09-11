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

package io.sarl.docs.doclet2.html.taglets;

import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import javax.lang.model.element.Element;

import org.jsoup.nodes.Node;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import com.sun.source.doctree.DocTree;

import io.sarl.docs.doclet2.framework.ElementUtils;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.HtmlFactory;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContext;
import io.sarl.docs.doclet2.html.framework.HtmlTags;

/** Abstract implementation of a taglet.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public abstract class AbstractSarlTaglet implements SarlTaglet {

	private HtmlFactory htmlFactory;

	private ElementUtils elementUtils;
	
	private DocletOptions docletOptions;

	private final String name;

	private final boolean inline;

	private final Set<Location> locations;

	/** Constructor.
	 *
	 * @param name the name of the taglet.
	 * @param inline indicates if the tag is inline ({@code true}) or a block tag ({@code false}).
	 * @param locations the accepted locations of the tag.
	 */
	public AbstractSarlTaglet(String name, boolean inline, Location... locations) {
		this.name = name;
		this.inline = inline;
		if (inline) {
			this.locations = Collections.emptySet();
		} else {
			this.locations = EnumSet.copyOf(Arrays.asList(locations));
		}
	}

	/** Change the HTML factory.
	 *
	 * @param factory the HTML factory.
	 */
	@Inject
	public void setHtmlFactory(HtmlFactory factory) {
		this.htmlFactory = factory;
	}

	/** Replies the HTML factory.
	 *
	 * @return the HTML factory.
	 */
	public HtmlFactory getHtmlFactory() {
		return this.htmlFactory;
	}

	/** Change the doclet options.
	 *
	 * @param options the doclet options.
	 */
	@Inject
	public void setDocletOptions(DocletOptions options) {
		this.docletOptions = options;
	}

	/** Replies the doclet options.
	 *
	 * @return the doclet options.
	 */
	public DocletOptions getDocletOptions() {
		return this.docletOptions;
	}

	/** Change the SARL utilities for elements.
	 *
	 * @param utils the utilities.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the SARL utilities for elements.
	 *
	 * @return the element utils.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}
	
	@Override
	public String toString() {
		final var buf = new StringBuilder();
		if (isInlineTag()) {
			buf.append("@"); //$NON-NLS-1$
		}
		buf.append(getName());
		buf.append(":"); //$NON-NLS-1$
		buf.append(getAllowedLocations());
		return buf.toString(); 
	}
	
	@Override
	public String toString(List<? extends DocTree> tags, Element element) {
		final var elt = new org.jsoup.nodes.Element(HtmlTags.pseudoTag("empty")); //$NON-NLS-1$
		if (appendNode(elt, tags, element, null, null, null)) {
			return elt.html();
		}
		return ""; //$NON-NLS-1$
	}

	@Override
	public Set<Location> getAllowedLocations() {
		return Collections.unmodifiableSet(this.locations);
	}

	@Override
	public boolean isInlineTag() {
		return this.inline;
	}

	@Override
	public String getName() {
		return this.name;
	}

	/** Append the given comment separated by white space.
	 *
	 * @param parent the receiver.
	 * @param tags the comment texts.
	 * @param documentedElement the documented element.
	 * @param style the CSS style to associated to the HTML tag.
	 * @param context the generation context for the HTML elements.
	 * @return {@code true} if something has been output.
	 */
	protected boolean appendCommentTextWithSpace(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element documentedElement, CssStyles style, HtmlFactoryContext context) {
		return appendCommentText(parent, tags, documentedElement, null, true, style, context);
	}

	/** Append the given comment separated by coma.
	 *
	 * @param parent the receiver.
	 * @param tags the comment texts.
	 * @param documentedElement the documented element.
	 * @param style the CSS style to associated to the HTML tag.
	 * @param context the generation context for the HTML elements.
	 * @return {@code true} if something has been output.
	 */
	protected boolean appendCommentTextWithComa(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element documentedElement, CssStyles style, HtmlFactoryContext context) {
		return appendCommentText(parent, tags, documentedElement, ",", true, style, context); //$NON-NLS-1$
	}

	/** Append the given comment separated by a separator.
	 *
	 * @param parent the receiver.
	 * @param tags the comment texts.
	 * @param documentedElement the documented element.
	 * @param separator the separator.
	 * @param createSpan indicates if a &lt;span/&gt; element must be created for receiving the elements from {@code tags}.
	 * @param style the CSS style to associated to the HTML tag.
	 * @param context the generation context for the HTML elements.
	 * @return {@code true} if something has been output.
	 */
	protected boolean appendCommentText(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element documentedElement, String separator, boolean createSpan, CssStyles style, HtmlFactoryContext context) {
      if (!tags.isEmpty()) {
      	final var contentElements = tags.iterator();
      	// Pass null as parent in order to avoid to add it into the parent node if there is
      	// no text to output
      	final org.jsoup.nodes.Element outputNode;
      	if (createSpan) {
      		outputNode = getHtmlFactory().createSpanTag(null, style);
      	} else {
      		outputNode = parent;
      	}
		final var memory = getHtmlFactory().createCommentTextMemory(outputNode, documentedElement, context);
		var changed = getHtmlFactory().createCommentText(memory, contentElements.next(), style);
      	while (contentElements.hasNext()) {
          	if (!Strings.isNullOrEmpty(separator)) {
          		outputNode.appendText(separator);
          	}
          	getHtmlFactory().createSecableSpace(outputNode);
          	changed = getHtmlFactory().createCommentText(memory, contentElements.next(), style)
          			|| changed;
      	}
      	if (changed && createSpan) {
      		parent.appendChild(outputNode);
      	}
        return changed;
      }
      return false;
	}

	/** Append the given child to the given parent. This function ignores the pseudo tags
	 * and adds the children of a pseudo tag directly (not the pseudo tag itself).
	 *
	 * @param parent the receiver.
	 * @param children the nodes to add into the parent node.
	 * @see #appendChildren(org.jsoup.nodes.Element, Node)
	 */
	protected void appendChildren(org.jsoup.nodes.Element parent, Iterable<? extends Node> children) {
		assert parent != null : "parent argument must not be null"; //$NON-NLS-1$
		assert children != null : "children argument must not be null"; //$NON-NLS-1$
		for (final var child : children) {
			if (child != null) {
				appendChildren(parent, child);
			}
		}
	}

	/** Append the given child to the given parent. This function ignores the pseudo tags
	 * and adds the children of a pseudo tag directly (not the pseudo tag itself).
	 *
	 * @param parent the receiver.
	 * @param child the node to add into the parent node.
	 * @see #appendChildren(org.jsoup.nodes.Element, Iterable)
	 */
	@SuppressWarnings("static-method")
	protected void appendChildren(org.jsoup.nodes.Element parent, Node child) {
		assert parent != null : "parent argument must not be null"; //$NON-NLS-1$
		assert child != null : "child argument must not be null"; //$NON-NLS-1$
		if (HtmlTags.isPseudoTag(child.nodeName()) && child instanceof Element) {
			for (final var chld : child.childNodes()) {
				parent.appendChild(chld.clone());
			}
		} else {
			parent.appendChild(child.clone());
		}
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor referenceExtractor) {
    	throw new RuntimeException("Unexpected use of the taglet " + getClass().getSimpleName()); //$NON-NLS-1$
	}

}
