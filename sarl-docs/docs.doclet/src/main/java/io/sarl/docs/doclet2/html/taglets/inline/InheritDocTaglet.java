/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import com.google.common.collect.Iterables;
import com.sun.source.doctree.BlockTagTree;
import com.sun.source.doctree.DocCommentTree;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.AuthorTree;
import com.sun.source.doctree.DeprecatedTree;
import com.sun.source.doctree.ParamTree;
import com.sun.source.doctree.ProvidesTree;
import com.sun.source.doctree.ReturnTree;
import com.sun.source.doctree.SeeTree;
import com.sun.source.doctree.SinceTree;
import com.sun.source.doctree.ThrowsTree;
import com.sun.source.doctree.UsesTree;
import com.sun.source.doctree.VersionTree;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code inheritDoc} tag.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class InheritDocTaglet extends AbstractSarlTaglet {

	/** Name of the taglet.
	 */
	public static final String TAGLET_NAME = "inheritDoc"; //$NON-NLS-1$

	/** Constructor.
	 */
	public InheritDocTaglet() {
		super(TAGLET_NAME.toLowerCase(), true);
	}

	/** Determine the element that is really documented. It may be a parameter instead of a method for example.
	 *
	 * @param tags the documentation tags.
	 * @return the fine-grained documented element, or the {@code element} itself.
	 */
	
	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor referenceExtractor) {
		if (sourceDocumentation instanceof BlockTagTree) {
			return appendFromInheritedBlockTagDocumentation(parent, element, sourceDocumentation, referenceExtractor);
		}
		return appendFromInheritedMainDocumentation(parent, element, sourceDocumentation, referenceExtractor);
	}

	private static boolean isSameBlockTag(DocTree hidder, DocTree hidden) {
		if (hidder instanceof ParamTree && hidden instanceof ParamTree) {
			final ParamTree phidder = (ParamTree) hidder;
			final ParamTree phidden = (ParamTree) hidden;
			return phidder.isTypeParameter() == phidden.isTypeParameter()
					&& phidder.getName().toString().equals(phidden.getName().toString());
		}
		if (hidder instanceof ThrowsTree && hidden instanceof ThrowsTree) {
			final ThrowsTree phidder = (ThrowsTree) hidder;
			final ThrowsTree phidden = (ThrowsTree) hidden;
			return phidder.getExceptionName().getSignature().equals(phidden.getExceptionName().getSignature());
		}
		if (hidder instanceof ProvidesTree && hidden instanceof ProvidesTree) {
			final ProvidesTree phidder = (ProvidesTree) hidder;
			final ProvidesTree phidden = (ProvidesTree) hidden;
			return phidder.getServiceType().getSignature().equals(phidden.getServiceType().getSignature());
		}
		if (hidder instanceof UsesTree && hidden instanceof UsesTree) {
			final UsesTree phidder = (UsesTree) hidder;
			final UsesTree phidden = (UsesTree) hidden;
			return phidder.getServiceType().getSignature().equals(phidden.getServiceType().getSignature());
		}
		if ((hidder instanceof AuthorTree && hidden instanceof AuthorTree)
			|| (hidder instanceof DeprecatedTree && hidden instanceof DeprecatedTree)
			|| (hidder instanceof ReturnTree && hidden instanceof ReturnTree)
			|| (hidder instanceof SeeTree && hidden instanceof SeeTree)
			|| (hidder instanceof SinceTree && hidden instanceof SinceTree)
			|| (hidder instanceof VersionTree && hidden instanceof VersionTree)) {
			return true;
		}
		return false;
	}
	
	/** Append the documentation text from an inherited block tag documentation.
	 *
	 * @param parent the receiver of the HTML code.
	 * @param element the main documented element (a type of a member).
	 * @param sourceDocumentation the source documentation of the {@code element}.
	 * @param referenceExtractor is the too for extracting information from the generation context.
	 * @return {@code true} if the some element was added to the output.
	 * @see #appendFromInheritedMainDocumentation(org.jsoup.nodes.Element, Element, DocTree, HtmlFactoryContentExtractor)
	 */
	protected boolean appendFromInheritedBlockTagDocumentation(org.jsoup.nodes.Element parent, Element element, DocTree sourceDocumentation, HtmlFactoryContentExtractor referenceExtractor) {
		final SarlDocletEnvironment env = referenceExtractor.getContext().getEnvironment();
		final DocCommentTree fullComment = referenceExtractor.getContext().getDocUtils().getInheritedFullDocumentation(element, env);
		if (fullComment != null) {
			final List<? extends DocTree> blocks = fullComment.getBlockTags();
			final DocTree inheritedDoc = Iterables.find(blocks, it -> {
				return isSameBlockTag(sourceDocumentation, it);
			});
			if (inheritedDoc != null) {
				return appendCommentText(parent, Collections.singletonList(inheritedDoc), element, " ", false, null, referenceExtractor.getContext()); //$NON-NLS-1$
			}
		}
		return false;
	}

	/** Append the documentation text from an inherited main documentation (not block tag).
	 *
	 * @param parent the receiver of the HTML code.
	 * @param element the main documented element (a type of a member).
	 * @param sourceDocumentation the source documentation of the {@code element}.
	 * @param referenceExtractor is the too for extracting information from the generation context.
	 * @return {@code true} if the some element was added to the output.
	 * @see #appendFromInheritedBlockTagDocumentation(org.jsoup.nodes.Element, Element, DocTree, HtmlFactoryContentExtractor)
	 */
	protected boolean appendFromInheritedMainDocumentation(org.jsoup.nodes.Element parent, Element element, DocTree sourceDocumentation, HtmlFactoryContentExtractor referenceExtractor) {
		final SarlDocletEnvironment env = referenceExtractor.getContext().getEnvironment();
		final List<? extends DocTree> documentation = referenceExtractor.getContext().getDocUtils().getInheritedDocumentation(element, env);
		if (!documentation.isEmpty()) {
			return appendCommentText(parent, documentation, element, " ", false, null, referenceExtractor.getContext()); //$NON-NLS-1$
		}
		return false;
	}

}
