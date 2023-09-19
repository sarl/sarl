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

package io.sarl.docs.doclet2.framework;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.inject.Inject;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;

import com.sun.source.doctree.BlockTagTree;
import com.sun.source.doctree.DeprecatedTree;
import com.sun.source.doctree.DocCommentTree;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.DocTree.Kind;
import com.sun.source.doctree.ParamTree;
import com.sun.source.util.SimpleDocTreeVisitor;
import jdk.javadoc.doclet.Taglet.Location;

/** Utilities for documentation elements.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.13.0 20230919-093059
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class DocUtilsImpl implements DocUtils {

	private ElementUtils elementUtils;

	private TypeHierarchy typeHierarchy;

	/** Change the SARL-specific type-hierarchy utilities.
	 *
	 * @param utils the element utilities.
	 */
	@Inject
	public void setTypeHierarchy(TypeHierarchy utils) {
		this.typeHierarchy = utils;
	}

	/** Replies the SARL-specific type-hierarchy utilities.
	 *
	 * @return the utilities.
	 */
	public TypeHierarchy getTypeHierarchy() {
		return this.typeHierarchy;
	}

	/** Change the SARL-specific element utilities.
	 *
	 * @param utils the element utilities.
	 */
	@Inject
	public void setElementUtils(ElementUtils utils) {
		this.elementUtils = utils;
	}

	/** Replies the SARL-specific element utilities.
	 *
	 * @return the utilities.
	 */
	public ElementUtils getElementUtils() {
		return this.elementUtils;
	}

	@Override
	public List<? extends DocTree> getTypeParameterComment(Element element, String parameterElement, SarlDocletEnvironment environment) {
		final DocCommentTree commentTree = environment.getDocTrees().getDocCommentTree(element);
		if (commentTree != null) {
			for (final DocTree docTag : commentTree.getBlockTags()) {
				if (docTag instanceof ParamTree) {
					final ParamTree paramTag = (ParamTree) docTag;
					if (paramTag.isTypeParameter() && parameterElement.equals(paramTag.getName().toString())) {
						return paramTag.getDescription();
					}
				}
			}
		}
		return Collections.emptyList();
	}

	@Override
	public List<? extends DocTree> getCommentForDeprecatedTag(DocTree tree) {
		final List<DocTree> documentation = new ArrayList<>();
		tree.accept(new SimpleDocTreeVisitor<Void, Void>() {
			@Override
			public Void visitDeprecated(DeprecatedTree node, Void p) {
				documentation.addAll(node.getBody());
				return null;
			}
		}, null);
		return documentation;
	}

	@Override
	public List<? extends BlockTagTree> getBlockTags(Element element, Kind type, SarlDocletEnvironment environment) {
		final List<BlockTagTree> list = new ArrayList<>();
		final DocCommentTree commentTree = environment.getDocTrees().getDocCommentTree(element);
		if (commentTree != null) {
			for (final DocTree tagBlock : commentTree.getBlockTags()) {
				if (tagBlock.getKind() == type && tagBlock instanceof BlockTagTree) {
					list.add((BlockTagTree) tagBlock);
				}
			}
		}
		return list;
	}

	@Override
	public List<? extends BlockTagTree> getBlockTags(Element element, SarlDocletEnvironment environment) {
		final List<BlockTagTree> list = new ArrayList<>();
		final DocCommentTree commentTree = environment.getDocTrees().getDocCommentTree(element);
		if (commentTree != null) {
			for (final DocTree tagBlock : commentTree.getBlockTags()) {
				if (tagBlock instanceof BlockTagTree) {
					list.add((BlockTagTree) tagBlock);
				}
			}
		}
		return list;
	}

	@Override
	public Location getTagletLocation(Element element) {
		if (element != null) {
			switch (element.getKind()) {
			case CONSTRUCTOR:
				return Location.CONSTRUCTOR;
			case ANNOTATION_TYPE:
			case CLASS:
			case ENUM:
			case INTERFACE:
				return Location.TYPE;
			case ENUM_CONSTANT:
			case EXCEPTION_PARAMETER:
			case FIELD:
			case LOCAL_VARIABLE:
			case PARAMETER:
			case RESOURCE_VARIABLE:
			case TYPE_PARAMETER:
				return Location.FIELD;
			case INSTANCE_INIT:
			case METHOD:
			case STATIC_INIT:
				return Location.METHOD;
			case MODULE:
				return Location.MODULE;
			case PACKAGE:
				return Location.PACKAGE;
			case OTHER:
			default:
				break;
			}
		}
		return Location.OVERVIEW;
	}

	@Override
	public List<? extends DocTree> getInheritedDocumentation(Element element, SarlDocletEnvironment environment) {
		final List<DocTree> doc = new ArrayList<>();
		final DocCommentTree doc0 = getInheritedFullDocumentation(element, environment);
		if (doc0 != null) {
			final List<? extends DocTree> doc1 = doc0.getFullBody();
			if (doc1 != null) {
				doc.addAll(doc1);
			}
		}
		return doc;
	}

	@Override
	public DocCommentTree getInheritedFullDocumentation(Element element, SarlDocletEnvironment environment) {
		final TypeElement type = getElementUtils().getEnclosingTypeElement(element);
		final Elements elts = environment.getElementUtils();
		final Collection<? extends Element> inheritedElements;
		if (element instanceof ExecutableElement) {
			final ExecutableElement ee = (ExecutableElement) element;
			inheritedElements = getTypeHierarchy().getInheritedElements(
					type, true, true, false, false, environment, it -> {
				return it instanceof ExecutableElement && elts.overrides(ee, (ExecutableElement) it, type);
			});
		} else {
			inheritedElements = getTypeHierarchy().getInheritedElements(
					type, true, true, false, false, environment, it -> {
				return elts.hides(element, it);
			});
		}
		if (!inheritedElements.isEmpty()) {
			for (final Element superElement : inheritedElements) {
				final DocCommentTree doc0 = environment.getDocTrees().getDocCommentTree(superElement);
				if (doc0 != null) {
					return doc0;
				}
			}
		}
		return null;
	}

}
