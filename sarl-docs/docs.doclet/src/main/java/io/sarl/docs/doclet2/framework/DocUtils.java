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

package io.sarl.docs.doclet2.framework;

import java.util.List;

import javax.lang.model.element.Element;

import com.sun.source.doctree.BlockTagTree;
import com.sun.source.doctree.DocCommentTree;
import com.sun.source.doctree.DocTree;
import jdk.javadoc.doclet.Taglet.Location;

/** Utilities for documentation elements.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public interface DocUtils {

	/** Replies the description of the given type parameter.
	 *
	 * @param element the container of the comment.
	 * @param parameterElement the name of type parameter to search for.
	 * @param environment the generation environment.
	 * @return the description, or empty string.
	 */
	List<? extends DocTree> getTypeParameterComment(Element element, String parameterElement, SarlDocletEnvironment environment);

	/** Replies the comment tags that have the given name.
	 *
	 * @param element the code element from which the comment tags must be extracted.
	 * @param type the type of tag.
	 * @param environment the generation environment.
	 * @return the tags.
	 */
	List<? extends BlockTagTree> getBlockTags(Element element, DocTree.Kind type, SarlDocletEnvironment environment);

	/** Replies the comment tags.
	 *
	 * @param element the code element from which the comment tags must be extracted.
	 * @param environment the generation environment.
	 * @return the tags.
	 */
	List<? extends BlockTagTree> getBlockTags(Element element, SarlDocletEnvironment environment);

	/** Replies the comment of the given documentation tree.
	 *
	 * @param tree the comment tree.
	 * @return the comment text.
	 */
	List<? extends DocTree> getCommentForDeprecatedTag(DocTree tree);

	/** Replies the taglet location that corresponds to the given element.
	 *
	 * @param element the documented element.
	 * @return the taglet location.
	 */
	Location getTagletLocation(Element element);

	/** Replies the inherited documentation for the give element.
	 * Inherited documentation is those that is inside the super element, not in the given element.
	 *
	 * @param element the container of the comment.
	 * @param environment the generation environment.
	 * @return the inherited documentation.
	 */
	List<? extends DocTree> getInheritedDocumentation(Element element, SarlDocletEnvironment environment);

	/** Replies the full content of the inherited documentation for the give element.
	 * Inherited documentation is those that is inside the super element, not in the given element.
	 *
	 * @param element the container of the comment.
	 * @param environment the generation environment.
	 * @return the inherited documentation, or {@code null}.
	 */
	DocCommentTree getInheritedFullDocumentation(Element element, SarlDocletEnvironment environment);

}
