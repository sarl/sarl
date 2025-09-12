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

package io.sarl.docs.doclet2.html.framework;

import java.util.List;

import javax.lang.model.element.Element;

import com.sun.source.doctree.DocTree;
import org.jsoup.nodes.Node;

/** Tool for extracting an HTML reference from the internal structure of a {@link HtmlFactory}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 * @see HtmlFactory
 */
public interface HtmlFactoryContentExtractor {

	/** Replies the context of the HTML factory caller.
	 *
	 * @return the context.
	 */
	HtmlFactoryContext getContext();
	
	/** Extract an HTML reference from the current state of the HTML factory parser.
	 *
	 * @param docNode the node to parse.
	 * @param label the label of the reference or {@code null}.
	 * @return the nodes that represents the reference.
	 */
	default List<Node> extractReference(DocTree docNode, List<Node> label) {
		return extractReference(docNode, label, false);
	}

	/** Extract a referenced element from the current state of the HTML factory parser.
	 *
	 * @param docNode the node to parse.
	 * @return the referenced element.
	 */
	Element extractReferencedElement(DocTree docNode);

	/** Extract an HTML reference from the current state of the HTML factory parser.
	 *
	 * @param docNode the node to parse.
	 * @param label the label of the reference or {@code null}.
	 * @param isplain indicates if the reference is for a plain link.
	 * @return the nodes that represents the reference.
	 */
	List<Node> extractReference(DocTree docNode, List<Node> label, boolean isplain);

	/** Extract the given text from the documentation and put it in a HTML node.
	 *
	 * @param text the documentation text.
	 * @return the node.
	 */
	org.jsoup.nodes.Element extractSimpleText(List<? extends DocTree> text);

}
