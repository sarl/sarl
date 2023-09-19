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

package io.sarl.docs.doclet2.html.taglets;

import java.text.MessageFormat;
import java.util.List;

import javax.lang.model.element.Element;

import com.sun.source.doctree.DocTree;
import jdk.javadoc.doclet.Taglet;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;

/** Manager of taglets.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface SarlTaglet extends Taglet {

	/** Replies the label of the tag. By default, the label is the value replied by
	 * {@link #getName()} for which the first letters are upper cased.
	 *
	 * @return the label of the tag.
	 */
	default String getTagBlockLabel() {
		return buildBlockLabel(getName());
	}

	/** Returns the HTML representation of a series of instances of this tag to be included in the generated output.
	 * If this taglet is for an inline tag it will be called once per instance of the tag, each time with a singleton
	 * list. Otherwise, if this tag is a block tag, it will be called once per comment, with a list of all the
	 * instances of the tag in a comment.
	 *
	 * @param parent the receiver.
	 * @param tags the list of instances of this tag.
	 * @param element the element to which the enclosing comment belongs.
	 * @param sourceDocumentation the source documentation, of the given {@code element}.
	 * @param style the CSS style to be used.
	 * @param referenceExtractor extractor of HTML reference.
	 * @return {@code true} if some element was added. {@code false} if nothing was added.
	 */
	boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor referenceExtractor);

	/** Create a standard label for a block tag.
	 *
	 * @param name the name of the tag.
	 * @return the label
	 */
	static String buildBlockLabel(String name) {
		return MessageFormat.format(Messages.SarlTaglet_0, org.eclipse.xtext.util.Strings.toFirstUpper(name));
	}

	/** Replies if the taglet is active according to the given options for the doclet.
	 *
	 * @param docletOptions the doclet options.
	 * @return {@code true} if the taglet is active.
	 */
	default boolean isActiveTaglet(DocletOptions docletOptions) {
		return true;
	}

}
