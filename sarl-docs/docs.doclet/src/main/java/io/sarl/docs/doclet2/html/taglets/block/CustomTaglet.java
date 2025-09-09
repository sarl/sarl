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

package io.sarl.docs.doclet2.html.taglets.block;

import java.util.ArrayList;
import java.util.List;

import javax.lang.model.element.Element;

import com.sun.source.doctree.DocTree;

import io.sarl.docs.doclet2.framework.CustomTag;
import io.sarl.docs.doclet2.framework.CustomTagLocation;
import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;
import io.sarl.docs.doclet2.html.taglets.SarlTaglet;

/** Taglet defined by the user.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class CustomTaglet extends AbstractSarlTaglet {

	private final String header;
	
	/** Constructor.
	 *
	 * @param name the name of the taglet.
	 * @param header the text that will be used as header for this taglet.
	 * @param locations the locations of the taglet.
	 */
	public CustomTaglet(String name, String header, Location... locations) {
		super(name.toLowerCase(), false, locations);
		this.header = header;
	}

	/** Constructor.
	 *
	 * @param ctag the custom tag description.
	 */
	public CustomTaglet(CustomTag ctag) {
		this(ctag.name(), ctag.header(), toLocations(ctag.locations()));
	}

	private static Location[] toLocations(List<CustomTagLocation> locations) {
		final var locs = new ArrayList<Location>();
		for (final var ctl : locations) {
			for (final var loc : ctl.toJavadocLocation()) {
				locs.add(loc);
			}
		}
		return locs.toArray(new Location[locs.size()]);
	}

	/** Replies the CSS style for the author text.
	 *
	 * @param original the original CSS style.
	 * @return the CSS style.
	 */
	@SuppressWarnings("static-method")
	protected CssStyles getTextCssStyle(CssStyles original) {
		return CssStyles.TAG_CUSTOMTAG_COMMENT;
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor referenceExtractor) {
    	return appendCommentTextWithComa(parent, tags, element, getTextCssStyle(style), referenceExtractor.getContext());
	}

	@Override
	public String getTagBlockLabel() {
		return SarlTaglet.buildBlockLabel(this.header);
	}

	@Override
	public boolean isActiveTaglet(DocletOptions docletOptions) {
		return true;
	}

}
