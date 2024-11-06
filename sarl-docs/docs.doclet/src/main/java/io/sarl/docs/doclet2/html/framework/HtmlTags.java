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

package io.sarl.docs.doclet2.html.framework;

/** Tags for HTML document.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.14.0 20241106-161409
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public interface HtmlTags {

	/** Replies if the given name represents a pseudo tag.
	 *
	 * @param name the tag name to test.
	 * @return {@code true} if the given name is for a pseudo tag.
	 */
	static boolean isPseudoTag(String name) {
		return name != null && name.startsWith("#"); //$NON-NLS-1$
	}

	/** Create the name of a pseudo tag.
	 *
	 * @param name the tag name.
	 * @return the pseudo tag name.
	 */
	static String pseudoTag(String name) {
		return "#" + name; //$NON-NLS-1$
	}

	/** Hidden pseudo tag.
	 */
	String HIDDEN_PSEUDO_TAG = pseudoTag("temporaryhiddentag"); //$NON-NLS-1$

	/** Hidden pseudo tag for references.
	 */
	String REFERENCE_PSEUDO_TAG = pseudoTag("temporaryreference"); //$NON-NLS-1$

	/** &lt;html /&gt;.
	 */
	String HTML_TAG = "html"; //$NON-NLS-1$

	/** &lt;meta /&gt;.
	 */
	String META_TAG = "meta"; //$NON-NLS-1$

	/** &lt;head /&gt;.
	 */
	String HEAD_TAG = "head"; //$NON-NLS-1$

	/** &lt;body /&gt;.
	 */
	String BODY_TAG = "body"; //$NON-NLS-1$

	/** &lt;frameset /&gt;.
	 */
	String FRAMESET_TAG = "frameset"; //$NON-NLS-1$

	/** &lt;frame /&gt;.
	 */
	String FRAME_TAG = "frame"; //$NON-NLS-1$

	/** &lt;noframes /&gt;.
	 */
	String NOFRAMES_TAG = "noframes"; //$NON-NLS-1$

	/** &lt;a /&gt;.
	 */
	String A_TAG = "a"; //$NON-NLS-1$

	/** &lt;p /&gt;.
	 */
	String P_TAG = "p"; //$NON-NLS-1$

	/** &lt;title /&gt;.
	 */
	String TITLE_TAG = "title"; //$NON-NLS-1$

	/** &lt;code /&gt;.
	 */
	String CODE_TAG = "code"; //$NON-NLS-1$

	/** &lt;link /&gt;.
	 */
	String LINK_TAG = "link"; //$NON-NLS-1$

	/** &lt;script /&gt;.
	 */
	String SCRIPT_TAG = "script"; //$NON-NLS-1$

	/** &lt;noscript /&gt;.
	 */
	String NOSCRIPT_TAG = "noscript"; //$NON-NLS-1$

	/** &lt;div /&gt;.
	 */
	String DIV_TAG = "div"; //$NON-NLS-1$

	/** &lt;button /&gt;.
	 */
	String BUTTON_TAG = "button"; //$NON-NLS-1$

	/** &lt;h2 /&gt;.
	 */
	String H2_TAG = "h2"; //$NON-NLS-1$

	/** &lt;dl /&gt;.
	 */
	String DL_TAG = "dl"; //$NON-NLS-1$

	/** &lt;dt /&gt;.
	 */
	String DT_TAG = "dt"; //$NON-NLS-1$

	/** &lt;dd /&gt;.
	 */
	String DD_TAG = "dd"; //$NON-NLS-1$

	/** &lt;span /&gt;.
	 */
	String SPAN_TAG = "span"; //$NON-NLS-1$

	/** &lt;table /&gt;.
	 */
	String TABLE_TAG = "table"; //$NON-NLS-1$

	/** &lt;thead /&gt;.
	 */
	String THEAD_TAG = "thead"; //$NON-NLS-1$

	/** &lt;tbody /&gt;.
	 */
	String TBODY_TAG = "tbody"; //$NON-NLS-1$

	/** &lt;tr /&gt;.
	 */
	String TR_TAG = "tr"; //$NON-NLS-1$

	/** &lt;th /&gt;.
	 */
	String TH_TAG = "th"; //$NON-NLS-1$

	/** &lt;td /&gt;.
	 */
	String TD_TAG = "td"; //$NON-NLS-1$

	/** &lt;pre /&gt;.
	 */
	String PRE_TAG = "pre"; //$NON-NLS-1$

	/** &lt;ul /&gt;.
	 */
	String UL_TAG = "ul"; //$NON-NLS-1$

	/** &lt;li /&gt;.
	 */
	String LI_TAG = "li"; //$NON-NLS-1$

	/** &lt;br /&gt;.
	 */
	String BR_TAG = "br"; //$NON-NLS-1$

	/** &amp;nbsp;.
	 */
	String SPACE_ENTITY = "&nbsp;"; //$NON-NLS-1$

	/** href="".
	 */
	String HREF_ATTR = "href"; //$NON-NLS-1$

	/** rel="".
	 */
	String REL_ATTR = "rel"; //$NON-NLS-1$

	/** "stylesheet" attribute value.
	 */
	String REL_ATTR_VALUE = "stylesheet"; //$NON-NLS-1$

	/** style="".
	 */
	String STYLE_ATTR = "style"; //$NON-NLS-1$

	/** src="".
	 */
	String SRC_ATTR = "src"; //$NON-NLS-1$

	/** type="".
	 */
	String TYPE_ATTR = "type"; //$NON-NLS-1$

	/** name="".
	 */
	String NAME_ATTR = "name"; //$NON-NLS-1$

	/** "display:none".
	 */
	String STYLE_NO_DISPLAY_ATTR_VALUE = "display:none;"; //$NON-NLS-1$

	/** onclick="".
	 */
	String ONCLICK_ATTR = "onclick"; //$NON-NLS-1$

	/** scrolling="".
	 */
	String SCROLLING_ATTR = "scrolling"; //$NON-NLS-1$

	/** "yes".
	 */
	String BOOLEAN_YES_ATTR_VALUE = "yes"; //$NON-NLS-1$

	/** onload="".
	 */
	String ONLOAD_ATTR = "onload"; //$NON-NLS-1$

	/** "active".
	 */
	String ACTIVE_ATTR_VALUE = "active"; //$NON-NLS-1$

	/** cols="".
	 */
	String COLS_ATTR = "cols"; //$NON-NLS-1$

	/** rows="".
	 */
	String ROWS_ATTR = "rows"; //$NON-NLS-1$

	/** title="".
	 */
	String TITLE_ATTR = "title"; //$NON-NLS-1$

	/** target="".
	 */
	String TARGET_ATTR = "target"; //$NON-NLS-1$

	/** lang="".
	 */
	String LANG_ATTR = "lang"; //$NON-NLS-1$

	/** "us".
	 */
	String DEFAULT_LANG_ATTR_VALUE = "en"; //$NON-NLS-1$

	/** charset="".
	 */
	String CHARSET_ATTR = "charset"; //$NON-NLS-1$

}
