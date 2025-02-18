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

package io.sarl.docs.doclet2.html.framework;

/** Names of styles for the CSS.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
@SuppressWarnings("all")
public enum CssStyles {
	
	/** See in several CSS style sheets.
	 */
	ACTIVE("active"),

	/** See {@code general.css}.
	 */
	CONTENT("content"),

	/** See {@code general.css}.
	 */
	FRAME("frame"),

	/** See {@code header.css}.
	 */
	HEADER("header"),
	
	/** See {@code header.css}.
	 */
	FOOTER("footer"),

	/** See {@code header.css}.
	 */
	HEADER_PRIVATEAPI_MESSAGE("header", "privateapi", "message"),

	/** See {@code header.css}.
	 */
	HEADER_MODULE_NAME("header", "modulename"),

	/** See {@code header.css}.
	 */
	HEADER_PACKAGE_NAME("header", "packagename"),

	/** See {@code header.css}.
	 */
	HEADER_TYPE_NAME("header", "typename"),

	/** See {@code header.css}.
	 */
	HEADER_SMALL_TITLE("header", "smalltitle"),

	/** See {@code inheritancetree.css}.
	 */
	INHERITANCE_TREE("inheritancetree"),

	/** See {@code inheritancetree.css}.
	 */
	INHERITANCE_TREE_TREE("inheritancetree", "tree"),

	/** See {@code inheritancetree.css}.
	 */
	INHERITANCE_TREE_TYPE("inheritancetree", "type"),

	/** See {@code typeinfobox.css}.
	 */
	TYPE_INFO_BOX("typeinfobox"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_BOX("summarybox"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_BOX_TITLE("summarybox", "title"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_BOX_TAB_TITLE("summarybox", "tab", "title"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_BOX_TAB_CONTENT("summarybox", "tab", "content"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_BOX_ID("summarybox", "id"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_TABLE("summarytable"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_TABLE_TYPE_COLUMN("summarytable", "typecolumn"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_TABLE_DESCRIPTION_COLUMN("summarytable", "descriptioncolumn"),

	/** See {@code summarybox.css}.
	 */
	SUMMARY_BOX_INHERITED("summarybox", "inherited"),

	/** See {@code detailbox.css}.
	 */
	DETAIL_BOX("detailbox"),

	/** See {@code detailbox.css}.
	 */
	DETAIL_BOX_TITLE("detailbox", "title"),

	/** See {@code general.css}.
	 */
	COPYRIGHT_BOX("copyrightbox"),

	/** See {@code detailbox.css}.
	 */
	DETAIL_BOX_TAG("detailbox", "tag"),

	TYPE_PARAMETER_LIST("typeparameterlist"),

	/** See {@code general.css}.
	 */
	PRE_INDENT("pre", "indent"),

	/** See {@code general.css}.
	 */
	KEYWORD("keyword"),

	/** See {@code general.css}.
	 */
	EMPH("emph"),

	/** See {@code index.css}.
	 */
	INDEX("index"),

	/** See {@code navigation.css}.
	 */
	NAVIGATION("navigation"),

	/** See {@code navigation.css}.
	 */
	GLOBAL_NAVIGATION("navigation", "global"),

	/** See {@code navigation.css}.
	 */
	LOCAL_NAVIGATION("navigation", "local"),

	/** See {@code navigation.css}.
	 */
	NAVIGATION_MAIN_LIST("navigation", "mainlist"),

	/** See {@code navigation.css}.
	 */
	NAVIGATION_LIST("navigation", "list"),

	/** See {@code navigation.css}.
	 */
	NAVIGATION_SUBLIST("navigation", "sublist"),

	SUPER_INTERFACE_LIST("superinterfacelist"),

	DIRECT_SUBTYPE_LIST("directsubtypelist"),

	IMPLEMENTING_CLASS_LIST("implementingclasslist"),

	/** See {@code general.css}.
	 */
	BULLET_LESS_LIST("bulletlesslist"),

	NESTED_TYPE_INFO("nestedtypeinfo"),

	TYPE_SIGNATURE("typesignature"),

	TYPE_SIGNATURE_ANNOTATION_INFO("typesignature", "annotationinfo"),

	TYPE_SIGNATURE_ANNOTATION_INFO_VALUE("typesignature", "annotationinfo", "value"),

	TYPE_SIGNATURE_TYPE_NAME("typesignature", "typename"),

	DEPRECATION_INFO("deprecationinfo"),

	/** See {@code general.css}.
	 */
	DEPRECATION_INFO_TITLE("deprecationinfo", "title"),

	/** See {@code moduleinfobox.css}.
	 */
	MODULE_DESCRIPTION("moduledescription"),

	MODULE_TAG_INFO("moduletaginfo"),

	/** See {@code packageinfobox.css}.
	 */
	PACKAGE_DESCRIPTION("packagedescription"),

	PACKAGE_TAG_INFO("packagetaginfo"),

	TYPE_DESCRIPTION("typedescription"),

	TYPE_DESCRIPTION_MAIN("typedescription", "main"),

	TYPE_TAG_INFO("typetaginfo"),
	
	TAG_DEPRECATED_COMMENT("tags", "deperecated", "comment"),

	TAG_GENERATED_COMMENT("tags", "generated", "comment"),

	TAG_ORDERED_COMMENT("tags", "ordered", "comment"),

	TAG_PRIVATEAPI_COMMENT("tags", "privateapi", "comment"),

	TAG_RETURN_COMMENT("tags", "return", "comment"),

	TAG_SEE_COMMENT("tags", "see", "comment"),

	TAG_SINCE_COMMENT("tags", "since", "comment"),

	TAG_THROWS_COMMENT("tags", "throws", "comment"),

	TAG_FIRES_COMMENT("tags", "fires", "comment"),

	/** See {@code inline.css}.
	 */
	TAG_VALUE_COMMENT("tags", "value", "comment"),

	TAG_USES_COMMENT("tags", "uses", "comment"),

	TAG_SERIAL_COMMENT("tags", "serial", "comment"),

	TAG_SERIALFIELD_COMMENT("tags", "serialfield", "comment"),

	TAG_SERIALDATA_COMMENT("tags", "serialdata", "comment"),

	TAG_PROVIDES_COMMENT("tags", "provides", "comment"),

	TAG_PARAM_COMMENT("tags", "param", "comment"),

	TAG_OPTIONALPARAM_COMMENT("tags", "optionalparam", "comment"),

	TAG_MODEL_COMMENT("tags", "model", "comment"),

	TAG_LITERAL_COMMENT("tags", "literal", "comment"),

	TAG_LINK_COMMENT("tags", "link", "comment"),

	TAG_CODE_COMMENT("tags", "code", "comment"),

	TAG_AUTHOR_COMMENT("tags", "author", "comment"),

	TAG_MAVENGROUPID_COMMENT("tags", "mavengroupid", "comment"),

	TAG_MAVENARTIFACTID_COMMENT("tags", "mavenartifactid", "comment"),

	TAG_INDEX_COMMENT("tags", "index", "comment"),

	TAG_VERSION_COMMENT("tags", "version", "comment"),

	TAG_CUSTOMTAG_COMMENT("tags", "customtag", "comment"),

	/** See {@code inline.css}.
	 */
	TAG_COMMENT("tags", "comment"),

	/** See {@code tabs.css}.
	 */
	TABS_TITLE("tabs", "title"),

	/** See {@code tabs.css}.
	 */
	TABS_CONTENT("tabs", "content");

	/** List of CSS files.
	 */
	public static final String[] CSS_RESOURCES = {
			"general.css",
			"header.css",
			"inheritancetree.css",
			"typeinfobox.css",
			"moduleinfobox.css",
			"packageinfobox.css",
			"summarybox.css",
			"detailbox.css",
			"inline.css",
			"tabs.css",
			"navigation.css",
			"index.css",
		};

	/** List of JS files.
	 */
	public static final String[] JS_RESOURCES = {
			"tabs.js",
		};

	private static final String SEPARATOR = "-";

	private static final String PREFIX = "doclet";

	private final String cssClassname;
	
	private CssStyles(String... classnames) {
		final StringBuilder name = new StringBuilder();
		name.append(PREFIX);
		for (final String elt : classnames) {
			name.append(SEPARATOR);
			name.append(elt);
		}
		this.cssClassname = name.toString();
	}

	/** Replies the CSS name for this style.
	 *
	 * @return the name to put in the CSS class attribute.
	 */
	public String getCssClassname() {
		return this.cssClassname;
	}

}
