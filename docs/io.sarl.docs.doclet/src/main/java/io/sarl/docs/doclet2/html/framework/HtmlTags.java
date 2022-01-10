/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2022 the original authors or authors.
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

package io.sarl.docs.doclet2.html.framework;

/** Tags for HTML document.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public interface HtmlTags {

	/** Replies if the given name represents a pseudo tag.
	 *
	 * @param name the tag name to test.
	 * @return {@code true} if the given name is for a pseudo tag.
	 */
	static boolean isPseudoTag(String name) {
		return name != null && name.startsWith("#");
	}

	/** Create the name of a pseudo tag.
	 *
	 * @param name the tag name.
	 * @return the pseudo tag name.
	 */
	static String pseudoTag(String name) {
		return "#" + name;
	}

	/** Hidden pseudo tag.
	 */
	String HIDDEN_PSEUDO_TAG = pseudoTag("temporaryhiddentag");

	/** Hidden pseudo tag for references.
	 */
	String REFERENCE_PSEUDO_TAG = pseudoTag("temporaryreference");

	/** &lt;html /&gt;.
	 */
	String HTML_TAG = "html";

	/** &lt;meta /&gt;.
	 */
	String META_TAG = "meta";

	/** &lt;head /&gt;.
	 */
	String HEAD_TAG = "head";

	/** &lt;body /&gt;.
	 */
	String BODY_TAG = "body";

	/** &lt;frameset /&gt;.
	 */
	String FRAMESET_TAG = "frameset";

	/** &lt;frame /&gt;.
	 */
	String FRAME_TAG = "frame";

	/** &lt;noframes /&gt;.
	 */
	String NOFRAMES_TAG = "noframes";

	/** &lt;a /&gt;.
	 */
	String A_TAG = "a";

	/** &lt;p /&gt;.
	 */
	String P_TAG = "p";

	/** &lt;title /&gt;.
	 */
	String TITLE_TAG = "title";

	/** &lt;code /&gt;.
	 */
	String CODE_TAG = "code";

	/** &lt;link /&gt;.
	 */
	String LINK_TAG = "link";

	/** &lt;script /&gt;.
	 */
	String SCRIPT_TAG = "script";

	/** &lt;noscript /&gt;.
	 */
	String NOSCRIPT_TAG = "noscript";

	/** &lt;div /&gt;.
	 */
	String DIV_TAG = "div";

	/** &lt;button /&gt;.
	 */
	String BUTTON_TAG = "button";

	/** &lt;h2 /&gt;.
	 */
	String H2_TAG = "h2";

	/** &lt;dl /&gt;.
	 */
	String DL_TAG = "dl";

	/** &lt;dt /&gt;.
	 */
	String DT_TAG = "dt";

	/** &lt;dd /&gt;.
	 */
	String DD_TAG = "dd";

	/** &lt;span /&gt;.
	 */
	String SPAN_TAG = "span";

	/** &lt;table /&gt;.
	 */
	String TABLE_TAG = "table";

	/** &lt;thead /&gt;.
	 */
	String THEAD_TAG = "thead";

	/** &lt;tbody /&gt;.
	 */
	String TBODY_TAG = "tbody";

	/** &lt;tr /&gt;.
	 */
	String TR_TAG = "tr";

	/** &lt;th /&gt;.
	 */
	String TH_TAG = "th";

	/** &lt;td /&gt;.
	 */
	String TD_TAG = "td";

	/** &lt;pre /&gt;.
	 */
	String PRE_TAG = "pre";

	/** &lt;ul /&gt;.
	 */
	String UL_TAG = "ul";

	/** &lt;li /&gt;.
	 */
	String LI_TAG = "li";

	/** &lt;br /&gt;.
	 */
	String BR_TAG = "br";

	/** &amp;nbsp;.
	 */
	String SPACE_ENTITY = "&nbsp;";

	/** href="".
	 */
	String HREF_ATTR = "href";

	/** rel="".
	 */
	String REL_ATTR = "rel";

	/** "stylesheet" attribute value.
	 */
	String REL_ATTR_VALUE = "stylesheet";

	/** style="".
	 */
	String STYLE_ATTR = "style";

	/** src="".
	 */
	String SRC_ATTR = "src";

	/** type="".
	 */
	String TYPE_ATTR = "type";

	/** name="".
	 */
	String NAME_ATTR = "name";

	/** "display:none".
	 */
	String STYLE_NO_DISPLAY_ATTR_VALUE = "display:none;";

	/** onclick="".
	 */
	String ONCLICK_ATTR = "onclick";

	/** scrolling="".
	 */
	String SCROLLING_ATTR = "scrolling";

	/** "yes".
	 */
	String BOOLEAN_YES_ATTR_VALUE = "yes";

	/** onload="".
	 */
	String ONLOAD_ATTR = "onload";

	/** "active".
	 */
	String ACTIVE_ATTR_VALUE = "active";

	/** cols="".
	 */
	String COLS_ATTR = "cols";

	/** rows="".
	 */
	String ROWS_ATTR = "rows";

	/** title="".
	 */
	String TITLE_ATTR = "title";

	/** target="".
	 */
	String TARGET_ATTR = "target";

	/** lang="".
	 */
	String LANG_ATTR = "lang";

	/** "us".
	 */
	String DEFAULT_LANG_ATTR_VALUE = "en";

	/** charset="".
	 */
	String CHARSET_ATTR = "charset";

}
