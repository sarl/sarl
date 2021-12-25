/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.Node;

/** Accessor tool to HTML elements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 * @see HtmlFactory
 */
public interface HtmlAccessor {

	/** Replies the root element for the given document.
	 * Usually, it is a &lt;html/&gt; tag.
	 *
	 * @param document the document to parse.
	 * @return the root element.
	 */
	Element getRootElement(Document document);

	/** Replies the first direct child node with the given tag name.
	 *
	 * @param parent the parent node
	 * @param tagName the expected tag name.
	 * @return the child node or {@code null} if there is no child node.
	 */
	Element getChildNode(Node parent, String tagName);

	/** Replies the first direct child node with the given tag name and of the given type.
	 *
	 * @param parent the parent node
	 * @param tagName the expected tag name.
	 * @param expectedType the expected type for the child node.
	 * @return the child node or {@code null} if there is no child node.
	 */
	<T extends Element> T getChildNode(Node parent, String tagName, Class<T> expectedType);

}
