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
 */
public class HtmlAccessorImpl implements HtmlAccessor, HtmlTags {

	@Override
	public Element getChildNode(Node parent, String tagName) {
		for (final Node node : parent.childNodes()) {
			if (node instanceof Element) {
				final Element element = (Element) node;
				final String name = element.tagName();
				if (name == tagName || (name != null && name.equalsIgnoreCase(tagName))) {
					return element;
				}
			}
		}
		return null;
	}

	@Override
	public <T extends Element> T getChildNode(Node parent, String tagName, Class<T> expectedType) {
		for (final Node node : parent.childNodes()) {
			if (expectedType.isInstance(node)) {
				final T element = expectedType.cast(node);
				final String name = element.tagName();
				if (name == tagName || (name != null && name.equalsIgnoreCase(tagName))) {
					return element;
				}
			}
		}
		return null;
	}

	@Override
	public Element getRootElement(Document document) {
		return getChildNode(document, HTML_TAG);
	}

}
