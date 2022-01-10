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
