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

package io.sarl.docs.doclet2.html.taglets;

import java.text.MessageFormat;
import java.util.List;

import javax.lang.model.element.Element;

import com.sun.source.doctree.DocTree;
import jdk.javadoc.doclet.Taglet;

import io.sarl.docs.doclet2.html.framework.CssStyles;
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

}
