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
 */

package io.sarl.docs.doclet2.html.taglets.inline;

import java.text.MessageFormat;
import java.util.List;

import javax.lang.model.element.Element;
import javax.lang.model.element.VariableElement;
import javax.tools.Diagnostic.Kind;

import com.google.common.collect.Iterables;
import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.ReferenceTree;
import org.eclipse.xtext.util.Strings;
import org.jsoup.nodes.TextNode;

import io.sarl.docs.doclet2.html.framework.CssStyles;
import io.sarl.docs.doclet2.html.framework.HtmlFactoryContentExtractor;
import io.sarl.docs.doclet2.html.taglets.AbstractSarlTaglet;

/** Taglet for {@code @value} tag that is replaced by the value of the referenced constant.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class ValueTaglet extends AbstractSarlTaglet {

	/** Name of the taglet.
	 */
	public static final String TAGLET_NAME = "value";

	/** Constructor.
	 */
	public ValueTaglet() {
		super(TAGLET_NAME.toLowerCase(), true);
	}

	/** Replies the CSS style for the value.
	 *
	 * @param base the base CSS provided to the {@code @value} tag.
	 * @return the value's CSS style.
	 */
	protected CssStyles getCssStyle(CssStyles base) {
		return CssStyles.TAG_VALUE_COMMENT;
	}

	@Override
	public boolean appendNode(org.jsoup.nodes.Element parent, List<? extends DocTree> tags, Element element, DocTree sourceDocumentation, CssStyles style, HtmlFactoryContentExtractor contentExtractor) {
		final ReferenceTree node = (ReferenceTree) Iterables.find(tags, it -> it instanceof ReferenceTree);

		if (node == null) {
			contentExtractor.getContext().getReporter().print(Kind.ERROR, Messages.ValueTaglet_0);
			return false;
		}
		
		final Element referencedElement = contentExtractor.extractReferencedElement(node);
		if (referencedElement == null) {
			contentExtractor.getContext().getReporter().print(Kind.ERROR, MessageFormat.format(Messages.ValueTaglet_1, node.toString()));
			return false;
		}

		if (!(referencedElement instanceof VariableElement)) {
			contentExtractor.getContext().getReporter().print(Kind.ERROR, MessageFormat.format(Messages.ValueTaglet_2, node.toString()));
			return false;
		}

		final VariableElement variable = (VariableElement) referencedElement;
		final Object constantValue = variable.getConstantValue();
		
		if (constantValue == null) {
			contentExtractor.getContext().getReporter().print(Kind.ERROR, MessageFormat.format(Messages.ValueTaglet_3, variable.getSimpleName(), node.toString()));
			return false;
		}

		final TextNode textNode = new TextNode(formatValue(constantValue));
		final CssStyles theStyle = getCssStyle(style);
		if (theStyle != null) {
			final org.jsoup.nodes.Element spanNode = getHtmlFactory().createSpanTag(parent, theStyle);
			spanNode.appendChild(textNode);
		} else {
			parent.appendChild(textNode);
		}
		return true;
	}

	/** Format the given value for the documentation.
	 *
	 * <p>The value will be of a primitive type or a
     * {@code String}.  If the value is of a primitive type, it is
     * wrapped in the appropriate wrapper class (such as {@link
     * Integer}).
	 *
	 * @param value the value to format.
	 * @return the string representation of the value.
	 */
	protected String formatValue(Object value) {
		return defaultFormatValue(value);
	}
	
	/** Format the given value for the documentation.
	 *
	 * <p>The value will be of a primitive type or a
    * {@code String}.  If the value is of a primitive type, it is
    * wrapped in the appropriate wrapper class (such as {@link
    * Integer}).
	 *
	 * @param value the value to format.
	 * @return the string representation of the value.
	 */
	public static String defaultFormatValue(Object value) {
		if (value instanceof CharSequence) {
			return "\"" + Strings.convertToJavaString(value.toString()) + "\"";
		}
		return value.toString();
	}

}