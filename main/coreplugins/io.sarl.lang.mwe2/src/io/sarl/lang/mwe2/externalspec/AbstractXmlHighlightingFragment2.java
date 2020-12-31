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

package io.sarl.lang.mwe2.externalspec;

import java.nio.channels.UnsupportedAddressTypeException;
import java.text.MessageFormat;

import com.google.common.escape.Escaper;
import com.google.common.xml.XmlEscapers;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xtext.generator.CodeConfig;

/**
 * An abstract generator of an XML-based external language specification.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractXmlHighlightingFragment2 extends AbstractExternalHighlightingFragment2<IXmlStyleAppendable> {

	@Override
	protected IXmlStyleAppendable newStyleAppendable() {
		return new XmlAppendable(getCodeConfig(), getLanguageSimpleName(), getLanguageVersion());
	}

	/** Appendable for xml-based styles.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class XmlAppendable extends AbstractAppendable implements IXmlStyleAppendable {

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected XmlAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			super(" ", codeConfig, languageName, languageVersion); //$NON-NLS-1$
		}

		@Override
		public void appendComment(String text, Object... parameters) {
			appendCommentNoNl(text, parameters);
			newLine();
		}

		/** Append a comment without newline at the end.
		 *
		 * @param text the comment text.
		 * @param parameters the parameters.
		 */
		void appendCommentNoNl(String text, Object... parameters) {
			final String comment = applyFormat(text, parameters);
			appendNl("<!-- "); //$NON-NLS-1$
			for (final String line : comment.split("[\n\r]")) { //$NON-NLS-1$
				appendNl("\t " + line.trim()); //$NON-NLS-1$
			}
			append("-->"); //$NON-NLS-1$
		}

		@Override
		public void appendHeader() {
			appendNl(MessageFormat.format("<?xml version=\"1.0\" encoding=\"{0}\"?>", //$NON-NLS-1$
					getCodeConfig().getEncoding()));
			final String[] header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
			appendNl("<!--"); //$NON-NLS-1$
			for (final String headerLine : header) {
				appendNl(headerLine.replaceFirst("^\\s*[/]?[*][/]?", "\t ") //$NON-NLS-1$//$NON-NLS-2$
						.replaceFirst("\\s+$", "")); //$NON-NLS-1$//$NON-NLS-2$
			}
			appendNl("-->"); //$NON-NLS-1$
			appendNl("<!-- Style for {0} {1} -->", getLanguageSimpleName(), getLanguageVersion()); //$NON-NLS-1$
			newLine();
		}

		/** Open a tag.
		 *
		 * @param tag the tag name.
		 * @param nameValuePairs the properties.
		 */
		void internalOpenTag(String tag, String... nameValuePairs) {
			append("<").append(tag); //$NON-NLS-1$
			for (int i = 0; i < nameValuePairs.length; i = i + 2) {
				append(" "); //$NON-NLS-1$
				append(nameValuePairs[i]);
				append("=\""); //$NON-NLS-1$
				append(XmlEscapers.xmlAttributeEscaper().escape(nameValuePairs[i + 1]));
				append("\""); //$NON-NLS-1$
			}
		}

		@Override
		public IXmlStyleCloseable open(String tag, String... nameValuePairs) {
			internalOpenTag(tag, nameValuePairs);
			append(">"); //$NON-NLS-1$
			increaseIndentation();
			return new XmlCloseable(tag, this);
		}

		@Override
		public void appendTag(String tag, String... nameValuePairs) {
			appendTagNoNl(tag, nameValuePairs);
			newLine();
		}

		/** Append a tag without newline at the end.
		 *
		 * @param tag the tag name.
		 * @param nameValuePairs the properties of the tag.
		 */
		void appendTagNoNl(String tag, String... nameValuePairs) {
			internalOpenTag(tag, nameValuePairs);
			append(" />"); //$NON-NLS-1$
		}

		@Override
		public void appendTagWithValue(String tag, String value, String... nameValuePairs) {
			appendTagWithValueNoNl(tag, value, nameValuePairs);
			newLine();
		}

		/** Append a valued tag without newline at the end.
		 *
		 * @param tag the tag name.
		 * @param value the value.
		 * @param nameValuePairs the properties of the tag.
		 */
		void appendTagWithValueNoNl(String tag, String value, String... nameValuePairs) {
			internalOpenTag(tag, nameValuePairs);
			if (Strings.isEmpty(value)) {
				append(" />"); //$NON-NLS-1$
			} else {
				append(">"); //$NON-NLS-1$
				final boolean block = value.contains("\n") || value.contains("\r"); //$NON-NLS-1$//$NON-NLS-2$
				final Escaper escaper = XmlEscapers.xmlContentEscaper();
				if (block) {
					increaseIndentation();
					boolean first = true;
					for (final String line : value.split("[\n\r]+")) { //$NON-NLS-1$
						if (first) {
							first = false;
						} else {
							newLine();
						}
						final String escapedValue = escaper.escape(line);
						append(escapedValue);
					}
					decreaseIndentation();
					newLine();
				} else {
					final String escapedValue = escaper.escape(value);
					append(escapedValue.trim());
				}
				append("</").append(tag).append(">"); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}

	}

	/** Appendable for xml-based styles.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class XmlCloseable implements IXmlStyleCloseable {

		private final String tag;

		private final XmlAppendable parent;

		private int nbLines = 1;

		/** Constructor.
		 *
		 * @param tag the tag name.
		 * @param parent the parent.
		 */
		protected XmlCloseable(String tag, XmlAppendable parent) {
			this.tag = tag;
			this.parent = parent;
		}

		@Override
		public void appendHeader() {
			throw new UnsupportedAddressTypeException();
		}

		@Override
		public void appendComment(String text, Object... parameters) {
			appendNewLines();
			this.parent.appendCommentNoNl(text, parameters);
			newLine();
		}

		@Override
		public ISourceAppender append(CharSequence string) {
			appendNewLines();
			this.parent.append(string);
			return this;
		}

		@Override
		public ISourceAppender append(JvmType type) {
			appendNewLines();
			this.parent.append(type);
			return this;
		}

		@Override
		public ISourceAppender append(LightweightTypeReference typeRef) {
			appendNewLines();
			this.parent.append(typeRef);
			return this;
		}

		@Override
		public ISourceAppender increaseIndentation() {
			appendNewLines();
			this.parent.increaseIndentation();
			return this;
		}

		@Override
		public ISourceAppender decreaseIndentation() {
			appendNewLines();
			this.parent.decreaseIndentation();
			return this;
		}

		@Override
		public boolean isJava() {
			return this.parent.isJava();
		}

		@Override
		public void appendTag(String tag, String... nameValuePairs) {
			appendNewLines();
			this.parent.appendTagNoNl(tag, nameValuePairs);
			newLine();
		}

		@Override
		public void appendTagWithValue(String tag, String value, String... nameValuePairs) {
			appendNewLines();
			this.parent.appendTagWithValueNoNl(tag, value, nameValuePairs);
			newLine();
		}

		@Override
		public IXmlStyleCloseable open(String tag, String... nameValuePairs) {
			appendNewLines();
			return this.parent.open(tag, nameValuePairs);
		}

		@Override
		public ISourceAppender newLine() {
			++this.nbLines;
			return this;
		}

		@Override
		public void close() {
			this.parent.decreaseIndentation();
			this.nbLines = Math.max(1, this.nbLines);
			appendNewLines();
			append("</").append(this.tag).append(">"); //$NON-NLS-1$ //$NON-NLS-2$
			this.parent.newLine();
		}

		private void appendNewLines() {
			final int nb = this.nbLines;
			this.nbLines = 0;
			for (int i = 0; i < nb; ++i) {
				this.parent.newLine();
			}
		}

	}

}
