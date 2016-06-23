/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.lang.mwe2.externalLang;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.google.common.xml.XmlEscapers;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * An abstract generator of an XML-based external language specification.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractXmlHighlightingFragment2 extends AbstractExternalHighlightingFragment2 {
	
	private final List<String> lines = new ArrayList<>();
	
	private final List<String> openedContexts = new ArrayList<>();

	private String indent() {
		StringBuilder builder = new StringBuilder();
		for (int i = 0; i < this.openedContexts.size(); ++i) {
			builder.append("\t"); //$NON-NLS-1$
		}
		return builder.toString();
	}

	/** Open the tag.
	 *
	 * @param tag the name of the tag.
	 * @param nameValuePairs the parameters of the tag (name-value pairs).
	 * @see #close()
	 */
	protected void open(String tag, String... nameValuePairs) {
		StringBuilder line = new StringBuilder();
		line.append(indent());
		line.append("<").append(tag); //$NON-NLS-1$
		for (int i = 0; i < nameValuePairs.length; i = i + 2) {
			line.append(" "); //$NON-NLS-1$
			line.append(nameValuePairs[i]);
			line.append("=\""); //$NON-NLS-1$
			line.append(XmlEscapers.xmlAttributeEscaper().escape(nameValuePairs[i + 1]));
			line.append("\""); //$NON-NLS-1$
		}
		line.append(">"); //$NON-NLS-1$
		this.openedContexts.add(tag);
		this.lines.add(line.toString());
	}
	
	/** Close a tag.
	 *
	 * @see #open(String, String...)
	 */
	protected void close() {
		String closable = this.openedContexts.remove(this.openedContexts.size() - 1);
		this.lines.add(indent() + "</" + closable + ">"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Open and close a tag.
	 *
	 * @param tag the name of the tag.
	 * @param nameValuePairs the parameters of the tag (name-value pairs).
	 */
	protected void tag(String tag, String... nameValuePairs) {
		StringBuilder line = new StringBuilder();
		line.append(indent());
		line.append("<").append(tag); //$NON-NLS-1$
		for (int i = 0; i < nameValuePairs.length; i = i + 2) {
			line.append(" "); //$NON-NLS-1$
			line.append(nameValuePairs[i]);
			line.append("=\""); //$NON-NLS-1$
			line.append(XmlEscapers.xmlAttributeEscaper().escape(nameValuePairs[i + 1]));
			line.append("\""); //$NON-NLS-1$
		}
		line.append(" />"); //$NON-NLS-1$
		this.lines.add(line.toString());
	}

	/** Create a tag with a value.
	 *
	 * @param tag the name of the tag.
	 * @param value the value.
	 * @param nameValuePairs the parameters of the tag (name-value pairs).
	 */
	protected void valuedTag(String tag, String value, String... nameValuePairs) {
		StringBuilder line = new StringBuilder();
		line.append(indent());
		line.append("<").append(tag); //$NON-NLS-1$
		for (int i = 0; i < nameValuePairs.length; i = i + 2) {
			line.append(" "); //$NON-NLS-1$
			line.append(nameValuePairs[i]);
			line.append("=\""); //$NON-NLS-1$
			line.append(XmlEscapers.xmlAttributeEscaper().escape(nameValuePairs[i + 1]));
			line.append("\""); //$NON-NLS-1$
		}
		line.append(">"); //$NON-NLS-1$
		String escapedValue = XmlEscapers.xmlContentEscaper().escape(value);
		line.append(escapedValue);
		if (escapedValue.endsWith(getCodeConfig().getLineDelimiter())) {
			line.append(indent());
		}
		line.append("</").append(tag).append(">"); //$NON-NLS-1$ //$NON-NLS-2$
		this.lines.add(line.toString());
	}

	/** Merge the given lines with indentation.
	 *
	 * @param prefix the prefix to add to each line.
	 * @param lines the lines.
	 * @return the merged elements.
	 */
	@Pure
	protected String lines(String prefix, String... lines) {
		String delimiter = getCodeConfig().getLineDelimiter();
		StringBuilder buffer = new StringBuilder();
		for (String line : lines) {
			buffer.append(indent());
			buffer.append(prefix);
			buffer.append(line);
			buffer.append(delimiter);
		}
		return buffer.toString();
	}

	@Override
	protected final void generate(Set<String> literals, Set<String> keywords,
			Set<String> punctuation, Set<String> ignored) {
		// Generate
		this.lines.clear();
		this.lines.add(MessageFormat.format("<?xml version=\"1.0\" encoding=\"{0}\"?>", //$NON-NLS-1$
				getCodeConfig().getEncoding()));
		
		String[] header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
		this.lines.add("<!--"); //$NON-NLS-1$
		for (String headerLine : header) {
			headerLine = headerLine.replaceFirst("^\\s*[/]?[*][/]?", "\t"); //$NON-NLS-1$//$NON-NLS-2$
			this.lines.add(headerLine);
		}
		this.lines.add("-->"); //$NON-NLS-1$
		
		String basename = generateXml(literals, keywords, punctuation, ignored);

		// Save
		writeFile(basename, this.lines);
	}
	
	/** Generate the XML content.
	 *
	 * @param literals the literals of the language.
	 * @param keywords the keywords of the language.
	 * @param punctuation the punctuation symbols. 
	 * @param ignored the tokens that were ignored in the MWE2 configuration.
	 * @return the basename of the file to create.
	 */
	protected abstract String generateXml(Set<String> literals, Set<String> keywords, Set<String> punctuation,
			Set<String> ignored);

}
