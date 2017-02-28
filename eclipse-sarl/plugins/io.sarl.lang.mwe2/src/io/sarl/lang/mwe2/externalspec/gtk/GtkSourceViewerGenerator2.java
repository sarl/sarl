/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.mwe2.externalspec.gtk;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.google.inject.Injector;
import org.eclipse.xtext.generator.IGeneratorFragment;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.web.RegexpExtensions;

import io.sarl.lang.mwe2.externalspec.AbstractXmlHighlightingFragment2;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the GTK source viewer.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class GtkSourceViewerGenerator2 extends AbstractXmlHighlightingFragment2 {

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN = "{0}.lang"; //$NON-NLS-1$

	private static final String XML_FORMAT_VERSION = "2.0"; //$NON-NLS-1$

	private final List<String> mimeTypes = new ArrayList<>();

	@Override
	public String toString() {
		return "GtkSourceView"; //$NON-NLS-1$
	}

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		setBasenameTemplate(BASENAME_PATTERN);
	}

	/** Add a mime type for the SARL source code.
	 *
	 * @param mimeType the mime type of SARL.
	 */
	public void addMimeType(String mimeType) {
		if (!Strings.isEmpty(mimeType)) {
			for (final String mtype : mimeType.split("[:;,]")) { //$NON-NLS-1$
				this.mimeTypes.add(mtype);
			}
		}
	}

	/** Replies the mime types for the SARL source code.
	 *
	 * @return the mime type for SARL.
	 */
	@Pure
	public List<String> getMimeTypes() {
		if (this.mimeTypes.isEmpty()) {
			return Arrays.asList("text/x-" + getLanguageSimpleName().toLowerCase()); //$NON-NLS-1$
		}
		return this.mimeTypes;
	}

	/** Generate the metadata section.
	 */
	protected void generateMetadata() {
		valuedTag("property", //$NON-NLS-1$
				Strings.concat(";", getMimeTypes()), //$NON-NLS-1$
				"name", "mimetypes"); //$NON-NLS-1$ //$NON-NLS-2$

		final StringBuilder buffer = new StringBuilder();
		for (final String fileExtension : getLanguage().getFileExtensions()) {
			if (buffer.length() > 0) {
				buffer.append(";"); //$NON-NLS-1$
			}
			buffer.append("*.").append(fileExtension); //$NON-NLS-1$
		}
		valuedTag("property", //$NON-NLS-1$
				buffer.toString(),
				"name", "globs"); //$NON-NLS-1$ //$NON-NLS-2$

		valuedTag("property", //$NON-NLS-1$
				"//", //$NON-NLS-1$
				"name", "line-comment-start"); //$NON-NLS-1$ //$NON-NLS-2$

		valuedTag("property", //$NON-NLS-1$
				"/*", //$NON-NLS-1$
				"name", "block-comment-start"); //$NON-NLS-1$ //$NON-NLS-2$

		valuedTag("property", //$NON-NLS-1$
				"*/", //$NON-NLS-1$
				"name", "block-comment-end"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Generate the style section.
	 */
	protected void generateStyles() {
		tag("style", //$NON-NLS-1$
				"id", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Comment", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:comment"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "error", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Error", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:error"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "escaped-character", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Escaped Character", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:special-char"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "String", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:string"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "keyword", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Keyword", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:keyword"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "literal", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Literal", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:special-constant"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "number", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Number", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:number"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "operator", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Operator", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:operator"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "identifier", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Identifier", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:text"); //$NON-NLS-1$ //$NON-NLS-2$
		tag("style", //$NON-NLS-1$
				"id", "annotation", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Annotation", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:preprocessor"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Generate the definition of the language elements.
	 *
	 * @param literals the literals of the language.
	 * @param keywords the keywords of the language.
	 * @param punctuation the punctuation symbols.
	 */
	protected void generateDefinitions(Set<String> literals, Set<String> keywords, Set<String> punctuation) {
		valuedTag("define-regex", lines("\t", //$NON-NLS-1$ //$NON-NLS-2$
				"", //$NON-NLS-1$
				"\\\\(               # leading backslash", //$NON-NLS-1$
				"[\\\\\\\"\\'nrbtf] |   # escaped character", //$NON-NLS-1$
				"[0-9]{1,3}    |   # latin encoded char", //$NON-NLS-1$
				"u[0-9]{1,4}       # unicode char", //$NON-NLS-1$
				")"), //$NON-NLS-1$
				"id", "escaped-character", //$NON-NLS-1$ //$NON-NLS-2$
				"extended", "true"); //$NON-NLS-1$ //$NON-NLS-2$

		valuedTag("define-regex", //$NON-NLS-1$
				"0[xX][0-9a-fA-F_]+(#(([bB][iI])|([lL])))?", //$NON-NLS-1$
				"id", "hex-number"); //$NON-NLS-1$ //$NON-NLS-2$

		valuedTag("define-regex", //$NON-NLS-1$
				"[0-9][0-9_]*", //$NON-NLS-1$
				"id", "int-number"); //$NON-NLS-1$ //$NON-NLS-2$

		valuedTag("define-regex", //$NON-NLS-1$
				"\\%{int-number}([eE][+\\-]?\\%{int-number})?(([bB][iIdD])|([lLdDfF]))?", //$NON-NLS-1$
				"id", "decimal-number"); //$NON-NLS-1$ //$NON-NLS-2$

		valuedTag("define-regex", //$NON-NLS-1$
				"(\\%{hex-number})|(\\%{decimal-number}(\\.\\%{decimal-number})?)", //$NON-NLS-1$
				"id", "sarl-number"); //$NON-NLS-1$ //$NON-NLS-2$

		valuedTag("define-regex", //$NON-NLS-1$
				"\\^?[a-zA-Z_$][a-zA-Z_$0-9]*", //$NON-NLS-1$
				"id", "sarl-identifier"); //$NON-NLS-1$ //$NON-NLS-2$

		open("context", //$NON-NLS-1$
				"id", "qq-string", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"end-at-line-end", "true", //$NON-NLS-1$ //$NON-NLS-2$
				"class", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"class-disabled", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("start", "\""); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("end", "\""); //$NON-NLS-1$ //$NON-NLS-2$
		open("include"); //$NON-NLS-1$
		open("context", //$NON-NLS-1$
				"id", "qq-string-escaped-character", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "escaped-character"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("match", "\\%{escaped-character}"); //$NON-NLS-1$ //$NON-NLS-2$
		close();
		close();
		close();

		open("context", //$NON-NLS-1$
				"id", "q-string", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"end-at-line-end", "true", //$NON-NLS-1$ //$NON-NLS-2$
				"class", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"class-disabled", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("start", "'"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("end", "'"); //$NON-NLS-1$ //$NON-NLS-2$
		open("include"); //$NON-NLS-1$
		open("context", //$NON-NLS-1$
				"id", "q-string-escaped-character", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "escaped-character"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("match", "\\%{escaped-character}"); //$NON-NLS-1$ //$NON-NLS-2$
		close();
		close();
		close();

		open("context", //$NON-NLS-1$
				"id", "line-comment", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"end-at-line-end", "true", //$NON-NLS-1$ //$NON-NLS-2$
				"class", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"class-disabled", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("start", "//"); //$NON-NLS-1$ //$NON-NLS-2$
		open("include"); //$NON-NLS-1$
		tag("context", //$NON-NLS-1$
				"ref", "def:in-line-comment"); //$NON-NLS-1$ //$NON-NLS-2$
		close();
		close();

		open("context", //$NON-NLS-1$
				"id", "block-comment", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"class", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"class-disabled", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("start", "/\\*"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("end", "\\*/"); //$NON-NLS-1$ //$NON-NLS-2$
		open("include"); //$NON-NLS-1$
		tag("context", //$NON-NLS-1$
				"ref", "def:in-comment"); //$NON-NLS-1$ //$NON-NLS-2$
		close();
		close();

		open("context", //$NON-NLS-1$
				"id", "close-comment-outside-comment", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "error"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("match", "\\*/(?!\\*)"); //$NON-NLS-1$ //$NON-NLS-2$
		close();

		open("context", //$NON-NLS-1$
				"id", "sarl-keywords", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "keyword"); //$NON-NLS-1$ //$NON-NLS-2$
		for (final String keyword : keywords) {
			valuedTag("keyword", keyword); //$NON-NLS-1$
		}
		close();

		open("context", //$NON-NLS-1$
				"id", "sarl-literals", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "literal"); //$NON-NLS-1$ //$NON-NLS-2$
		for (final String literal : literals) {
			valuedTag("keyword", literal); //$NON-NLS-1$
		}
		close();

		open("context", //$NON-NLS-1$
				"id", "annotations", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "annotation"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("match", "\\@\\%{sarl-identifier}(\\.\\%{sarl-identifier})*"); //$NON-NLS-1$ //$NON-NLS-2$
		close();

		open("context", //$NON-NLS-1$
				"id", "identifiers", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "identifier"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("match", "\\%{sarl-identifier}"); //$NON-NLS-1$ //$NON-NLS-2$
		close();

		open("context", //$NON-NLS-1$
				"id", "numeric", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "number"); //$NON-NLS-1$ //$NON-NLS-2$
		valuedTag("match", "\\%{sarl-number}"); //$NON-NLS-1$ //$NON-NLS-2$
		close();

		open("context", //$NON-NLS-1$
				"id", "operators", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "operator", //$NON-NLS-1$ //$NON-NLS-2$
				"extend-parent", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		StringBuilder buffer = new StringBuilder();
		final Set<Character> characters = new TreeSet<>();
		for (final String operator : punctuation) {
			if (buffer.length() > 0) {
				buffer.append("|"); //$NON-NLS-1$
			}
			buffer.append("("); //$NON-NLS-1$
			buffer.append(RegexpExtensions.toRegexpString(operator, false));
			buffer.append(")"); //$NON-NLS-1$
			operator.chars().forEach(c -> characters.add((char) c));
		}
		valuedTag("match", buffer.toString()); //$NON-NLS-1$
		close();

		open("context", //$NON-NLS-1$
				"id", "invalid-operators", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "error"); //$NON-NLS-1$ //$NON-NLS-2$
		buffer = new StringBuilder();
		for (final Character character : characters) {
			buffer.append(RegexpExtensions.toRegexpString(character.toString(), false));
		}
		valuedTag("match", "[" + buffer.toString() + "]+"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		close();

		open("context", //$NON-NLS-1$
				"id", getLanguageSimpleName().toLowerCase(), //$NON-NLS-1$
				"class", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		open("include"); //$NON-NLS-1$
		tag("context", "ref", "qq-string"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "q-string"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "line-comment"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "block-comment"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "close-comment-outside-comment"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "sarl-keywords"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "sarl-literals"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "annotations"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "identifiers"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "numeric"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag("context", "ref", "operators"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		close();
		close();
	}

	@Override
	protected String generateXml(Set<String> literals, Set<String> keywords, Set<String> punctuation,
			Set<String> ignored, Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		open("language", //$NON-NLS-1$
				"id", getLanguageSimpleName().toLowerCase(), //$NON-NLS-1$
				"_name", getLanguageSimpleName(), //$NON-NLS-1$
				"version", XML_FORMAT_VERSION, //$NON-NLS-1$
				"_section", "Sources"); //$NON-NLS-1$ //$NON-NLS-2$

		open("metadata"); //$NON-NLS-1$
		generateMetadata();
		close();

		open("styles"); //$NON-NLS-1$
		generateStyles();
		close();

		open("definitions"); //$NON-NLS-1$
		generateDefinitions(literals, keywords, punctuation);
		close();

		close();

		return getBasename(MessageFormat.format(getBasenameTemplate(), getLanguageSimpleName().toLowerCase()));
	}

}

