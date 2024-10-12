/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
import java.util.Set;
import java.util.TreeSet;

import com.google.inject.Injector;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xtext.generator.web.RegexpExtensions;

import io.sarl.lang.mwe2.externalspec.AbstractXmlHighlightingFragment2;
import io.sarl.lang.mwe2.externalspec.IXmlStyleAppendable;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the GTK source viewer.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://wiki.gnome.org/Projects/GtkSourceView"
 */
public class GtkSourceViewerGenerator2 extends AbstractXmlHighlightingFragment2 {

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN = "{0}.lang"; //$NON-NLS-1$

	private static final String XML_FORMAT_VERSION = "2.0"; //$NON-NLS-1$

	@Override
	public String toString() {
		return "GtkSourceView"; //$NON-NLS-1$
	}

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		setBasenameTemplate(BASENAME_PATTERN);
	}

	@Override
	protected void generate(IXmlStyleAppendable it, Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation, Set<String> ignored,
			Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		it.appendHeader();

		final var tag1 = it.open("language", //$NON-NLS-1$
				"id", getLanguageSimpleName().toLowerCase(), //$NON-NLS-1$
				"_name", getLanguageSimpleName(), //$NON-NLS-1$
				"version", XML_FORMAT_VERSION, //$NON-NLS-1$
				"_section", "Source"); //$NON-NLS-1$ //$NON-NLS-2$

		var tag2 = tag1.open("metadata"); //$NON-NLS-1$
		generateMetadata(tag2);
		tag2.close();

		tag2 = tag1.open("styles"); //$NON-NLS-1$
		generateStyles(tag2);
		tag2.close();

		tag2 = tag1.open("definitions"); //$NON-NLS-1$
		generateDefinitions(tag2, literals,
				sortedConcat(expressionKeywords, modifiers, primitiveTypes, typeDeclarationKeywords, specialKeywords),
				punctuation);
		tag2.close();

		tag1.close();
	}

	/** Generate the metadata section.
	 *
	 * @param it the appendable
	 */
	protected void generateMetadata(IXmlStyleAppendable it) {
		it.appendTagWithValue("property", //$NON-NLS-1$
				Strings.concat(";", getMimeTypes()), //$NON-NLS-1$
				"name", "mimetypes"); //$NON-NLS-1$ //$NON-NLS-2$

		final var buffer = new StringBuilder();
		for (final var fileExtension : getLanguage().getFileExtensions()) {
			if (buffer.length() > 0) {
				buffer.append(";"); //$NON-NLS-1$
			}
			buffer.append("*.").append(fileExtension); //$NON-NLS-1$
		}
		it.appendTagWithValue("property", //$NON-NLS-1$
				buffer.toString(),
				"name", "globs"); //$NON-NLS-1$ //$NON-NLS-2$

		it.appendTagWithValue("property", //$NON-NLS-1$
				"//", //$NON-NLS-1$
				"name", "line-comment-start"); //$NON-NLS-1$ //$NON-NLS-2$

		it.appendTagWithValue("property", //$NON-NLS-1$
				"/*", //$NON-NLS-1$
				"name", "block-comment-start"); //$NON-NLS-1$ //$NON-NLS-2$

		it.appendTagWithValue("property", //$NON-NLS-1$
				"*/", //$NON-NLS-1$
				"name", "block-comment-end"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Generate the style section.
	 *
	 * @param it the appendable
	 */
	@SuppressWarnings("static-method")
	protected void generateStyles(IXmlStyleAppendable it) {
		it.appendTag("style", //$NON-NLS-1$
				"id", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Comment", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:comment"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "error", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Error", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:error"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "escaped-character", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Escaped Character", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:special-char"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "String", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:string"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "keyword", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Keyword", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:keyword"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "literal", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Literal", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:special-constant"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "number", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Number", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:number"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "operator", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Operator", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:operator"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "identifier", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Identifier", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:text"); //$NON-NLS-1$ //$NON-NLS-2$
		it.appendTag("style", //$NON-NLS-1$
				"id", "annotation", //$NON-NLS-1$ //$NON-NLS-2$
				"_name", "Annotation", //$NON-NLS-1$ //$NON-NLS-2$
				"map-to", "def:preprocessor"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Generate the definition of the language elements.
	 *
	 * @param it the appendable
	 * @param literals the literals of the language.
	 * @param keywords the keywords of the language.
	 * @param punctuation the punctuation symbols.
	 */
	protected void generateDefinitions(IXmlStyleAppendable it, Set<String> literals, Iterable<String> keywords,
			Set<String> punctuation) {
		it.appendTagWithValue("define-regex", lines("\t", //$NON-NLS-1$ //$NON-NLS-2$
				"", //$NON-NLS-1$
				"\\\\(               # leading backslash", //$NON-NLS-1$
				"[\\\\\\\"\\'nrbtf] |   # escaped character", //$NON-NLS-1$
				"[0-9]{1,3}    |   # latin encoded char", //$NON-NLS-1$
				"u[0-9]{1,4}       # unicode char", //$NON-NLS-1$
				")"), //$NON-NLS-1$
				"id", "escaped-character", //$NON-NLS-1$ //$NON-NLS-2$
				"extended", "true"); //$NON-NLS-1$ //$NON-NLS-2$

		it.appendTagWithValue("define-regex", //$NON-NLS-1$
				"0[xX][0-9a-fA-F_]+(#(([bB][iI])|([lL])))?", //$NON-NLS-1$
				"id", "hex-number"); //$NON-NLS-1$ //$NON-NLS-2$

		it.appendTagWithValue("define-regex", //$NON-NLS-1$
				"[0-9][0-9_]*", //$NON-NLS-1$
				"id", "int-number"); //$NON-NLS-1$ //$NON-NLS-2$

		it.appendTagWithValue("define-regex", //$NON-NLS-1$
				"\\%{int-number}([eE][+\\-]?\\%{int-number})?(([bB][iIdD])|([lLdDfF]))?", //$NON-NLS-1$
				"id", "decimal-number"); //$NON-NLS-1$ //$NON-NLS-2$

		it.appendTagWithValue("define-regex", //$NON-NLS-1$
				"(\\%{hex-number})|(\\%{decimal-number}(\\.\\%{decimal-number})?)", //$NON-NLS-1$
				"id", "sarl-number"); //$NON-NLS-1$ //$NON-NLS-2$

		it.appendTagWithValue("define-regex", //$NON-NLS-1$
				"\\^?[a-zA-Z_$][a-zA-Z_$0-9]*", //$NON-NLS-1$
				"id", "sarl-identifier"); //$NON-NLS-1$ //$NON-NLS-2$

		final var tag1 = it.open("context", //$NON-NLS-1$
				"id", "qq-string", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"end-at-line-end", "true", //$NON-NLS-1$ //$NON-NLS-2$
				"class", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"class-disabled", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		tag1.appendTagWithValue("start", "\""); //$NON-NLS-1$ //$NON-NLS-2$
		tag1.appendTagWithValue("end", "\""); //$NON-NLS-1$ //$NON-NLS-2$
		final var tag2 = tag1.open("include"); //$NON-NLS-1$
		final var tag3 = tag2.open("context", //$NON-NLS-1$
				"id", "qq-string-escaped-character", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "escaped-character"); //$NON-NLS-1$ //$NON-NLS-2$
		tag3.appendTagWithValue("match", "\\%{escaped-character}"); //$NON-NLS-1$ //$NON-NLS-2$
		tag3.close();
		tag2.close();
		tag1.close();

		final var tag4 = it.open("context", //$NON-NLS-1$
				"id", "q-string", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"end-at-line-end", "true", //$NON-NLS-1$ //$NON-NLS-2$
				"class", "string", //$NON-NLS-1$ //$NON-NLS-2$
				"class-disabled", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		tag4.appendTagWithValue("start", "'"); //$NON-NLS-1$ //$NON-NLS-2$
		tag4.appendTagWithValue("end", "'"); //$NON-NLS-1$ //$NON-NLS-2$
		final var tag5 = tag4.open("include"); //$NON-NLS-1$
		final var tag6 = tag5.open("context", //$NON-NLS-1$
				"id", "q-string-escaped-character", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "escaped-character"); //$NON-NLS-1$ //$NON-NLS-2$
		tag6.appendTagWithValue("match", "\\%{escaped-character}"); //$NON-NLS-1$ //$NON-NLS-2$
		tag6.close();
		tag5.close();
		tag4.close();

		final var tag7 = it.open("context", //$NON-NLS-1$
				"id", "line-comment", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"end-at-line-end", "true", //$NON-NLS-1$ //$NON-NLS-2$
				"class", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"class-disabled", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		tag7.appendTagWithValue("start", "//"); //$NON-NLS-1$ //$NON-NLS-2$
		final var tag8 = tag7.open("include"); //$NON-NLS-1$
		tag8.appendTag("context", //$NON-NLS-1$
				"ref", "def:in-line-comment"); //$NON-NLS-1$ //$NON-NLS-2$
		tag8.close();
		tag7.close();

		final var tag9 = it.open("context", //$NON-NLS-1$
				"id", "block-comment", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"class", "comment", //$NON-NLS-1$ //$NON-NLS-2$
				"class-disabled", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		tag9.appendTagWithValue("start", "/\\*"); //$NON-NLS-1$ //$NON-NLS-2$
		tag9.appendTagWithValue("end", "\\*/"); //$NON-NLS-1$ //$NON-NLS-2$
		final var tag10 = tag9.open("include"); //$NON-NLS-1$
		tag10.appendTag("context", //$NON-NLS-1$
				"ref", "def:in-comment"); //$NON-NLS-1$ //$NON-NLS-2$
		tag10.close();
		tag9.close();

		final var tag11 = it.open("context", //$NON-NLS-1$
				"id", "close-comment-outside-comment", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "error"); //$NON-NLS-1$ //$NON-NLS-2$
		tag11.appendTagWithValue("match", "\\*/(?!\\*)"); //$NON-NLS-1$ //$NON-NLS-2$
		tag11.close();

		final var tag12 = it.open("context", //$NON-NLS-1$
				"id", "sarl-keywords", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "keyword"); //$NON-NLS-1$ //$NON-NLS-2$
		for (final String keyword : keywords) {
			tag12.appendTagWithValue("keyword", keyword); //$NON-NLS-1$
		}
		tag12.close();

		final var tag13 = it.open("context", //$NON-NLS-1$
				"id", "sarl-literals", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "literal"); //$NON-NLS-1$ //$NON-NLS-2$
		for (final String literal : literals) {
			tag13.appendTagWithValue("keyword", literal); //$NON-NLS-1$
		}
		tag13.close();

		final var tag14 = it.open("context", //$NON-NLS-1$
				"id", "annotations", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "annotation"); //$NON-NLS-1$ //$NON-NLS-2$
		tag14.appendTagWithValue("match", "\\@\\%{sarl-identifier}(\\.\\%{sarl-identifier})*"); //$NON-NLS-1$ //$NON-NLS-2$
		tag14.close();

		final var tag15 = it.open("context", //$NON-NLS-1$
				"id", "identifiers", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "identifier"); //$NON-NLS-1$ //$NON-NLS-2$
		tag15.appendTagWithValue("match", "\\%{sarl-identifier}"); //$NON-NLS-1$ //$NON-NLS-2$
		tag15.close();

		final var tag16 = it.open("context", //$NON-NLS-1$
				"id", "numeric", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "number"); //$NON-NLS-1$ //$NON-NLS-2$
		tag16.appendTagWithValue("match", "\\%{sarl-number}"); //$NON-NLS-1$ //$NON-NLS-2$
		tag16.close();

		final var tag17 = it.open("context", //$NON-NLS-1$
				"id", "operators", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "operator", //$NON-NLS-1$ //$NON-NLS-2$
				"extend-parent", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		var buffer = new StringBuilder();
		final var characters = new TreeSet<Character>();
		for (final var operator : punctuation) {
			if (buffer.length() > 0) {
				buffer.append("|"); //$NON-NLS-1$
			}
			buffer.append("("); //$NON-NLS-1$
			buffer.append(RegexpExtensions.toRegexpString(operator, false));
			buffer.append(")"); //$NON-NLS-1$
			operator.chars().forEach(c -> characters.add(Character.valueOf((char) c)));
		}
		tag17.appendTagWithValue("match", buffer.toString()); //$NON-NLS-1$
		tag17.close();

		final var tag18 = it.open("context", //$NON-NLS-1$
				"id", "invalid-operators", //$NON-NLS-1$ //$NON-NLS-2$
				"style-ref", "error"); //$NON-NLS-1$ //$NON-NLS-2$
		buffer = new StringBuilder();
		for (final var character : characters) {
			buffer.append(RegexpExtensions.toRegexpString(character.toString(), false));
		}
		tag18.appendTagWithValue("match", "[" + buffer.toString() + "]+"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag18.close();

		final var tag19 = it.open("context", //$NON-NLS-1$
				"id", getLanguageSimpleName().toLowerCase(), //$NON-NLS-1$
				"class", "no-spell-check"); //$NON-NLS-1$ //$NON-NLS-2$
		final var tag20 = tag19.open("include"); //$NON-NLS-1$
		tag20.appendTag("context", "ref", "qq-string"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "q-string"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "line-comment"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "block-comment"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "close-comment-outside-comment"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "sarl-keywords"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "sarl-literals"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "annotations"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "identifiers"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "numeric"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.appendTag("context", "ref", "operators"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		tag20.close();
		tag19.close();
	}

	@Override
	protected Object getReadmeFileContent(String basename) {
		return concat(
				"1. MANUAL INSTALLATION", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"Copy the " + basename + " file into one of the folders:", //$NON-NLS-1$ //$NON-NLS-2$
				"* $HOME/.local/share/gtksourceview-3.0/language-specs/", //$NON-NLS-1$
				"* $HOME/.local/share/gtksourceview-2.0/language-specs/"); //$NON-NLS-1$
	}

}

