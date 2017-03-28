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

package io.sarl.lang.mwe2.externalspec.gnusrchighlight;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.inject.Injector;
import org.eclipse.xtext.generator.IGeneratorFragment;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.mwe2.externalspec.AbstractExternalHighlightingFragment2;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the GNU source-highlight.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class GnuSrcHighlightGenerator2 extends AbstractExternalHighlightingFragment2 {

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN = "{0}.lang"; //$NON-NLS-1$

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		setBasenameTemplate(BASENAME_PATTERN);
	}

	@Override
	public String toString() {
		return "GNU source-highlight"; //$NON-NLS-1$
	}

	private static void append(List<String> buffer, String text, Object... parameters) {
		if (parameters.length > 0) {
			buffer.add(MessageFormat.format(text, parameters));
		} else {
			buffer.add(text);
		}
	}

	private static void nl(List<String> buffer) {
		buffer.add(""); //$NON-NLS-1$
	}

	private static void comment(List<String> buffer, String text, Object... parameters) {
		final String comment;
		if (parameters.length > 0) {
			comment = MessageFormat.format(text, parameters);
		} else {
			comment = text;
		}
		buffer.add("# " + comment); //$NON-NLS-1$
	}

	@Override
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	protected void generate(Set<String> literals, Set<String> keywords, Set<String> punctuation,
			Set<String> ignored, Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		final StringBuilder punctuationPattern = new StringBuilder();
		for (final String punct : punctuation) {
			if (punctuationPattern.length() > 0) {
				punctuationPattern.append("|"); //$NON-NLS-1$
			}
			punctuationPattern.append(punct.replaceAll(Pattern.quote("\\"), //$NON-NLS-1$
					Matcher.quoteReplacement("\\\\")) //$NON-NLS-1$
					.replaceAll(Pattern.quote("|"), Matcher.quoteReplacement("\\|"))); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final StringBuilder keywordPattern = new StringBuilder();
		for (final String keyword : keywords) {
			if (keywordPattern.length() > 0) {
				keywordPattern.append("|"); //$NON-NLS-1$
			}
			keywordPattern.append(keyword);
		}

		final StringBuilder literalPattern = new StringBuilder();
		for (final String literal : literals) {
			if (literalPattern.length() > 0) {
				literalPattern.append("|"); //$NON-NLS-1$
			}
			literalPattern.append(literal);
		}

		final List<String> css = new ArrayList<>();

		final String[] header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
		for (final String headerLine : header) {
			css.add(headerLine.replaceFirst("^\\s*[/]?[*][/]?", "#")); //$NON-NLS-1$//$NON-NLS-2$
		}
		comment(css, "Style for {0} {1}", getLanguageSimpleName(), getLanguageVersion()); //$NON-NLS-1$
		nl(css);
		if (!specialKeywords.isEmpty()) {
			final StringBuilder preproc = new StringBuilder();
			for (final String specialKeyword : specialKeywords) {
				if (preproc.length() > 0) {
					preproc.append(","); //$NON-NLS-1$
				}
				preproc.append("\"" + specialKeyword + "\""); //$NON-NLS-1$ //$NON-NLS-2$
			}
			append(css, "preproc = {0}", preproc); //$NON-NLS-1$
			nl(css);
		}
		comment(css, "SARL comments"); //$NON-NLS-1$
		append(css, "include \"c_comment.lang\""); //$NON-NLS-1$
		nl(css);
		comment(css, "Numbers (integer, decimals, or hexadecimals"); //$NON-NLS-1$
		append(css, "number = '\\<[+-]?((0[xX][[:xdigit:]_]+(\\#(([bB][iI])|([lL])))?)|" //$NON-NLS-1$
				+ "(([[:digit:]][[:digit:]_]*\\.)?[[:digit:]]+([eE][+-]?[[:digit:]]+)?" //$NON-NLS-1$
				+ "(([bB][iIdD])|([lLdDfF]))?))\\>'"); //$NON-NLS-1$
		nl(css);
		comment(css, "Strings of characters"); //$NON-NLS-1$
		append(css, "include \"c_string.lang\""); //$NON-NLS-1$
		nl(css);
		comment(css, "Annotations"); //$NON-NLS-1$
		append(css, "label = '@[$[:alnum:]_.:^]+'"); //$NON-NLS-1$
		nl(css);
		comment(css, "Protected IDs with ^"); //$NON-NLS-1$
		append(css, "normal = '\\^[$[:alnum:]_]+'"); //$NON-NLS-1$
		nl(css);
		if (!typeDeclarationKeywords.isEmpty()) {
			comment(css, "Highlight the type declarations"); //$NON-NLS-1$
			final StringBuilder tdkeywords = new StringBuilder();
			for (final String typeDeclarationKeyword : typeDeclarationKeywords) {
				if (tdkeywords.length() > 0) {
					tdkeywords.append("|"); //$NON-NLS-1$
				}
				tdkeywords.append(typeDeclarationKeyword);
			}
			append(css, "(keyword,normal,classname) = `(\\<(?:{0}))([[:blank:]]+)([$[:alnum:]_]+)`", tdkeywords); //$NON-NLS-1$
			nl(css);
		}
		append(css, "keyword = \"{0}\"", keywordPattern); //$NON-NLS-1$
		nl(css);
		append(css, "keyword = \"{0}\"", literalPattern); //$NON-NLS-1$
		nl(css);
		append(css, "symbol = \"{0}\"", punctuationPattern); //$NON-NLS-1$
		nl(css);
		append(css, "cbracket = \"{|}\""); //$NON-NLS-1$
		nl(css);
		append(css, "cbracket = \"[|]\""); //$NON-NLS-1$

		final String language = getLanguageSimpleName().toLowerCase();
		final String basename = getBasename(MessageFormat.format(getBasenameTemplate(), language));
		writeFile(basename, css);
	}

}

