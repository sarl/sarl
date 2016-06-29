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

package io.sarl.lang.mwe2.externalspec.gnusrchighlite;

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
 * the GNU src-highlite.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class GnuSrcHighliteGenerator2 extends AbstractExternalHighlightingFragment2 {

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
		return "GNU src-highlite"; //$NON-NLS-1$
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

	@Override
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	protected void generate(Set<String> literals, Set<String> keywords, Set<String> punctuation, Set<String> ignored) {
		StringBuilder punctuationPattern = new StringBuilder();
		for (String punct : punctuation) {
			if (punctuationPattern.length() > 0) {
				punctuationPattern.append("|"); //$NON-NLS-1$
			}
			punctuationPattern.append(punct.replaceAll(Pattern.quote("\\"), //$NON-NLS-1$
					Matcher.quoteReplacement("\\\\")) //$NON-NLS-1$
					.replaceAll(Pattern.quote("|"), Matcher.quoteReplacement("\\|"))); //$NON-NLS-1$ //$NON-NLS-2$
		}

		StringBuilder keywordPattern = new StringBuilder();
		for (String keyword : keywords) {
			if (keywordPattern.length() > 0) {
				keywordPattern.append("|"); //$NON-NLS-1$
			}
			keywordPattern.append(keyword);
		}

		StringBuilder literalPattern = new StringBuilder();
		for (String literal : literals) {
			if (literalPattern.length() > 0) {
				literalPattern.append("|"); //$NON-NLS-1$
			}
			literalPattern.append(literal);
		}

		List<String> css = new ArrayList<>();

		String[] header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
		for (String headerLine : header) {
			headerLine = headerLine.replaceFirst("^\\s*[/]?[*][/]?", "#"); //$NON-NLS-1$//$NON-NLS-2$
			css.add(headerLine);
		}

		append(css, "include \"c_comment.lang\""); //$NON-NLS-1$
		nl(css);
		append(css, "include \"number.lang\""); //$NON-NLS-1$
		nl(css);
		append(css, "string delim \"\\\"\" \"\\\"\" escape \"\\\\\""); //$NON-NLS-1$
		append(css, "string delim \"'\" \"'\"  escape \"\\\\\""); //$NON-NLS-1$
		append(css, "regexp = '/[^\n]*/'"); //$NON-NLS-1$
		append(css, "(symbol,regexp) = `(%r)(\\{(?:\\\\\\}|#\\{[[:alnum:]]+\\}|[^}])*\\})`"); //$NON-NLS-1$
		nl(css);
		append(css, "keyword = \"{0}\"", keywordPattern); //$NON-NLS-1$
		nl(css);
		append(css, "keyword = \"{0}\"", punctuationPattern); //$NON-NLS-1$
		nl(css);
		append(css, "keyword = \"{0}\"", literalPattern); //$NON-NLS-1$
		nl(css);
		append(css, "include \"symbols.lang\""); //$NON-NLS-1$
		nl(css);
		append(css, "cbracket = \"{|}\""); //$NON-NLS-1$

		String language = getLanguageSimpleName().toLowerCase();
		String basename = getBasename(
				MessageFormat.format(getBasenameTemplate(), language));
		writeFile(basename, css);
	}

}

