/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.inject.Injector;

import io.sarl.lang.mwe2.externalspec.AbstractScriptHighlightingFragment2;
import io.sarl.lang.mwe2.externalspec.IStyleAppendable;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the GNU source-highlight.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 * @see "https://www.gnu.org/software/src-highlite/"
 */
public class GnuSrcHighlightGenerator2 extends AbstractScriptHighlightingFragment2 {

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

	@Override
	protected void generate(IStyleAppendable it, Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation, Set<String> ignored,
			Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		final var punctuationPattern = new StringBuilder();
		for (final var punct : punctuation) {
			if (punctuationPattern.length() > 0) {
				punctuationPattern.append("|"); //$NON-NLS-1$
			}
			punctuationPattern.append(punct.replaceAll(Pattern.quote("\\"), //$NON-NLS-1$
					Matcher.quoteReplacement("\\\\")) //$NON-NLS-1$
					.replaceAll(Pattern.quote("|"), Matcher.quoteReplacement("\\|"))); //$NON-NLS-1$ //$NON-NLS-2$
		}

		final var keywordPattern = new StringBuilder();
		for (final var keyword : sortedConcat(expressionKeywords, modifiers, primitiveTypes, typeDeclarationKeywords,
				specialKeywords)) {
			if (keywordPattern.length() > 0) {
				keywordPattern.append("|"); //$NON-NLS-1$
			}
			keywordPattern.append(keyword);
		}

		final var literalPattern = new StringBuilder();
		for (final var literal : literals) {
			if (literalPattern.length() > 0) {
				literalPattern.append("|"); //$NON-NLS-1$
			}
			literalPattern.append(literal);
		}

		it.appendHeader();

		if (!specialKeywords.isEmpty()) {
			final var preproc = new StringBuilder();
			for (final var specialKeyword : specialKeywords) {
				if (preproc.length() > 0) {
					preproc.append(","); //$NON-NLS-1$
				}
				preproc.append("\"" + specialKeyword + "\""); //$NON-NLS-1$ //$NON-NLS-2$
			}
			it.appendNl("preproc = {0}", preproc); //$NON-NLS-1$
			it.newLine();
		}
		it.appendComment("SARL comments"); //$NON-NLS-1$
		it.appendNl("include \"c_comment.lang\""); //$NON-NLS-1$
		it.newLine();
		it.appendComment("Numbers (integer, decimals, or hexadecimals"); //$NON-NLS-1$
		it.appendNl("number = '\\<[+-]?((0[xX][[:xdigit:]_]+(\\#(([bB][iI])|([lL])))?)|" //$NON-NLS-1$
				+ "(([[:digit:]][[:digit:]_]*\\.)?[[:digit:]]+([eE][+-]?[[:digit:]]+)?" //$NON-NLS-1$
				+ "(([bB][iIdD])|([lLdDfF]))?))\\>'"); //$NON-NLS-1$
		it.newLine();
		it.appendComment("Strings of characters"); //$NON-NLS-1$
		it.appendNl("include \"c_string.lang\""); //$NON-NLS-1$
		it.newLine();
		it.appendComment("Annotations"); //$NON-NLS-1$
		it.appendNl("label = '@[$[:alnum:]_.:^]+'"); //$NON-NLS-1$
		it.newLine();
		it.appendComment("Protected IDs with ^"); //$NON-NLS-1$
		it.appendNl("normal = '\\^[$[:alnum:]_]+'"); //$NON-NLS-1$
		it.newLine();
		if (!typeDeclarationKeywords.isEmpty()) {
			it.appendComment("Highlight the type declarations"); //$NON-NLS-1$
			final var tdkeywords = new StringBuilder();
			for (final var typeDeclarationKeyword : typeDeclarationKeywords) {
				if (tdkeywords.length() > 0) {
					tdkeywords.append("|"); //$NON-NLS-1$
				}
				tdkeywords.append(typeDeclarationKeyword);
			}
			it.appendNl("(keyword,normal,classname) = `(\\<(?:{0}))([[:blank:]]+)([$[:alnum:]_]+)`", tdkeywords); //$NON-NLS-1$
			it.newLine();
		}
		it.appendNl("keyword = \"{0}\"", keywordPattern); //$NON-NLS-1$
		it.newLine();
		it.appendNl("keyword = \"{0}\"", literalPattern); //$NON-NLS-1$
		it.newLine();
		it.appendNl("symbol = \"{0}\"", punctuationPattern); //$NON-NLS-1$
		it.newLine();
		it.appendNl("cbracket = \"{|}\""); //$NON-NLS-1$
		it.newLine();
		it.appendNl("cbracket = \"[|]\""); //$NON-NLS-1$
	}

	@Override
	protected Object getReadmeFileContent(String basename) {
		return null;
	}

}

