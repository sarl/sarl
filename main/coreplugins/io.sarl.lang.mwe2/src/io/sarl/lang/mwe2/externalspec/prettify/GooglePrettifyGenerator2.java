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

package io.sarl.lang.mwe2.externalspec.prettify;

import java.text.MessageFormat;
import java.util.Set;
import java.util.TreeSet;

import com.google.inject.Injector;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.mwe2.externalspec.AbstractCsyntaxHighlightingFragment2;
import io.sarl.lang.mwe2.externalspec.IStyleAppendable;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the Google prettify library.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/google/code-prettify"
 */
public class GooglePrettifyGenerator2 extends AbstractCsyntaxHighlightingFragment2 {

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN = "lang-{0}.js"; //$NON-NLS-1$

	private String whitespaces = "\\t\\n\\r \\xA0"; //$NON-NLS-1$

	private String doubleQuotedStrings = "^(?:\"(?:[^\\\"\\\\]|\\\\.)*\"|'(?!\\'\\')(?:[^\\'\\\\]|\\\\.)*')"; //$NON-NLS-1$

	private String singleQuotedStrings = "^'(?:[^\\r\\n\\\\']|\\\\(?:'|[^\\r\\n']+))'"; //$NON-NLS-1$

	private String characterLiteral = "^'[a-zA-Z_$][\\w$]*(?!['$\\w])"; //$NON-NLS-1$

	@SuppressWarnings("checkstyle:linelength")
	private String numberLiteral = "^(?:(?:0(?:[0-7]+|X[0-9A-F]+))L?|(?:(?:0|[1-9][0-9]*)(?:(?:\\.[0-9]+)?(?:E[+\\-]?[0-9]+)?F?|L?))|\\\\.[0-9]+(?:E[+\\-]?[0-9]+)?F?)"; //$NON-NLS-1$

	private String typePattern = "^[$_]*[A-Z][_$A-Z0-9]*[a-z][\\w$]*"; //$NON-NLS-1$

	private String plainTextPattern = "^[$a-zA-Z_][\\w$]*"; //$NON-NLS-1$

	private String commentPattern = "^\\/(?:\\/.*|\\*(?:\\/|\\**[^*/])*(?:\\*+\\/?)?)"; //$NON-NLS-1$

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		setBasenameTemplate(BASENAME_PATTERN);
	}

	@Override
	public String toString() {
		return "Google Prettify"; //$NON-NLS-1$
	}

	/** Set the pattern that match a white space.
	 *
	 * @param pattern the pattern.
	 */
	public void setWhitespacePattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.whitespaces = pattern;
		}
	}

	/** Set the pattern that match a double-quoted string.
	 *
	 * @param pattern the pattern.
	 */
	public void setDoubleQuotedStringPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.doubleQuotedStrings = pattern;
		}
	}

	/** Set the pattern that match a single-quoted string.
	 *
	 * @param pattern the pattern.
	 */
	public void setSingleQuotedStringPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.singleQuotedStrings = pattern;
		}
	}

	/** Set the pattern that match a character literal.
	 *
	 * @param pattern the pattern.
	 */
	public void setCharacterLiteralPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.characterLiteral = pattern;
		}
	}

	/** Set the pattern that match a number literal.
	 *
	 * @param pattern the pattern.
	 */
	public void setNumberLiteralPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.numberLiteral = pattern;
		}
	}

	/** Set the pattern that match a type.
	 *
	 * @param pattern the pattern.
	 */
	public void setTypePattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.typePattern = pattern;
		}
	}

	/** Set the pattern that match a plain text.
	 *
	 * @param pattern the pattern.
	 */
	public void setPlainTextPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.plainTextPattern = pattern;
		}
	}

	/** Set the pattern that match a comment.
	 *
	 * @param pattern the pattern.
	 */
	public void setCommentPattern(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.commentPattern = pattern;
		}
	}

	@SuppressWarnings({"checkstyle:parameternumber", "checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	@Override
	protected void generate(IStyleAppendable it, Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation, Set<String> ignored,
			Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		final Set<Character> characters = new TreeSet<>();
		for (final String punct : punctuation) {
			punct.chars().forEach((int candidate) -> characters.add(Character.valueOf((char) candidate)));
		}
		final StringBuilder punctuationCharacters1 = new StringBuilder();
		final StringBuilder punctuationCharacters2 = new StringBuilder();
		for (final Character candidate : characters) {
			switch (candidate) {
			case '-':
			case '\\':
			case '[':
			case ']':
				punctuationCharacters1.append("\\").append(candidate.toString()); //$NON-NLS-1$
				break;
			case '^':
				if (punctuationCharacters1.length() > 0) {
					punctuationCharacters1.append(candidate.toString());
				} else {
					punctuationCharacters1.append("\\").append(candidate.toString()); //$NON-NLS-1$
				}
				break;
			default:
				punctuationCharacters1.append(candidate.toString());
			}
			switch (candidate) {
			case '\\':
			case '\'':
				punctuationCharacters2.append("\\").append(candidate.toString()); //$NON-NLS-1$
				break;
			default:
				punctuationCharacters2.append(candidate.toString());
			}
		}

		final StringBuilder keywordPattern = new StringBuilder();
		for (final String keyword : sortedConcat(expressionKeywords, modifiers, primitiveTypes,
				specialKeywords, typeDeclarationKeywords)) {
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

		it.appendHeader();

		it.appendNl("PR['registerLangHandler']("); //$NON-NLS-1$
		it.appendNl("   PR['createSimpleLexer']("); //$NON-NLS-1$
		it.appendNl("      ["); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_PLAIN''], /^[{0}]+/, null, ''{0}''],", this.whitespaces); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_PUNCTUATION''], /^[{0}]+/, null, ''{1}''],", punctuationCharacters1, //$NON-NLS-1$
				punctuationCharacters2);
		it.appendNl("      ],"); //$NON-NLS-1$
		it.appendNl("      ["); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_STRING''], /{0}/],", this.doubleQuotedStrings); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_STRING''], /{0}/],", this.singleQuotedStrings); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_LITERAL''], /{0}/],", this.characterLiteral); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_KEYWORD''], /^(?:{0})\\b/],", keywordPattern); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_LITERAL''], /^(?:{0})\\b/],", literalPattern); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_LITERAL''], /{0}/i],", this.numberLiteral); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_TYPE''], /{0}/],", this.typePattern); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_PLAIN''], /{0}/],", this.plainTextPattern); //$NON-NLS-1$
		it.appendNl("         [PR[''PR_COMMENT''], /{0}/],", this.commentPattern); //$NON-NLS-1$
		it.appendNl("         [PR['PR_PUNCTUATION'], /^(?:\\.+|\\/)/]"); //$NON-NLS-1$
		it.appendNl("      ]),"); //$NON-NLS-1$
		final String language = getLanguageSimpleName().toLowerCase();
		it.appendNl("   [''{0}'']);", language); //$NON-NLS-1$
	}

	@Override
	protected Object getReadmeFileContent(String basename) {
		return null;
	}

}

