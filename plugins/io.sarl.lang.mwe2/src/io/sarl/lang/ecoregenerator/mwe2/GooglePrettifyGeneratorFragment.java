/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.ecoregenerator.mwe2;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xpand2.XpandExecutionContext;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.common.types.access.impl.Primitives;
import org.eclipse.xtext.generator.DefaultGeneratorFragment;
import org.eclipse.xtext.generator.IGeneratorFragment;
import org.eclipse.xtext.util.Strings;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the Google prettify library.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
public class GooglePrettifyGeneratorFragment extends DefaultGeneratorFragment {

	private static final Logger LOG = Logger.getLogger(GooglePrettifyGeneratorFragment.class);

	private static final Pattern KEYWORD_PATTERN = Pattern.compile("^[a-zA-Z]{2,}$"); //$NON-NLS-1$

	@SuppressWarnings("checkstyle:linelength")
	private static final Pattern PUNCTUATION_PATTERN = Pattern.compile("^[!#%&()*/+,\\-:;<=>?@\\[\\\\\\]^{|}~.%]+$"); //$NON-NLS-1$

	private final Set<String> outputDirectories = new TreeSet<>();

	private final Set<String> additionalLiterals = new TreeSet<>();

	private final Set<String> additionalKeywords = new TreeSet<>();

	private final Set<String> excludedKeywords = new TreeSet<>();

	private final Set<String> additionalPunctuation = new TreeSet<>();

	private boolean addNativeTypes = true;

	private String language = "sarl"; //$NON-NLS-1$

	private String whitespaces = "\\t\\n\\r \\xA0"; //$NON-NLS-1$

	private String doubleQuotedStrings = "^(?:\"(?:[^\\\"\\\\]|\\\\.)*\"|'(?!\\'\\')(?:[^\\'\\\\]|\\\\.)*')"; //$NON-NLS-1$

	private String singleQuotedStrings = "^'(?:[^\\r\\n\\\\']|\\\\(?:'|[^\\r\\n']+))'"; //$NON-NLS-1$

	private String characterLiteral = "^'[a-zA-Z_$][\\w$]*(?!['$\\w])"; //$NON-NLS-1$

	@SuppressWarnings("checkstyle:linelength")
	private String numberLiteral = "^(?:(?:0(?:[0-7]+|X[0-9A-F]+))L?|(?:(?:0|[1-9][0-9]*)(?:(?:\\.[0-9]+)?(?:E[+\\-]?[0-9]+)?F?|L?))|\\\\.[0-9]+(?:E[+\\-]?[0-9]+)?F?)"; //$NON-NLS-1$

	private String typePattern = "^[$_]*[A-Z][_$A-Z0-9]*[a-z][\\w$]*"; //$NON-NLS-1$

	private String plainTextPattern = "^[$a-zA-Z_][\\w$]*"; //$NON-NLS-1$

	private String commentPattern = "^\\/(?:\\/.*|\\*(?:\\/|\\**[^*/])*(?:\\*+\\/?)?)"; //$NON-NLS-1$

	/** Set the name of the language.
	 *
	 * @param name the language name.
	 */
	public void setLanguage(String name) {
		if (!Strings.isEmpty(name)) {
			this.language = name;
		}
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

	/** Add a directory into which the Groogle Prettify must be written.
	 *
	 * @param directory the output directory.
	 */
	public void addOutput(String directory) {
		if (!Strings.isEmpty(directory)) {
			this.outputDirectories.add(directory);
		}
	}

	/** Add a literal into which the Groogle Prettify CSS.
	 *
	 * @param literal the additional literal.
	 */
	public void addLiteral(String literal) {
		if (!Strings.isEmpty(literal)) {
			this.additionalLiterals.add(literal);
		}
	}

	/** Add a keyword into which the Groogle Prettify CSS.
	 *
	 * @param keyword the additional keyword.
	 */
	public void addKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.additionalKeywords.add(keyword);
		}
	}

	/** Ignore a keyword into which the Groogle Prettify CSS.
	 *
	 * @param keyword the keyword to ignore.
	 */
	public void addIgnoreKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.excludedKeywords.add(keyword);
		}
	}

	/** Add a punctuation symbol into which the Groogle Prettify CSS.
	 *
	 * @param symbol the additional punctuation symbol.
	 */
	public void addPunctuation(String symbol) {
		if (!Strings.isEmpty(symbol)) {
			this.additionalPunctuation.add(symbol);
		}
	}

	/** Indicates if the native types must be added in the keyword list.
	 *
	 * @param addNativeTypes <code>true</code> for adding the native types.
	 */
	public void setAddNativeTypes(boolean addNativeTypes) {
		this.addNativeTypes = addNativeTypes;
	}

	@SuppressWarnings("checkstyle:nestedifdepth")
	private void exploreGrammar(Grammar grammar, Set<String> keywords, Set<String> punctuation,
			Set<String> literals, Set<String> ignored) {
		for (AbstractRule rule : grammar.getRules()) {
			TreeIterator<EObject> iterator = rule.eAllContents();
			while (iterator.hasNext()) {
				EObject object = iterator.next();
				if (object instanceof Keyword) {
					Keyword xkeyword = (Keyword) object;
					String value = xkeyword.getValue();
					if (!Strings.isEmpty(value)) {
						if (KEYWORD_PATTERN.matcher(value).matches()) {
							if (!literals.contains(value)) {
								if (this.excludedKeywords.contains(value)) {
									ignored.add(value);
								} else {
									keywords.add(value);
								}
							}
						} else if (PUNCTUATION_PATTERN.matcher(value).matches()) {
							punctuation.add(value);
						} else {
							LOG.debug(MessageFormat.format("IGNORE TOKEN = {0}", value)); //$NON-NLS-1$
						}
					}
				}
			}
		}
	}

	private static String toString(Iterable<?> list) {
		StringBuilder builder = new StringBuilder();
		for (Object o : list) {
			if (builder.length() > 0) {
				builder.append(", "); //$NON-NLS-1$
			}
			builder.append(o.toString());
		}
		return builder.toString();
	}

	private static void append(List<String> buffer, String text, Object... parameters) {
		buffer.add(MessageFormat.format(text, parameters));
	}

	@Override
	public void generate(Grammar grammar, XpandExecutionContext ctx) {
		LOG.info("Generating the CSS of SARL for Google Prettify"); //$NON-NLS-1$

		if (grammar == null) {
			throw new RuntimeException("No grammar defined"); //$NON-NLS-1$
		}

		Set<String> literals = new TreeSet<>();
		Set<String> keywords = new TreeSet<>();

		literals.addAll(this.additionalLiterals);

		for (String keyword : this.additionalKeywords) {
			if (!literals.contains(keyword)) {
				keywords.add(keyword);
			}
		}

		if (this.addNativeTypes) {
			for (Class<?> type : Primitives.ALL_PRIMITIVE_TYPES) {
				keywords.add(type.getSimpleName());
			}
		}

		Set<String> punctuation = new TreeSet<>();
		punctuation.addAll(this.additionalPunctuation);

		Queue<Grammar> grammars = new ArrayDeque<>();
		grammars.add(grammar);

		Set<String> ignored = new TreeSet<>();

		while (!grammars.isEmpty()) {
			Grammar grammarToTreat = grammars.poll();
			grammars.addAll(grammarToTreat.getUsedGrammars());
			exploreGrammar(grammarToTreat, keywords, punctuation, literals, ignored);
		}

		LOG.info(MessageFormat.format("Keywords: {0}", toString(keywords))); //$NON-NLS-1$
		LOG.info(MessageFormat.format("Literals: {0}", toString(literals))); //$NON-NLS-1$
		LOG.info(MessageFormat.format("Punctuation symbols: {0}", toString(punctuation))); //$NON-NLS-1$

		Set<String> tmp = new TreeSet<>(this.excludedKeywords);
		tmp.removeAll(ignored);
		if (!tmp.isEmpty()) {
			throw new RuntimeException(MessageFormat.format(
					"The following keywords cannot be ignored because they are not defined in the grammars: {0}", //$NON-NLS-1$
					toString(tmp)));
		}

		Set<Character> characters = new TreeSet<>();
		for (String punct : punctuation) {
			punct.chars().forEach((int candidate) -> characters.add(Character.valueOf((char) candidate)));
		}
		StringBuilder punctuationCharacters1 = new StringBuilder();
		StringBuilder punctuationCharacters2 = new StringBuilder();
		for (Character candidate : characters) {
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
		append(css, "PR[''registerLangHandler'']("); //$NON-NLS-1$
		append(css, "   PR[''createSimpleLexer'']("); //$NON-NLS-1$
		append(css, "      ["); //$NON-NLS-1$
		append(css, "         [PR[''PR_PLAIN''], /^[{0}]+/, null, ''{0}''],", this.whitespaces); //$NON-NLS-1$
		append(css, "         [PR[''PR_PUNCTUATION''], /^[{0}]+/, null, ''{1}''],", punctuationCharacters1, //$NON-NLS-1$
				punctuationCharacters2);
		append(css, "      ],"); //$NON-NLS-1$
		append(css, "      ["); //$NON-NLS-1$
		append(css, "         [PR[''PR_STRING''], /{0}/],", this.doubleQuotedStrings); //$NON-NLS-1$
		append(css, "         [PR[''PR_STRING''], /{0}/],", this.singleQuotedStrings); //$NON-NLS-1$
		append(css, "         [PR[''PR_LITERAL''], /{0}/],", this.characterLiteral); //$NON-NLS-1$
		append(css, "         [PR[''PR_KEYWORD''], /^(?:{0})\\b/],", keywordPattern); //$NON-NLS-1$
		append(css, "         [PR[''PR_LITERAL''], /^(?:{0})\\b/],", literalPattern); //$NON-NLS-1$
		append(css, "         [PR[''PR_LITERAL''], /{0}/i],", this.numberLiteral); //$NON-NLS-1$
		append(css, "         [PR[''PR_TYPE''], /{0}/],", this.typePattern); //$NON-NLS-1$
		append(css, "         [PR[''PR_PLAIN''], /{0}/],", this.plainTextPattern); //$NON-NLS-1$
		append(css, "         [PR[''PR_COMMENT''], /{0}/],", this.commentPattern); //$NON-NLS-1$
		append(css, "         [PR[''PR_PUNCTUATION''], /^(?:\\.+|\\/)/]"); //$NON-NLS-1$
		append(css, "      ]),"); //$NON-NLS-1$
		append(css, "   [''{0}'']);", this.language); //$NON-NLS-1$

		LOG.debug(css.toString());

		for (String output : this.outputDirectories) {
			File directory = new File(output).getAbsoluteFile();
			try {
				LOG.info(MessageFormat.format("\twriting into {0}", directory.getAbsolutePath())); //$NON-NLS-1$
				directory.mkdirs();
				File outputFile = new File(directory, "lang-" + this.language.toLowerCase() + ".js");  //$NON-NLS-1$//$NON-NLS-2$
				Files.write(Paths.get(outputFile.getAbsolutePath()), css);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}

	}

}

