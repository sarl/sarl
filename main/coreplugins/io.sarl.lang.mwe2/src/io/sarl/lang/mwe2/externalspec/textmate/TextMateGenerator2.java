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

package io.sarl.lang.mwe2.externalspec.textmate;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;

import com.google.common.io.Files;
import com.google.inject.Injector;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.ISourceAppender;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xtext.generator.CodeConfig;

import io.sarl.lang.mwe2.externalspec.AbstractExternalHighlightingFragment2;

/**
 * A {@link IGeneratorFragment} that create the language specification for
 * the TextMate-compatible viewers.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://manual.macromates.com/en/language_grammars"
 * @see "https://github.com/staltz/SublimeXtend/"
 */
public class TextMateGenerator2 extends AbstractExternalHighlightingFragment2<ITmStyleAppendable> {

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN_OLD = "{0}.property-list"; //$NON-NLS-1$

	/** The default basename pattern for {@link MessageFormat}.
	 */
	public static final String BASENAME_PATTERN_NEW = "{0}.tmLanguage"; //$NON-NLS-1$

	/** Name of the license file.
	 */
	public static final String LICENSE_FILE = "LICENSE"; //$NON-NLS-1$

	private static final String FIRST_LINE_MATCHING = "^//.*\\bsarl\\b."; //$NON-NLS-1$

	private static final String FOLDING_START = "\\{\\s*(//.*)?$"; //$NON-NLS-1$

	private static final String FOLDING_END = "^\\s*\\}"; //$NON-NLS-1$

	private static final String BLOCK_COMMENT_STYLE = "comment.block"; //$NON-NLS-1$

	private static final String BLOCK_COMMENT_DELIMITER_STYLE = "punctuation.definition.comment"; //$NON-NLS-1$

	private static final String LINE_COMMENT_STYLE = "comment.line"; //$NON-NLS-1$

	private static final String LINE_COMMENT_DELIMITER_STYLE = "comment.line.double-slash"; //$NON-NLS-1$

	private static final String DOUBLE_QUOTE_STRING_STYLE = "string.quoted.double"; //$NON-NLS-1$

	private static final String SINGLE_QUOTE_STRING_STYLE = "string.quoted.single"; //$NON-NLS-1$

	private static final String STRING_BEGIN_STYLE = "punctuation.definition.string.begin"; //$NON-NLS-1$

	private static final String STRING_END_STYLE = "punctuation.definition.string.end"; //$NON-NLS-1$

	private static final String ESCAPE_CHARACTER_STYLE = "constant.character.escape"; //$NON-NLS-1$

	private static final String LITERAL_STYLE = "constant.language"; //$NON-NLS-1$

	private static final String NUMBER_STYLE = "constant.numeric"; //$NON-NLS-1$

	private static final String PRIMITIVE_TYPE_STYLE = "storage.type.primitive"; //$NON-NLS-1$

	private static final String PUNCTUATION_STYLE = "keyword.operator"; //$NON-NLS-1$

	private static final String MODIFIER_STYLE = "storage.modifier"; //$NON-NLS-1$

	private static final String SPECIAL_KEYWORD_STYLE = "keyword.other.special"; //$NON-NLS-1$

	private static final String KEYWORD_STYLE = "keyword.control"; //$NON-NLS-1$

	private static final String TYPE_DECLARATION_STYLE = "keyword.other.declaration"; //$NON-NLS-1$

	private static final String ANNOTATION_STYLE = "support.type"; //$NON-NLS-1$

	private static final String BEGIN_PROP = "begin"; //$NON-NLS-1$

	private static final String END_PROP = "end"; //$NON-NLS-1$

	private static final String MATCH_PROP = "match"; //$NON-NLS-1$

	private static final String NAME_PROP = "name"; //$NON-NLS-1$

	private static final String SCOPE_NAME_PROP = "scopeName"; //$NON-NLS-1$

	private static final String CAPTURES_PROP = "captures"; //$NON-NLS-1$

	private static final String FILE_TYPES_PROP = "fileTypes"; //$NON-NLS-1$

	private static final String FIRST_LINE_MATCH_PROP = "firstLineMatch"; //$NON-NLS-1$

	private static final String FOLDING_START_PROP = "foldingStartMarker"; //$NON-NLS-1$

	private static final String FOLDING_END_PROP = "foldingStopMarker"; //$NON-NLS-1$

	private static final String PATTERNS_PROP = "patterns"; //$NON-NLS-1$

	private static final String SARL_VERSION_PROP = "__sarlVersion"; //$NON-NLS-1$

	private static final String VERSION_PROP = "version"; //$NON-NLS-1$

	private static final String UUID_PROP = "uuid"; //$NON-NLS-1$

	private static final String BEGIN_CAPTURES_PROP = "beginCaptures"; //$NON-NLS-1$

	private static final String END_CAPTURES_PROP = "endCaptures"; //$NON-NLS-1$

	private static final String COMMENT_PROP = "comment"; //$NON-NLS-1$

	@Override
	public String toString() {
		return "TextMate"; //$NON-NLS-1$
	}

	@Override
	public void initialize(Injector injector) {
		super.initialize(injector);
		setBasenameTemplate(BASENAME_PATTERN_OLD);
	}

	@Override
	protected ITmStyleAppendable newStyleAppendable() {
		return new CombinedTmAppendable(getCodeConfig(), getLanguageSimpleName(), getLanguageVersion());
	}

	/** Replies the UUID of the TextMate style.
	 *
	 * @return the UUID.
	 */
	protected UUID getUUID() {
		return UUID.nameUUIDFromBytes(getLanguageVersion().getBytes());
	}

	@Override
	@SuppressWarnings("checkstyle:parameternumber")
	protected void generate(ITmStyleAppendable appendable, Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation, Set<String> ignored,
			Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		final Map<String, Object> root = new HashMap<>();
		root.put(NAME_PROP, getLanguageSimpleName());
		root.put(SARL_VERSION_PROP, getLanguageVersion());
		root.put(VERSION_PROP, getLanguageVersion());
		root.put(UUID_PROP, getUUID().toString().toUpperCase());
		root.put(SCOPE_NAME_PROP, "source." + getLanguage().getFileExtensions().get(0)); //$NON-NLS-1$
		root.put(FILE_TYPES_PROP, getLanguage().getFileExtensions());
		root.put(FIRST_LINE_MATCH_PROP, FIRST_LINE_MATCHING);
		root.put(FOLDING_START_PROP, FOLDING_START);
		root.put(FOLDING_END_PROP, FOLDING_END);

		root.put(PATTERNS_PROP, createPatterns(literals, expressionKeywords, modifiers, primitiveTypes,
				punctuation, ignored, specialKeywords, typeDeclarationKeywords));

		appendable.appendHeader();
		appendable.appendProperty(root);
		appendable.appendFooter();
	}

	@Override
	protected void generateAdditionalFiles(String oldBasename, ITmStyleAppendable writtenAppendable) {
		if (writtenAppendable instanceof CombinedTmAppendable) {
			generatePlistFile((CombinedTmAppendable) writtenAppendable);
		}
		generateLicenseFile();
	}

	/** Generate the Plist file.
	 *
	 * @param it the appendable used for generated the "tmLanguage" file.
	 */
	protected void generatePlistFile(CombinedTmAppendable it) {
		final String language = getLanguageSimpleName().toLowerCase();
		final String newBasename = getBasename(MessageFormat.format(BASENAME_PATTERN_NEW, language));
		writeFile(newBasename, it.getNewSyntaxContent());
	}

	/** Generate the LICENSE file.
	 */
	protected void generateLicenseFile() {
		final CharSequence licenseText = getLicenseText();
		if (licenseText != null) {
			final String text = licenseText.toString();
			if (!Strings.isEmpty(text)) {
				writeFile(LICENSE_FILE, text.getBytes());
			}
		}
	}

	/** Replies the text of the license to write to the generated output.
	 *
	 * @return the text.
	 */
	protected CharSequence getLicenseText() {
		final URL url = getClass().getResource(LICENSE_FILE);
		if (url != null) {
			final File filename = new File(url.getPath());
			try {
				final byte[] array = Files.toByteArray(filename);
				if (array != null) {
					return new String(array);
				}
			} catch (IOException exception) {
				throw new RuntimeException(exception);
			}
		}
		return null;
	}

	/** Create the patterns.
	 *
	 * @param literals the SARL literals.
	 * @param expressionKeywords the SARL keywords, usually within expressions.
	 * @param modifiers the modifier keywords.
	 * @param primitiveTypes the primitive types.
	 * @param punctuation the SARL punctuation symbols.
	 * @param ignored the ignored literals (mostly for information).
	 * @param specialKeywords the keywords that are marked as special. They are also in keywords.
	 * @param typeDeclarationKeywords the keywords that are marked as type declaration keywords. They are also in keywords.
	 * @return the patterns.
	 */
	protected List<?> createPatterns(Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation, Set<String> ignored,
			Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		final List<Map<String, ?>> patterns = new ArrayList<>();

		patterns.addAll(generateComments());
		patterns.addAll(generateStrings());
		patterns.addAll(generateNumericConstants());

		patterns.addAll(generateAnnotations());

		patterns.addAll(generateLiterals(literals));

		patterns.addAll(generatePrimitiveTypes(primitiveTypes));

		patterns.addAll(generateSpecialKeywords(specialKeywords));
		patterns.addAll(generateModifiers(modifiers));
		patterns.addAll(generateTypeDeclarations(typeDeclarationKeywords));
		patterns.addAll(generateStandardKeywords(expressionKeywords));

		patterns.addAll(generatePunctuation(punctuation));

		return patterns;
	}

	/** Generate the rules for the annotations.
	 *
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateAnnotations() {
		final List<Map<String, ?>> list = new ArrayList<>();
		list.add(pattern(it -> {
			it.matches("\\@[_a-zA-Z$][_0-9a-zA-Z$]*"); //$NON-NLS-1$
			it.style(ANNOTATION_STYLE);
			it.comment("Annotations"); //$NON-NLS-1$
		}));
		return list;
	}

	/** Generate the rules for the comments.
	 *
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateComments() {
		final List<Map<String, ?>> list = new ArrayList<>();
		// Block comment
		list.add(pattern(it -> {
			it.delimiters("(/\\*+)", "(\\*/)"); //$NON-NLS-1$ //$NON-NLS-2$
			it.style(BLOCK_COMMENT_STYLE);
			it.beginStyle(BLOCK_COMMENT_DELIMITER_STYLE);
			it.endStyle(BLOCK_COMMENT_DELIMITER_STYLE);
			it.pattern(it2 -> {
				it2.matches("^\\s*(\\*)(?!/)"); //$NON-NLS-1$
				it2.style(BLOCK_COMMENT_DELIMITER_STYLE);
			});
			it.comment("Multiline comments"); //$NON-NLS-1$
		}));
		// Line comment
		list.add(pattern(it -> {
			it.matches("\\s*(//)(.*)$"); //$NON-NLS-1$
			it.substyle(1, LINE_COMMENT_DELIMITER_STYLE);
			it.substyle(2, LINE_COMMENT_STYLE);
			it.comment("Single-line comment"); //$NON-NLS-1$
		}));
		return list;
	}

	/** Generates the rules for the strings of characters.
	 *
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateStrings() {
		final List<Map<String, ?>> list = new ArrayList<>();
		// Double quote
		list.add(pattern(it -> {
			it.delimiters("\"", "\""); //$NON-NLS-1$ //$NON-NLS-2$
			it.style(DOUBLE_QUOTE_STRING_STYLE);
			it.beginStyle(STRING_BEGIN_STYLE);
			it.endStyle(STRING_END_STYLE);
			it.pattern(it2 -> {
				it2.matches("\\\\."); //$NON-NLS-1$
				it2.style(ESCAPE_CHARACTER_STYLE);
			});
			it.comment("Double quoted strings of characters"); //$NON-NLS-1$
		}));
		// Single quote
		list.add(pattern(it -> {
			it.delimiters("'", "'"); //$NON-NLS-1$ //$NON-NLS-2$
			it.style(SINGLE_QUOTE_STRING_STYLE);
			it.beginStyle(STRING_BEGIN_STYLE);
			it.endStyle(STRING_END_STYLE);
			it.pattern(it2 -> {
				it2.matches("\\\\."); //$NON-NLS-1$
				it2.style(ESCAPE_CHARACTER_STYLE);
			});
			it.comment("Single quoted strings of characters"); //$NON-NLS-1$
		}));
		return list;
	}

	/** Generate the rules for the numeric constants.
	 *
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateNumericConstants() {
		final List<Map<String, ?>> list = new ArrayList<>();
		list.add(pattern(it -> {
			it.matches(
					"(?:" //$NON-NLS-1$
					+ "[0-9][0-9]*\\.[0-9]+([eE][0-9]+)?[fFdD]?" //$NON-NLS-1$
					+ ")|(?:" //$NON-NLS-1$
					+ "0[xX][0-9a-fA-F]+" //$NON-NLS-1$
					+ ")|(?:" //$NON-NLS-1$
					+ "[0-9]+[lL]?" //$NON-NLS-1$
					+ ")"); //$NON-NLS-1$
			it.style(NUMBER_STYLE);
			it.comment("Numbers"); //$NON-NLS-1$
		}));
		return list;
	}

	/** Generate the rules for the primitive types.
	 *
	 * @param primitiveTypes the primitive types.
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generatePrimitiveTypes(Set<String> primitiveTypes) {
		final List<Map<String, ?>> list = new ArrayList<>();
		if (!primitiveTypes.isEmpty()) {
			list.add(pattern(it -> {
				it.matches(keywordRegex(primitiveTypes) + "(?:\\s*\\[\\s*\\])*"); //$NON-NLS-1$
				it.style(PRIMITIVE_TYPE_STYLE);
				it.comment("Primitive types"); //$NON-NLS-1$
			}));
		}
		return list;
	}

	/** Generate the rules for the literals.
	 *
	 * @param literals the literals.
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateLiterals(Set<String> literals) {
		final List<Map<String, ?>> list = new ArrayList<>();
		if (!literals.isEmpty()) {
			list.add(pattern(it -> {
				it.matches(keywordRegex(literals));
				it.style(LITERAL_STYLE);
				it.comment("SARL Literals and Constants"); //$NON-NLS-1$
			}));
		}
		return list;
	}

	/** Generate the rules for the punctuation symbols.
	 *
	 * @param punctuation the punctuation symbols.
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generatePunctuation(Set<String> punctuation) {
		final List<Map<String, ?>> list = new ArrayList<>();
		if (!punctuation.isEmpty()) {
			list.add(pattern(it -> {
				it.matches(orRegex(punctuation));
				it.style(PUNCTUATION_STYLE);
				it.comment("Operators and Punctuations"); //$NON-NLS-1$
			}));
		}
		return list;
	}

	/** Generate the rules for the modifier keywords.
	 *
	 * @param modifiers the modifier keywords.
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateModifiers(Set<String> modifiers) {
		final List<Map<String, ?>> list = new ArrayList<>();
		if (!modifiers.isEmpty()) {
			list.add(pattern(it -> {
				it.matches(keywordRegex(modifiers));
				it.style(MODIFIER_STYLE);
				it.comment("Modifiers"); //$NON-NLS-1$
			}));
		}
		return list;
	}

	/** Generate the rules for the special keywords.
	 *
	 * @param keywords the special keywords.
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateSpecialKeywords(Set<String> keywords) {
		final List<Map<String, ?>> list = new ArrayList<>();
		if (!keywords.isEmpty()) {
			list.add(pattern(it -> {
				it.matches(keywordRegex(keywords));
				it.style(SPECIAL_KEYWORD_STYLE);
				it.comment("Special Keywords"); //$NON-NLS-1$
			}));
		}
		return list;
	}

	/** Generate the rules for the standard keywords.
	 *
	 * @param keywords the standard keywords.
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateStandardKeywords(Set<String> keywords) {
		final List<Map<String, ?>> list = new ArrayList<>();
		if (!keywords.isEmpty()) {
			list.add(pattern(it -> {
				it.matches(keywordRegex(keywords));
				it.style(KEYWORD_STYLE);
				it.comment("Standard Keywords"); //$NON-NLS-1$
			}));
		}
		return list;
	}

	/** Generate the rules for the type declaration keywords.
	 *
	 * @param declarators the type declaration keywords.
	 * @return the rules.
	 */
	protected List<Map<String, ?>> generateTypeDeclarations(Set<String> declarators) {
		final List<Map<String, ?>> list = new ArrayList<>();
		if (!declarators.isEmpty()) {
			list.add(pattern(it -> {
				it.matches(keywordRegex(declarators));
				it.style(TYPE_DECLARATION_STYLE);
				it.comment("Type Declarations"); //$NON-NLS-1$
			}));
		}
		return list;
	}

	/** Build a pattern definition.
	 *
	 * @param proc the initializer.
	 * @return the definition.
	 */
	protected Map<String, ?> pattern(Procedure1<? super Pattern> proc) {
		final Pattern patternDefinition = new Pattern();
		proc.apply(patternDefinition);
		return patternDefinition.getDefinition();
	}

	@Override
	protected Object getReadmeFileContent(String basename) {
		return concat(
				"SARL highlighting for TextMate", //$NON-NLS-1$
				"==============================", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"A TextMate package to apply syntax highlighting for the SARL language.", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"1. INSTALLATION", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"1.1. Manual", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"Copy the " + basename + "file into one of the following folders:", //$NON-NLS-1$ //$NON-NLS-2$
				"* Sublime Text 3: $HOME/.config/sublime-text-3/Packages/User/SARL/", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"1.2 With Sublime TextPackage Control", //$NON-NLS-1$
				"", //$NON-NLS-1$
				"If you have the Package Control package installed, you can install the highlighting format", //$NON-NLS-1$
				"from inside Sublime Text itself. Open the Command Palette and select \"Package Control: ", //$NON-NLS-1$
				"Install Package\", then search for \"SARL\"."); //$NON-NLS-1$
	}

	/** Definition of a pattern.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected final class Pattern {

		private Map<String, Object> content = new HashMap<>();

		private Map<Integer, String> captures = new HashMap<>();

		private List<Map<String, ?>> patterns = new ArrayList<>();

		/** Constructor.
		 */
		Pattern() {
			//
		}

		/** Finalize the definition.
		 *
		 * @return the definition.
		 */
		public Map<String, Object> getDefinition() {
			if (!this.captures.isEmpty()) {
				final Map<String, Map<String, String>> captures = new HashMap<>();
				for (final Entry<Integer, String> part : this.captures.entrySet()) {
					final Map<String, String> val = new HashMap<>();
					val.put(NAME_PROP, part.getValue());
					captures.put(part.getKey().toString(), val);
				}
				this.content.put(CAPTURES_PROP, captures);
			}
			if (!this.patterns.isEmpty()) {
				this.content.put(PATTERNS_PROP, this.patterns);
			}
			return this.content;
		}

		/** Define a pattern inside a pattern.
		 *
		 * @param proc the initializer.
		 * @see TextMateGenerator2#pattern(Procedure1)
		 */
		public void pattern(Procedure1<? super Pattern> proc) {
			final Map<String, ?> pattern = TextMateGenerator2.this.pattern(proc);
			this.patterns.add(pattern);
		}

		/** Add a comment.
		 *
		 * @param text the text of the comment.
		 */
		public void comment(String text) {
			this.content.put(COMMENT_PROP, text);
		}

		/** Add the delimiting patterns.
		 *
		 * <p>The single pattern is removed if it is defined.
		 *
		 * @param begin the start pattern.
		 * @param end the end pattern.
		 */
		public void delimiters(String begin, String end) {
			this.content.put(BEGIN_PROP, begin);
			this.content.put(END_PROP, end);
			this.content.remove(MATCH_PROP);
		}

		/** Add the pattern.
		 *
		 * <p>The begin/end patterns are removed if they are defined.
		 *
		 * @param pattern the pattern.
		 */
		public void matches(String pattern) {
			this.content.put(MATCH_PROP, pattern);
			this.content.remove(BEGIN_PROP);
			this.content.remove(END_PROP);
		}

		@SuppressWarnings("synthetic-access")
		private String toName(String name) {
			return name + "." + getLanguageSimpleName().toLowerCase(); //$NON-NLS-1$
		}

		/** Add the SARL specific style.
		 *
		 * <p>The SARL specific style name is the given TextMate style name post-fixed by the SARL key.
		 *
		 * @param name the name of the TextMate style.
		 */
		public void style(String name) {
			tmStyle(toName(name));
		}

		/** Add a style to a part of the pattern.
		 *
		 * <p>The given style name will be post-fixed by the SARL key, according to
		 * {@link #style(String)}
		 *
		 * @param part the index of the part (see group index in regular expression).
		 * @param style the style name.
		 */
		public void substyle(int part, String style) {
			this.captures.put(part, toName(style));
		}

		/** Set the style for the begin pattern.
		 *
		 * <p>The given style name will be post-fixed by the SARL key, according to
		 * {@link #style(String)}
		 *
		 * @param style the style name.
		 */
		public void endStyle(String style) {
			final Map<Integer, Map<String, String>> value = new HashMap<>();
			final Map<String, String> svalue = new HashMap<>();
			svalue.put(NAME_PROP, toName(style));
			value.put(0, svalue);
			this.content.put(BEGIN_CAPTURES_PROP, value);
		}

		/** Set the style for the end pattern.
		 *
		 * <p>The given style name will be post-fixed by the SARL key, according to
		 * {@link #style(String)}
		 *
		 * @param style the style name.
		 */
		public void beginStyle(String style) {
			final Map<Integer, Map<String, String>> value = new HashMap<>();
			final Map<String, String> svalue = new HashMap<>();
			svalue.put(NAME_PROP, toName(style));
			value.put(0, svalue);
			this.content.put(END_CAPTURES_PROP, value);
		}

		/** Add the style.
		 *
		 * @param name the name of the style.
		 */
		public void tmStyle(String name) {
			this.content.put(NAME_PROP, name);
		}

	}

	/** Appendable for TextMate styles.
	 *
	 * <p>This appendable uses the old property-file syntax.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class OldStyleTmAppendable extends AbstractAppendable implements ITmStyleAppendable {

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected OldStyleTmAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			super("  ", codeConfig, languageName, languageVersion); //$NON-NLS-1$
		}

		@Override
		protected void appendType(final JvmType type, StringBuilder builder) {
			builder.append(type.getQualifiedName());
		}

		@Override
		protected void appendType(final Class<?> type, StringBuilder builder) {
			builder.append(type.getName());
		}

		@Override
		public void appendHeader() {
			//
		}

		@Override
		public void appendFooter() {
			//
		}

		@Override
		public void appendComment(String text, Object... parameters) {
			//
		}

		/** Create a single quoted string.
		 *
		 * <p>The {@code '} character is protected with {@code ''}.
		 *
		 * @param value the string of characters.
		 * @return the protected string.
		 */
		@SuppressWarnings("static-method")
		protected String protect(String value) {
			if (value == null) {
				return new String();
			}
			return value.replaceAll("\\'", "''"); //$NON-NLS-1$ //$NON-NLS-2$
		}

		@Override
		public void appendProperty(Object value) {
			if (value instanceof Iterable) {
				append("("); //$NON-NLS-1$
				increaseIndentation().newLine();
				boolean first = true;
				for (final Object arrayElement : (Iterable<?>) value) {
					if (first) {
						first = false;
					} else {
						append(",").newLine(); //$NON-NLS-1$
					}
					appendProperty(arrayElement);
				}
				decreaseIndentation().newLine();
				append(")"); //$NON-NLS-1$
			} else if (value instanceof Map) {
				append("{"); //$NON-NLS-1$
				increaseIndentation().newLine();
				boolean first = true;
				for (final Entry<?, ?> entry : ((Map<?, ?>) value).entrySet()) {
					final Object rawKey = entry.getKey();
					if (rawKey != null) {
						if (first) {
							first = false;
						} else {
							append(";").newLine(); //$NON-NLS-1$
						}
						final Object rawValue = entry.getValue();
						append(rawKey.toString());
						append(" = "); //$NON-NLS-1$
						appendProperty(rawValue);
					}
				}
				decreaseIndentation().newLine();
				append("}"); //$NON-NLS-1$
			} else {
				append("'"); //$NON-NLS-1$
				append(value == null ? "" : protect(value.toString())); //$NON-NLS-1$
				append("'"); //$NON-NLS-1$
			}
		}

	}

	/** Appendable for TextMate styles.
	 *
	 * <p>This appendable uses the XML-based syntax.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static class XmlBasedTmAppendable extends AbstractAppendable implements ITmStyleAppendable {

		private static final String PLIST_VERSION = "1.0"; //$NON-NLS-1$

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected XmlBasedTmAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			super("\t", codeConfig, languageName, languageVersion); //$NON-NLS-1$
		}

		@Override
		protected void appendType(final JvmType type, StringBuilder builder) {
			builder.append(type.getQualifiedName());
		}

		@Override
		protected void appendType(final Class<?> type, StringBuilder builder) {
			builder.append(type.getName());
		}

		@Override
		public void appendHeader() {
			appendNl(MessageFormat.format("<?xml version=\"1.0\" encoding=\"{0}\"?>", //$NON-NLS-1$
					getCodeConfig().getEncoding()));
			append("<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-"); //$NON-NLS-1$
			append(PLIST_VERSION);
			appendNl(".dtd\">"); //$NON-NLS-1$
			final String[] header = Strings.emptyIfNull(getCodeConfig().getFileHeader()).split("[\n\r]+"); //$NON-NLS-1$
			appendNl("<!--"); //$NON-NLS-1$
			for (final String headerLine : header) {
				appendNl(headerLine.replaceFirst("^\\s*[/]?[*][/]?", "\t ") //$NON-NLS-1$//$NON-NLS-2$
						.replaceFirst("\\s+$", "")); //$NON-NLS-1$//$NON-NLS-2$
			}
			appendNl("-->"); //$NON-NLS-1$
			append("<plist version=\""); //$NON-NLS-1$
			append(PLIST_VERSION);
			appendNl("\">"); //$NON-NLS-1$
		}

		@Override
		public void appendFooter() {
			newLine();
			appendNl("</plist>"); //$NON-NLS-1$
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
		public void appendProperty(Object value) {
			if (value instanceof Iterable) {
				append("<array>"); //$NON-NLS-1$
				increaseIndentation().newLine();
				boolean first = true;
				for (final Object arrayElement : (Iterable<?>) value) {
					if (first) {
						first = false;
					} else {
						newLine();
					}
					appendProperty(arrayElement);
				}
				decreaseIndentation().newLine();
				append("</array>"); //$NON-NLS-1$
			} else if (value instanceof Map) {
				append("<dict>"); //$NON-NLS-1$
				increaseIndentation().newLine();
				boolean first = true;
				for (final Entry<?, ?> entry : ((Map<?, ?>) value).entrySet()) {
					final Object rawKey = entry.getKey();
					if (rawKey != null) {
						if (first) {
							first = false;
						} else {
							newLine();
						}
						final Object rawValue = entry.getValue();
						append("<key>"); //$NON-NLS-1$
						append(rawKey.toString());
						appendNl("</key>"); //$NON-NLS-1$
						appendProperty(rawValue);
					}
				}
				decreaseIndentation().newLine();
				append("</dict>"); //$NON-NLS-1$
			} else {
				append("<string>"); //$NON-NLS-1$
				if (value != null) {
					append(protect(value.toString()));
				}
				append("</string>"); //$NON-NLS-1$
			}
		}

		/** Protect the given string in order to be a valid XML string.
		 *
		 * @param value the value to protect.
		 * @return the XML equivalent.
		 */
		@SuppressWarnings("static-method")
		protected String protect(String value) {
			String tmp = value.replaceAll("&", "&amp;"); //$NON-NLS-1$//$NON-NLS-2$
			tmp = tmp.replaceAll(">", "&gt;"); //$NON-NLS-1$ //$NON-NLS-2$
			return tmp.replaceAll("<", "&lt;"); //$NON-NLS-1$ //$NON-NLS-2$
		}

	}

	/** Appendable for TextMate styles that combines the old syntax and the new syntax for the
	 * property file..
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	protected static final class CombinedTmAppendable implements ITmStyleAppendable {

		private final OldStyleTmAppendable oldAppendable;

		private final XmlBasedTmAppendable newAppendable;

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected CombinedTmAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			this.oldAppendable = new OldStyleTmAppendable(codeConfig, languageName, languageVersion);
			this.newAppendable = new XmlBasedTmAppendable(codeConfig, languageName, languageVersion);
		}

		@Override
		public String toString() {
			return this.oldAppendable.toString();
		}

		/** Replies the content with the old syntax.
		 *
		 * @return the content.
		 */
		public ITmStyleAppendable getOldSyntaxContent() {
			return this.oldAppendable;
		}

		/** Replies the content with the new syntax.
		 *
		 * @return the content.
		 */
		public ITmStyleAppendable getNewSyntaxContent() {
			return this.newAppendable;
		}

		@Override
		public void appendComment(String text, Object... parameters) {
			this.oldAppendable.appendComment(text, parameters);
			this.newAppendable.appendComment(text, parameters);
		}

		@Override
		public void appendHeader() {
			this.oldAppendable.appendHeader();
			this.newAppendable.appendHeader();
		}

		@Override
		public ISourceAppender append(CharSequence string) {
			this.oldAppendable.append(string);
			this.newAppendable.append(string);
			return this;
		}

		@Override
		public ISourceAppender append(JvmType type) {
			this.oldAppendable.append(type);
			this.newAppendable.append(type);
			return this;
		}

		@Override
		public ISourceAppender append(LightweightTypeReference typeRef) {
			this.oldAppendable.append(typeRef);
			this.newAppendable.append(typeRef);
			return this;
		}

		@Override
		public ISourceAppender newLine() {
			this.oldAppendable.newLine();
			this.newAppendable.newLine();
			return this;
		}

		@Override
		public ISourceAppender increaseIndentation() {
			this.oldAppendable.increaseIndentation();
			this.newAppendable.increaseIndentation();
			return this;
		}

		@Override
		public ISourceAppender decreaseIndentation() {
			this.oldAppendable.decreaseIndentation();
			this.newAppendable.decreaseIndentation();
			return this;
		}

		@Override
		public boolean isJava() {
			return this.oldAppendable.isJava() || this.newAppendable.isJava();
		}

		@Override
		public void appendFooter() {
			this.oldAppendable.appendFooter();
			this.newAppendable.appendFooter();
		}

		@Override
		public void appendProperty(Object value) {
			this.oldAppendable.appendProperty(value);
			this.newAppendable.appendProperty(value);
		}

	}

}

