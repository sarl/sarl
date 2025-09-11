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

package io.sarl.lang.mwe2.externalspec;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.apache.log4j.Logger;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.access.impl.Primitives;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.AbstractStringBuilderBasedAppendable;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.CodeConfig;

import io.sarl.lang.mwe2.keywords.GrammarKeywordAccessConfig;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create the highlighting in
 * external tools.
 *
 * @param <T> the type of appendable.
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 */
public abstract class AbstractExternalHighlightingFragment2<T extends IStyleAppendable> extends AbstractXtextGeneratorFragment {

	private static final Logger LOG = Logger.getLogger(AbstractExternalHighlightingFragment2.class);

	private static final Pattern KEYWORD_PATTERN = Pattern.compile("^[a-zA-Z]{2,}$"); //$NON-NLS-1$

	private static final Pattern PUNCTUATION_PATTERN = Pattern.compile("^[!#%&()*/+,\\-:;<=>?@\\[\\\\\\]^{|}~.%]+$"); //$NON-NLS-1$

	private static final Pattern MODIFIER_RULE_PATTERN = Pattern.compile("^[a-zA-Z]+Modifier$"); //$NON-NLS-1$

	private static final String README_BASENAME = "README"; //$NON-NLS-1$

	private static final String REGEX_SPECIAL_CHARS = "[\\<\\(\\[\\{\\\\\\^\\-\\=\\$\\!\\|\\]\\}\\)\\?\\*\\+\\.\\>\\:\\=\\!]"; //$NON-NLS-1$

	private static final String REGEX_SPECIAL_CHARS_PROTECT = "\\\\$0"; //$NON-NLS-1$

	@Inject
	private GrammarKeywordAccessConfig grammarKeywordAccessConfig;

	@Inject
	private CodeConfig codeConfig;

	@Inject
	private ExternalHighlightingConfig highlightingConfig;

	@Inject
	@Named("LANGUAGE_VERSION")
	private String languageVersion;

	private final Set<String> outputDirectories = new TreeSet<>();

	private String basename;

	private String basenameTemplate = "{0}.txt"; //$NON-NLS-1$

	private boolean enableColors = true;

	private final List<String> mimeTypes = new ArrayList<>();

	private Function2<? super File, ? super String, ? extends File> outputDirectoryFilter;

	/** Replies the default output directory filter.
	 *
	 * @return the filter, or {@code null}.
	 * @since 0.6
	 */
	public Function2<? super File, ? super String, ? extends File> getOutputDirectoryFilter() {
		return this.outputDirectoryFilter;
	}

	/** Change the default output directory filter.
	 *
	 * @param filter the filter, or {@code null} for avoiding default filtering.
	 * @since 0.6
	 */
	public void setOutputDirectoryFilter(Function2<? super File, ? super String, ? extends File> filter) {
		this.outputDirectoryFilter = filter;
	}

	/** Protect the given regular expression.
	 *
	 * @param regex the expression to protect.
	 * @return the protected expression.
	 */
	protected static String quoteRegex(String regex) {
		if (regex == null) {
			return ""; //$NON-NLS-1$
		}
		return regex.replaceAll(REGEX_SPECIAL_CHARS, REGEX_SPECIAL_CHARS_PROTECT);
	}

	/** Build a regular expression that is matching one of the given elements.
	 *
	 * @param elements the elements to match.
	 * @return the regular expression
	 */
	protected static String orRegex(Iterable<String> elements) {
		final var regex = new StringBuilder();
		for (final var element : elements) {
			if (regex.length() > 0) {
				regex.append("|"); //$NON-NLS-1$
			}
			regex.append("(?:"); //$NON-NLS-1$
			regex.append(quoteRegex(element));
			regex.append(")"); //$NON-NLS-1$
		}
		return regex.toString();
	}

	/** Build a regular expression that is matching one of the given keywords.
	 *
	 * @param keywords the keywords to match.
	 * @return the regular expression
	 */
	protected static String keywordRegex(Iterable<String> keywords) {
		return "\\b(?:" + orRegex(keywords) + ")\\b"; //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Concat the given iterables.
	 *
	 * @param iterables the iterables.
	 * @return the concat result.
	 */
	@SafeVarargs
	protected static Set<String> sortedConcat(Iterable<String>... iterables) {
		final var set = new TreeSet<String>();
		for (final var iterable : iterables) {
			for (final var obj : iterable) {
				set.add(obj);
			}
		}
		return set;
	}

	/** Add a mime type for the SARL source code.
	 *
	 * @param mimeType the mime type of SARL.
	 */
	public void addMimeType(String mimeType) {
		if (!Strings.isEmpty(mimeType)) {
			for (final var mtype : mimeType.split("[:;,]")) { //$NON-NLS-1$
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

	/** Change the basename of the file to generate.
	 *
	 * @param basename the basename.
	 */
	@Pure
	public void setBasename(String basename) {
		if (!Strings.isEmpty(basename)) {
			this.basename = basename;
		}
	}

	/** Replies the basename of the file to generate.
	 *
	 * @return the basename.
	 */
	@Pure
	public String getBasename() {
		return getBasename(null);
	}

	/** Replies the basename of the XML file to generate.
	 *
	 * @param defaultName the name to reply if the basename was not set.
	 * @return the basename.
	 */
	@Pure
	public String getBasename(String defaultName) {
		if (Strings.isEmpty(this.basename)) {
			return defaultName;
		}
		return this.basename;
	}

	/** Change the template for basename of the file to generate.
	 *
	 * @param pattern the template.
	 * @see MessageFormat
	 */
	@Pure
	public void setBasenameTemplate(String pattern) {
		if (!Strings.isEmpty(pattern)) {
			this.basenameTemplate = pattern;
		}
	}

	/** Replies the template for basename of the file to generate.
	 *
	 * @return the template.
	 */
	@Pure
	public String getBasenameTemplate() {
		return this.basenameTemplate;
	}

	/** Change if the colors could be output.
	 *
	 * @param useColors {@code true} to active the colors.
	 */
	public void setEnableColors(boolean useColors) {
		this.enableColors = useColors;
	}

	/** Replies if the colors could be output.
	 *
	 * @return {@code true} to active the colors.
	 */
	@Pure
	public boolean getEnableColors() {
		return this.enableColors;
	}

	/** Add an output directory.
	 *
	 * @param directory the directory.
	 */
	public void addOutput(String directory) {
		if (!Strings.isEmpty(directory)) {
			this.outputDirectories.add(directory);
		}
	}

	/** Replies the output directories.
	 *
	 * @return the output folders.
	 */
	@Pure
	public Set<String> getOutputs() {
		return this.outputDirectories;
	}

	/** Replies the code configuration.
	 *
	 * @return the code configuration.
	 */
	@Pure
	protected CodeConfig getCodeConfig() {
		return this.codeConfig;
	}

	/** Replies the highlighting configuration.
	 *
	 * @return the highlighting configuration.
	 */
	@Pure
	protected ExternalHighlightingConfig getHighlightingConfig() {
		return this.highlightingConfig;
	}

	/** Explore the grammar for extracting the key elements.
	 *
	 * @param grammar the grammar to explore.
	 * @param expressionKeywords the SARL keywords, usually within expressions.
	 * @param modifiers the modifier keywords.
	 * @param primitiveTypes the primitive types.
	 * @param punctuation the set of detected punctuation symbols.
	 * @param literals the set of detected literals.
	 * @param excludedKeywords the set of given excluded keywords.
	 * @param ignored the set of ignored tokens that is filled by this function.
	 */
	private static void exploreGrammar(Grammar grammar, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation,
			Set<String> literals, Set<String> excludedKeywords, Set<String> ignored) {
		for (final var rule : grammar.getRules()) {
			final var isModifierRule = MODIFIER_RULE_PATTERN.matcher(rule.getName()).matches();
			final var iterator = rule.eAllContents();
			while (iterator.hasNext()) {
				final var object = iterator.next();
				if (object instanceof Keyword xkeyword) {
					final var value = xkeyword.getValue();
					if (!Strings.isEmpty(value)) {
						if (KEYWORD_PATTERN.matcher(value).matches()) {
							if (!literals.contains(value) && !primitiveTypes.contains(value)) {
								if (excludedKeywords.contains(value)) {
									ignored.add(value);
								} else {
									if (isModifierRule) {
										modifiers.add(value);
									} else {
										expressionKeywords.add(value);
									}
								}
							}
						} else if (PUNCTUATION_PATTERN.matcher(value).matches()) {
							punctuation.add(value);
						}
					}
				}
			}
		}
	}

	@Override
	public final void generate() {
		final var grammar = getGrammar();
		if (grammar == null) {
			throw new RuntimeException("No grammar defined"); //$NON-NLS-1$
		}

		LOG.info(MessageFormat.format("Generating highlighting configuration for {0}", toString())); //$NON-NLS-1$

		final var literals = new TreeSet<String>();
		final var expressionKeywords = new TreeSet<String>();
		final var modifiers = new TreeSet<String>();
		final var primitiveTypes = new TreeSet<String>();

		final var hconfig = getHighlightingConfig();

		if (hconfig.getInheritFromGrammarKeywordAccesss() && this.grammarKeywordAccessConfig != null) {
			literals.addAll(this.grammarKeywordAccessConfig.getLiterals());
		}
		literals.addAll(hconfig.getLiterals());

		if (hconfig.getInheritFromGrammarKeywordAccesss() && this.grammarKeywordAccessConfig != null) {
			for (final var keyword : this.grammarKeywordAccessConfig.getKeywords()) {
				if (!literals.contains(keyword)) {
					expressionKeywords.add(keyword);
				}
			}
		}
		final var hKeywords = hconfig.getKeywords();
		for (final var keyword : hKeywords) {
			if (!literals.contains(keyword)) {
				expressionKeywords.add(keyword);
			}
		}

		if (hconfig.getAddNativeTypes()) {
			for (final var type : Primitives.ALL_PRIMITIVE_TYPES) {
				primitiveTypes.add(type.getSimpleName());
			}
		}

		final var punctuation = new TreeSet<String>();
		punctuation.addAll(hconfig.getPunctuation());

		final var grammars = new ArrayDeque<Grammar>();
		grammars.add(grammar);

		final var excluded = new HashSet<String>();
		if (hconfig.getInheritFromGrammarKeywordAccesss() && this.grammarKeywordAccessConfig != null) {
			for (final var ignoredKeyword : this.grammarKeywordAccessConfig.getIgnoredKeywords()) {
				if (!hKeywords.contains(ignoredKeyword)) {
					excluded.add(ignoredKeyword);
				}
			}
		}
		excluded.addAll(hconfig.getIgnoredKeywords());

		final var ignored = new TreeSet<String>();

		while (!grammars.isEmpty()) {
			final var grammarToTreat = grammars.poll();
			grammars.addAll(grammarToTreat.getUsedGrammars());
			exploreGrammar(grammarToTreat, expressionKeywords, modifiers, primitiveTypes, punctuation, literals, excluded, ignored);
		}
		expressionKeywords.removeAll(modifiers);

		final var tmp = new TreeSet<>(excluded);
		tmp.removeAll(ignored);
		if (!tmp.isEmpty()) {
			throw new RuntimeException(MessageFormat.format(
					"The following keywords cannot be ignored because they are not defined in the grammars: {0}", //$NON-NLS-1$
					tmp));
		}

		final var specialKeywords = new TreeSet<String>();
		for (final var specialKeyword : hconfig.getSpecialKeywords()) {
			if ((expressionKeywords.contains(specialKeyword) || modifiers.contains(specialKeyword)
					|| primitiveTypes.contains(specialKeyword)) && !ignored.contains(specialKeyword)) {
				specialKeywords.add(specialKeyword);
				expressionKeywords.remove(specialKeyword);
				modifiers.remove(specialKeyword);
				primitiveTypes.remove(specialKeyword);
			}
		}

		final var typeDeclarationKeywords = new TreeSet<String>();
		for (final var typeDeclarationKeyword : hconfig.getTypeDeclarationKeywords()) {
			if ((expressionKeywords.contains(typeDeclarationKeyword) || modifiers.contains(typeDeclarationKeyword)
					|| primitiveTypes.contains(typeDeclarationKeyword)) && !ignored.contains(typeDeclarationKeyword)) {
				typeDeclarationKeywords.add(typeDeclarationKeyword);
				expressionKeywords.remove(typeDeclarationKeyword);
				modifiers.remove(typeDeclarationKeyword);
				primitiveTypes.remove(typeDeclarationKeyword);
			}
		}

		generate(literals, expressionKeywords, modifiers, primitiveTypes,
				punctuation, ignored, specialKeywords, typeDeclarationKeywords);
	}

	/** Generate the external specification.
	 *
	 * @param literals the SARL literals.
	 * @param expressionKeywords the SARL keywords, usually within expressions.
	 * @param modifiers the modifier keywords.
	 * @param primitiveTypes the primitive types.
	 * @param punctuation the SARL punctuation symbols.
	 * @param ignored the ignored literals (mostly for information).
	 * @param specialKeywords the keywords that are marked as special. They are also in {@code keywords}.
	 * @param typeDeclarationKeywords the keywords that are marked as type declaration keywords.
	 *     They are also in {@code keywords}.
	 */
	protected final void generate(Set<String> literals, Set<String> expressionKeywords,
			Set<String> modifiers, Set<String> primitiveTypes, Set<String> punctuation,
			Set<String> ignored, Set<String> specialKeywords, Set<String> typeDeclarationKeywords) {
		final var appendable = newStyleAppendable();
		generate(appendable, literals, expressionKeywords, modifiers, primitiveTypes, punctuation, ignored,
				specialKeywords, typeDeclarationKeywords);
		final var language = getLanguageSimpleName().toLowerCase();
		final var basename = getBasename(MessageFormat.format(getBasenameTemplate(), language));
		writeFile(basename, appendable);
		generateAdditionalFiles(basename, appendable);
		generateReadme(basename);
	}

	/** Generate the external specification.
	 *
	 * @param appendable the appendable.
	 * @param literals the SARL literals.
	 * @param expressionKeywords the SARL keywords, usually within expressions.
	 * @param modifiers the modifier keywords.
	 * @param primitiveTypes the primitive types.
	 * @param punctuation the SARL punctuation symbols.
	 * @param ignored the ignored literals (mostly for information).
	 * @param specialKeywords the keywords that are marked as special. They are also in {@code keywords}.
	 * @param typeDeclarationKeywords the keywords that are marked as type declaration keywords.
	 *     They are also in {@code keywords}.
	 */
	protected abstract void generate(T appendable,
			Set<String> literals, Set<String> expressionKeywords, Set<String> modifiers, Set<String> primitiveTypes,
			Set<String> punctuation, Set<String> ignored, Set<String> specialKeywords, Set<String> typeDeclarationKeywords);

	/** Generate additional files.
	 *
	 * @param basename the basename for the language style.
	 * @param appendable the written appendable.
	 */
	protected void generateAdditionalFiles(String basename, T appendable) {
		//
	}

	/** Replies the content of the README file.
	 *
	 * @param basename the basename of the generated file.
	 * @return the content of the readme.
	 */
	protected abstract Object getReadmeFileContent(String basename);

	/** Contact the given strings of characters.
	 *
	 * @param lines the lines.
	 * @return the concatenation result.
	 */
	public static CharSequence concat(CharSequence... lines) {
		return new CharSequence() {

			private final StringBuilder content = new StringBuilder();

			private int next;

			private int length = -1;

			@Override
			public String toString() {
				ensure(length());
				return this.content.toString();
			}

			private void ensure(int index) {
				while (this.next < lines.length && index >= this.content.length()) {
					if (lines[this.next] != null) {
						this.content.append(lines[this.next]).append("\n"); //$NON-NLS-1$
					}
					++this.next;
				}
			}

			@Override
			public CharSequence subSequence(int start, int end) {
				ensure(end - 1);
				return this.content.subSequence(start, end);
			}

			@Override
			public int length() {
				if (this.length < 0) {
					var len = 0;
					for (final CharSequence seq : lines) {
						len += seq.length() + 1;
					}
					len = Math.max(0, len - 1);
					this.length = len;
				}
				return this.length;
			}

			@Override
			public char charAt(int index) {
				ensure(index);
				return this.content.charAt(index);
			}
		};
	}

	/** Generate the README file.
	 *
	 * @param basename the basename of the generated file.
	 * @since 0.6
	 */
	protected void generateReadme(String basename) {
		final var content = getReadmeFileContent(basename);
		if (content != null) {
			final var textualContent = content.toString();
			if (!Strings.isEmpty(textualContent)) {
				final var bytes = textualContent.getBytes();
				for (final var output : getOutputs()) {
					final var directory = new File(output).getAbsoluteFile();
					try {
						directory.mkdirs();
						final var outputFile = new File(directory, README_BASENAME);
						Files.write(Paths.get(outputFile.getAbsolutePath()), bytes);
					} catch (IOException e) {
						throw new RuntimeException(e);
					}
				}
			}
		}
	}

	/** Write the given lines into the file.
	 *
	 * @param basename the basename of the file.
	 * @param content the content of the style file.
	 */
	protected void writeFile(String basename, T content) {
		writeFile(basename, content, getOutputDirectoryFilter());
	}

	/** Write the given lines into the file.
	 *
	 * @param basename the basename of the file.
	 * @param content the content of the style file.
	 * @param outputDirectoryFilter the output directory.
	 */
	protected void writeFile(String basename, T content,
			Function2<? super File, ? super String, ? extends File> outputDirectoryFilter) {
		// Create the file.
		// Encode
		final var bytes = content.toString().getBytes(Charset.forName(getCodeConfig().getEncoding()));
		writeFile(basename, bytes, outputDirectoryFilter);
	}

	/** Write the given lines into the file.
	 *
	 * @param basename the basename of the file.
	 * @param content the content of the style file.
	 * @since 0.6
	 */
	protected void writeFile(String basename, byte[] content) {
		writeFile(basename, content, getOutputDirectoryFilter());
	}

	/** Write the given lines into the file.
	 *
	 * @param basename the basename of the file.
	 * @param content the content of the style file.
	 * @param outputDirectoryFilter the output directory.
	 * @throws RuntimeException a runtime exception
	 * @since 0.6
	 */
	protected void writeFile(String basename, byte[] content,
			Function2<? super File, ? super String, ? extends File> outputDirectoryFilter) {
		for (final var output : getOutputs()) {
			var directory = new File(output).getAbsoluteFile();
			if (outputDirectoryFilter != null) {
				directory = outputDirectoryFilter.apply(directory, basename);
			}
			try {
				final var outputFile = new File(directory, basename);
				outputFile.getParentFile().mkdirs();
				Files.write(Paths.get(outputFile.getAbsolutePath()), content);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	/** Replies the simple name of the language.
	 *
	 * @return the name.
	 */
	protected String getLanguageSimpleName() {
		final var name = getGrammar().getName();
		final var index = name.lastIndexOf('.');
		if (index > 0) {
			return name.substring(index + 1);
		}
		return name;
	}

	/** Replies the version of the language specification.
	 *
	 * @return the version.
	 */
	protected String getLanguageVersion() {
		return Strings.emptyIfNull(this.languageVersion);
	}

	/** Create a new style appendable.
	 *
	 * @return the appendable.
	 */
	protected abstract T newStyleAppendable();

	/** Merge the given lines with prefix.
	 *
	 * @param prefix the prefix to add to each line.
	 * @param lines the lines.
	 * @return the merged elements.
	 */
	@Pure
	protected String lines(String prefix, String... lines) {
		final var delimiter = getCodeConfig().getLineDelimiter();
		final var buffer = new StringBuilder();
		for (final var line : lines) {
			buffer.append(prefix);
			buffer.append(line);
			buffer.append(delimiter);
		}
		return buffer.toString();
	}

	/** Appendable for script-based styles.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version mwe2 0.15.1 20250911-224823
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid mwe2
	 * @since 0.6
	 */
	protected abstract static class AbstractAppendable extends AbstractStringBuilderBasedAppendable implements IStyleAppendable {

		private final CodeConfig codeConfig;

		private final String languageName;

		private final String languageVersion;

		/** Constructor.
		 *
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected AbstractAppendable(CodeConfig codeConfig, String languageName, String languageVersion) {
			this("  ", codeConfig, languageName, languageVersion); //$NON-NLS-1$
		}

		/** Constructor.
		 *
		 * @param indentation the string for a single indentation level.
		 * @param codeConfig the code configuration.
		 * @param languageName the language name.
		 * @param languageVersion the language version.
		 */
		protected AbstractAppendable(String indentation, CodeConfig codeConfig, String languageName, String languageVersion) {
			super(indentation, codeConfig.getLineDelimiter(), false);
			this.codeConfig = codeConfig;
			this.languageName = languageName;
			this.languageVersion = languageVersion;
		}

		/** Replies the simple name of the language.
		 *
		 * @return the name.
		 */
		protected String getLanguageSimpleName() {
			return this.languageName;
		}

		/** Replies the version of the language specification.
		 *
		 * @return the version.
		 */
		protected String getLanguageVersion() {
			return this.languageVersion;
		}

		/** Replies the code configuration.
		 *
		 * @return the code configuration.;
		 */
		protected CodeConfig getCodeConfig() {
			return this.codeConfig;
		}

		@Override
		protected void appendType(final JvmType type, StringBuilder builder) {
			builder.append(type.getQualifiedName());
		}

		@Override
		protected void appendType(final Class<?> type, StringBuilder builder) {
			builder.append(type.getName());
		}

		/** {@inheritDoc}.
		 * @deprecated Deprecated
		 */
		@Deprecated(since = "0.10", forRemoval = true)
		@Override
		public List<String> getImports() {
			return Collections.emptyList();
		}

	}

}
