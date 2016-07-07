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

package io.sarl.lang.mwe2.externalspec;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import javax.inject.Named;

import com.google.inject.Inject;
import org.apache.log4j.Logger;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.common.types.access.impl.Primitives;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.AbstractXtextGeneratorFragment;
import org.eclipse.xtext.xtext.generator.CodeConfig;

/**
 * A {@link AbstractXtextGeneratorFragment} that enables to create the highlighting in
 * external tools.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractExternalHighlightingFragment2 extends AbstractXtextGeneratorFragment {

	private static final Logger LOG = Logger.getLogger(AbstractExternalHighlightingFragment2.class);

	private static final Pattern KEYWORD_PATTERN = Pattern.compile("^[a-zA-Z]{2,}$"); //$NON-NLS-1$

	@SuppressWarnings("checkstyle:linelength")
	private static final Pattern PUNCTUATION_PATTERN = Pattern.compile("^[!#%&()*/+,\\-:;<=>?@\\[\\\\\\]^{|}~.%]+$"); //$NON-NLS-1$

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
	 * @param useColors <code>true</code> to active the colors.
	 */
	public void setEnableColors(boolean useColors) {
		this.enableColors = useColors;
	}

	/** Replies if the colors could be output.
	 *
	 * @return <code>true</code> to active the colors.
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
	 * @param keywords the set of detected keywords.
	 * @param punctuation the set of detected punctuation symbols.
	 * @param literals the set of detected literals.
	 * @param excludedKeywords the set of given excluded keywords.
	 * @param ignored the set of ignored tokens that is filled by this function.
	 */
	@SuppressWarnings("checkstyle:nestedifdepth")
	private static void exploreGrammar(Grammar grammar, Set<String> keywords, Set<String> punctuation,
			Set<String> literals, Set<String> excludedKeywords, Set<String> ignored) {
		for (final AbstractRule rule : grammar.getRules()) {
			final TreeIterator<EObject> iterator = rule.eAllContents();
			while (iterator.hasNext()) {
				final EObject object = iterator.next();
				if (object instanceof Keyword) {
					final Keyword xkeyword = (Keyword) object;
					final String value = xkeyword.getValue();
					if (!Strings.isEmpty(value)) {
						if (KEYWORD_PATTERN.matcher(value).matches()) {
							if (!literals.contains(value)) {
								if (excludedKeywords.contains(value)) {
									ignored.add(value);
								} else {
									keywords.add(value);
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
	@SuppressWarnings("checkstyle:npathcomplexity")
	public final void generate() {
		final Grammar grammar = getGrammar();
		if (grammar == null) {
			throw new RuntimeException("No grammar defined"); //$NON-NLS-1$
		}

		LOG.info(MessageFormat.format("Generating highlighting configuration for {0}", toString())); //$NON-NLS-1$

		final Set<String> literals = new TreeSet<>();
		final Set<String> keywords = new TreeSet<>();

		final ExternalHighlightingConfig hconfig = getHighlightingConfig();

		literals.addAll(hconfig.getLiterals());

		for (final String keyword : hconfig.getKeywords()) {
			if (!literals.contains(keyword)) {
				keywords.add(keyword);
			}
		}

		if (hconfig.getAddNativeTypes()) {
			for (final Class<?> type : Primitives.ALL_PRIMITIVE_TYPES) {
				keywords.add(type.getSimpleName());
			}
		}

		final Set<String> punctuation = new TreeSet<>();
		punctuation.addAll(hconfig.getPunctuation());

		final Queue<Grammar> grammars = new ArrayDeque<>();
		grammars.add(grammar);

		final Set<String> excluded = hconfig.getIgnoredKeywords();

		final Set<String> ignored = new TreeSet<>();

		while (!grammars.isEmpty()) {
			final Grammar grammarToTreat = grammars.poll();
			grammars.addAll(grammarToTreat.getUsedGrammars());
			exploreGrammar(grammarToTreat, keywords, punctuation, literals, excluded, ignored);
		}

		final Set<String> tmp = new TreeSet<>(excluded);
		tmp.removeAll(ignored);
		if (!tmp.isEmpty()) {
			throw new RuntimeException(MessageFormat.format(
					"The following keywords cannot be ignored because they are not defined in the grammars: {0}", //$NON-NLS-1$
					tmp));
		}

		final Set<String> specialKeywords = new TreeSet<>();
		for (final String specialKeyword : hconfig.getSpecialKeywords()) {
			if (keywords.contains(specialKeyword) && !ignored.contains(specialKeyword)) {
				specialKeywords.add(specialKeyword);
			}
		}

		final Set<String> typeDeclarationKeywords = new TreeSet<>();
		for (final String typeDeclarationKeyword : hconfig.getTypeDeclarationKeywords()) {
			if (keywords.contains(typeDeclarationKeyword) && !ignored.contains(typeDeclarationKeyword)) {
				typeDeclarationKeywords.add(typeDeclarationKeyword);
			}
		}

		generate(literals, keywords, punctuation, ignored, specialKeywords, typeDeclarationKeywords);
	}

	/** Generate the external specification.
	 *
	 * @param literals - the SARL literals.
	 * @param keywords - the SARL keywords.
	 * @param punctuation - the SARL punctuation symbols.
	 * @param ignored - the ignored literals (mostly for information).
	 * @param specialKeywords - the keywords that are marked as special. They are also in {@code keywords}.
	 * @param typeDeclarationKeywords - the keywords that are marked as type declaration keywords.
	 *     They are also in {@code keywords}.
	 */
	protected abstract void generate(Set<String> literals, Set<String> keywords, Set<String> punctuation,
			Set<String> ignored, Set<String> specialKeywords, Set<String> typeDeclarationKeywords);

	/** Write the given lines into the file.
	 *
	 * @param basename the basename of the file.
	 * @param lines the lines to write.
	 */
	protected void writeFile(String basename, Collection<String> lines) {
		// Create the file.
		final String lineDelimiter = getCodeConfig().getLineDelimiter();
		final StringBuilder content = new StringBuilder();
		for (final String line : lines) {
			content.append(line);
			content.append(lineDelimiter);
		}

		// Encode
		final byte[] bytes = content.toString().getBytes(Charset.forName(getCodeConfig().getEncoding()));

		for (final String output : getOutputs()) {
			final File directory = new File(output).getAbsoluteFile();
			try {
				directory.mkdirs();
				final File outputFile = new File(directory, basename);

				Files.write(Paths.get(outputFile.getAbsolutePath()), bytes);
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
		final String name = getGrammar().getName();
		final int index = name.lastIndexOf('.');
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

}
