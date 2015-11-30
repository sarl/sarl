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

package io.sarl.lang.mwe2.external;

import java.lang.ref.WeakReference;
import java.text.MessageFormat;
import java.util.ArrayDeque;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.AbstractRule;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.common.types.access.impl.Primitives;
import org.eclipse.xtext.util.Strings;

/**
 * An abstract generator of a external language specification.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class ExternalLanguageSpecificationGenerator {

	/** Indicates if the current generator ignores the parent's values
	 * when the value is defined for this generator.
	 * If not the parent's values are inherited.
	 */
	public static final boolean DEFAULT_OVERRIDE_PARENT_VALUES = false;

	private static final Pattern KEYWORD_PATTERN = Pattern.compile("^[a-zA-Z]{2,}$"); //$NON-NLS-1$

	@SuppressWarnings("checkstyle:linelength")
	private static final Pattern PUNCTUATION_PATTERN = Pattern.compile("^[!#%&()*/+,\\-:;<=>?@\\[\\\\\\]^{|}~.%]+$"); //$NON-NLS-1$

	/** Logger for this generator.
	 */
	protected final Logger log;

	private final Set<String> outputDirectories = new TreeSet<>();

	private boolean overrideOutputDirectories = DEFAULT_OVERRIDE_PARENT_VALUES;

	private final Set<String> additionalLiterals = new TreeSet<>();

	private boolean overrideAdditionalLiterals = DEFAULT_OVERRIDE_PARENT_VALUES;

	private final Set<String> additionalKeywords = new TreeSet<>();

	private boolean overrideAdditionalKeywords = DEFAULT_OVERRIDE_PARENT_VALUES;

	private final Set<String> excludedKeywords = new TreeSet<>();

	private boolean overrideExcludedKeywords = DEFAULT_OVERRIDE_PARENT_VALUES;

	private final Set<String> additionalPunctuation = new TreeSet<>();

	private boolean overrideAdditionalPunctuation = DEFAULT_OVERRIDE_PARENT_VALUES;

	private String language;

	private Boolean addNativeTypes;

	private WeakReference<ExternalLanguageSpecificationContext> context;

	/** Construct a generator.
	 */
	public ExternalLanguageSpecificationGenerator() {
		this.log = Logger.getLogger(getClass());
	}

	/** Set the parent fragment.
	 *
	 * @param fragment the fragment.
	 */
	void setContext(ExternalLanguageSpecificationContext fragment) {
		if (fragment == null) {
			this.context = null;
		} else {
			this.context = new WeakReference<>(fragment);
		}
	}

	private ExternalLanguageSpecificationContext getContext() {
		return this.context == null ? null : this.context.get();
	}

	/** Add a directory into which the external specification must be written.
	 *
	 * @param directory the output directory.
	 */
	public void addOutput(String directory) {
		if (!Strings.isEmpty(directory)) {
			this.outputDirectories.add(directory);
		}
	}

	/** Replies the output directories.
	 *
	 * @return the directories.
	 */
	public Set<String> getOutputs() {
		ExternalLanguageSpecificationContext parent = getContext();
		if (this.outputDirectories.isEmpty()) {
			if (parent != null) {
				return parent.getOutputs();
			}
		} else if (parent != null && !this.overrideOutputDirectories) {
			Set<String> parentValue = parent.getOutputs();
			if (parentValue != null && !parentValue.isEmpty()) {
				Set<String> values = new TreeSet<>(parentValue);
				values.addAll(this.outputDirectories);
				return values;
			}
		}
		return this.outputDirectories;
	}

	/** Indicates if the output folders from the parent fragment are ignored or not
	 * if a local value is defined.
	 *
	 * @param override <code>true</code> if the parent's values are ignored, <code>false</code>
	 *     to always use them.
	 */
	public void setOverrideOutputs(boolean override) {
		this.overrideOutputDirectories = override;
	}

	/** Set the name of the language.
	 *
	 * @param name the language name.
	 */
	public void setLanguage(String name) {
		if (!Strings.isEmpty(name)) {
			this.language = name;
		}
	}

	/** Replies the name of the language.
	 *
	 * @return the language name.
	 */
	public String getLanguage() {
		if (Strings.isEmpty(this.language)) {
			ExternalLanguageSpecificationContext parent = getContext();
			if (parent != null) {
				return parent.getLanguage();
			}
			return ExternalLanguageSpecificationFragment.DEFAULT_LANGUAGE;
		}
		return this.language;
	}

	/** Indicates if the native types must be added in the keyword list.
	 *
	 * @param addNativeTypes <code>true</code> for adding the native types.
	 */
	public void setAddNativeTypes(boolean addNativeTypes) {
		this.addNativeTypes = addNativeTypes;
	}

	/** Replies if the native types must be added in the keyword list.
	 *
	 * @return <code>true</code> for adding the native types.
	 */
	public boolean getAddNativeTypes() {
		if (this.addNativeTypes == null) {
			ExternalLanguageSpecificationContext parent = getContext();
			if (parent != null) {
				return parent.getAddNativeTypes();
			}
			return ExternalLanguageSpecificationFragment.DEFAULT_ADD_NATIVE_TYPES;
		}
		return this.addNativeTypes.booleanValue();
	}

	/** Add a literal that is not inside the SARL grammar.
	 *
	 * @param literal the additional literal.
	 */
	public void addLiteral(String literal) {
		if (!Strings.isEmpty(literal)) {
			this.additionalLiterals.add(literal);
		}
	}

	/** Replies the literals that are not inside the SARL grammar.
	 *
	 * @return the additional literal.
	 */
	public Set<String> getLiterals() {
		ExternalLanguageSpecificationContext parent = getContext();
		if (this.additionalLiterals.isEmpty()) {
			if (parent != null) {
				return parent.getLiterals();
			}
		} else if (parent != null && !this.overrideAdditionalLiterals) {
			Set<String> parentValue = parent.getLiterals();
			if (parentValue != null && !parentValue.isEmpty()) {
				Set<String> values = new TreeSet<>(parentValue);
				values.addAll(this.additionalLiterals);
				return values;
			}
		}
		return this.additionalLiterals;
	}

	/** Indicates if the additional literals from the parent fragment are ignored or not
	 * if a local value is defined.
	 *
	 * @param override <code>true</code> if the parent's values are ignored, <code>false</code>
	 *     to always use them.
	 */
	public void setOverrideLiterals(boolean override) {
		this.overrideAdditionalLiterals = override;
	}

	/** Add a keyword that is not inside the SARL grammar.
	 *
	 * @param keyword the additional keyword.
	 */
	public void addKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.additionalKeywords.add(keyword);
		}
	}

	/** Replies the keywords that are not inside the SARL grammar.
	 *
	 * @return the additional keywords.
	 */
	public Set<String> getKeywords() {
		ExternalLanguageSpecificationContext parent = getContext();
		if (this.additionalKeywords.isEmpty()) {
			if (parent != null) {
				return parent.getKeywords();
			}
		} else if (parent != null && !this.overrideAdditionalKeywords) {
			Set<String> parentValue = parent.getKeywords();
			if (parentValue != null && !parentValue.isEmpty()) {
				Set<String> values = new TreeSet<>(parentValue);
				values.addAll(this.additionalKeywords);
				return values;
			}
		}
		return this.additionalKeywords;
	}

	/** Indicates if the additional keywords from the parent fragment are ignored or not
	 * if a local value is defined.
	 *
	 * @param override <code>true</code> if the parent's values are ignored, <code>false</code>
	 *     to always use them.
	 */
	public void setOverrideKeywords(boolean override) {
		this.overrideAdditionalKeywords = override;
	}

	/** Ignore a keyword to ignore.
	 *
	 * @param keyword the keyword to ignore.
	 */
	public void addIgnoreKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.excludedKeywords.add(keyword);
		}
	}

	/** Replies the keywords to ignore.
	 *
	 * @return the keywords to ignore.
	 */
	public Set<String> getIgnoredKeywords() {
		ExternalLanguageSpecificationContext parent = getContext();
		if (this.excludedKeywords.isEmpty()) {
			if (parent != null) {
				return parent.getIgnoredKeywords();
			}
		} else if (parent != null && !this.overrideExcludedKeywords) {
			Set<String> parentValue = parent.getIgnoredKeywords();
			if (parentValue != null && !parentValue.isEmpty()) {
				Set<String> values = new TreeSet<>(parentValue);
				values.addAll(this.excludedKeywords);
				return values;
			}
		}
		return this.excludedKeywords;
	}

	/** Indicates if the ignored keywords from the parent fragment are ignored or not
	 * if a local value is defined.
	 *
	 * @param override <code>true</code> if the parent's values are ignored, <code>false</code>
	 *     to always use them.
	 */
	public void setOverrideIgnoredKeywords(boolean override) {
		this.overrideExcludedKeywords = override;
	}

	/** Add a punctuation symbol that is not inside the SARL grammar.
	 *
	 * @param symbol the additional punctuation symbol.
	 */
	public void addPunctuation(String symbol) {
		if (!Strings.isEmpty(symbol)) {
			this.additionalPunctuation.add(symbol);
		}
	}

	/** Replies the punctuation symbols that are not inside the SARL grammar.
	 *
	 * @return the additional punctuation symbols.
	 */
	public Set<String> getPunctuation() {
		ExternalLanguageSpecificationContext parent = getContext();
		if (this.additionalPunctuation.isEmpty()) {
			if (parent != null) {
				return parent.getPunctuation();
			}
		} else if (parent != null && !this.overrideAdditionalPunctuation) {
			Set<String> parentValue = parent.getPunctuation();
			if (parentValue != null && !parentValue.isEmpty()) {
				Set<String> values = new TreeSet<>(parentValue);
				values.addAll(this.additionalPunctuation);
				return values;
			}
		}
		return this.additionalPunctuation;
	}

	/** Indicates if the additional punctuation symbols from the parent fragment are ignored or not
	 * if a local value is defined.
	 *
	 * @param override <code>true</code> if the parent's values are ignored, <code>false</code>
	 *     to always use them.
	 */
	public void setOverridePunctuation(boolean override) {
		this.overrideAdditionalPunctuation = override;
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

	/** Replies the name of the language specification to generate.
	 *
	 * @return the human-readable name of the specification.
	 */
	protected abstract String getHumanReadableSpecificationName();

	/** Generate the external language specification from the given grammar.
	 *
	 * @param grammar the SARL grammar.
	 */
	public void generate(Grammar grammar) {
		this.log.info(MessageFormat.format("Generating the {0}", getHumanReadableSpecificationName())); //$NON-NLS-1$

		if (grammar == null) {
			throw new RuntimeException("No grammar defined"); //$NON-NLS-1$
		}

		Set<String> literals = new TreeSet<>();
		Set<String> keywords = new TreeSet<>();

		literals.addAll(getLiterals());

		for (String keyword : getKeywords()) {
			if (!literals.contains(keyword)) {
				keywords.add(keyword);
			}
		}

		if (getAddNativeTypes()) {
			for (Class<?> type : Primitives.ALL_PRIMITIVE_TYPES) {
				keywords.add(type.getSimpleName());
			}
		}

		Set<String> punctuation = new TreeSet<>();
		punctuation.addAll(getPunctuation());

		Queue<Grammar> grammars = new ArrayDeque<>();
		grammars.add(grammar);

		Set<String> excluded = getIgnoredKeywords();

		Set<String> ignored = new TreeSet<>();

		while (!grammars.isEmpty()) {
			Grammar grammarToTreat = grammars.poll();
			grammars.addAll(grammarToTreat.getUsedGrammars());
			exploreGrammar(grammarToTreat, keywords, punctuation, literals, excluded, ignored);
		}

		Set<String> tmp = new TreeSet<>(excluded);
		tmp.removeAll(ignored);
		if (!tmp.isEmpty()) {
			throw new RuntimeException(MessageFormat.format(
					"The following keywords cannot be ignored because they are not defined in the grammars: {0}", //$NON-NLS-1$
					toString(tmp)));
		}

		generate(literals, keywords, punctuation, ignored);
	}

	/** Generate the external specification.
	 *
	 * @param literals - the SARL literals.
	 * @param keywords - the SARL keywords.
	 * @param punctuation - the SARL punctuation symbols.
	 * @param ignored - the ignored literals (mostly for information).
	 */
	protected abstract void generate(Set<String> literals, Set<String> keywords, Set<String> punctuation, Set<String> ignored);

	/** Format the given iterable.
	 *
	 * @param list the iterable to format.
	 * @return the formatted representation of the iterable object.
	 */
	protected static String toString(Iterable<?> list) {
		return toString(list, true);
	}

	/** Format the given iterable.
	 *
	 * @param list the iterable to format.
	 * @param addSpaceAfterComa <code>true</code> for adding a space after the coma, <code>false</code> if not.
	 * @return the formatted representation of the iterable object.
	 */
	protected static String toString(Iterable<?> list, boolean addSpaceAfterComa) {
		StringBuilder builder = new StringBuilder();
		for (Object o : list) {
			if (builder.length() > 0) {
				builder.append(","); //$NON-NLS-1$
				if (addSpaceAfterComa) {
					builder.append(" "); //$NON-NLS-1$
				}
			}
			builder.append(o.toString());
		}
		return builder.toString();
	}

}

