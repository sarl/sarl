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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.xpand2.XpandExecutionContext;
import org.eclipse.xtext.Grammar;
import org.eclipse.xtext.generator.DefaultGeneratorFragment;
import org.eclipse.xtext.generator.IGeneratorFragment;
import org.eclipse.xtext.util.Strings;

/**
 * A {@link IGeneratorFragment} that enables to create the language specification
 * for external tools.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ExternalLanguageSpecificationFragment extends DefaultGeneratorFragment
		implements ExternalLanguageSpecificationContext {

	/** Default language name.
	 */
	public static final String DEFAULT_LANGUAGE = "sarl"; //$NON-NLS-1$

	/** Add the native types by default.
	 */
	public static final boolean DEFAULT_ADD_NATIVE_TYPES = false;

	private final List<ExternalLanguageSpecificationGenerator> generators = new ArrayList<>();

	private final Set<String> outputDirectories = new TreeSet<>();

	private final Set<String> additionalLiterals = new TreeSet<>();

	private final Set<String> additionalKeywords = new TreeSet<>();

	private final Set<String> excludedKeywords = new TreeSet<>();

	private final Set<String> additionalPunctuation = new TreeSet<>();

	private String language;

	private boolean addNativeTypes = DEFAULT_ADD_NATIVE_TYPES;

	@Override
	public void addGenerator(ExternalLanguageSpecificationGenerator generator) {
		if (generator != null) {
			this.generators.add(generator);
			generator.setContext(this);
		}
	}

	@Override
	public void setLanguage(String name) {
		if (!Strings.isEmpty(name)) {
			this.language = name;
		}
	}

	@Override
	public String getLanguage() {
		if (Strings.isEmpty(this.language)) {
			return DEFAULT_LANGUAGE;
		}
		return this.language;
	}

	@Override
	public void setAddNativeTypes(boolean addNativeTypes) {
		this.addNativeTypes = addNativeTypes;
	}

	@Override
	public boolean getAddNativeTypes() {
		return this.addNativeTypes;
	}

	@Override
	public void addOutput(String directory) {
		if (!Strings.isEmpty(directory)) {
			this.outputDirectories.add(directory);
		}
	}

	@Override
	public Set<String> getOutputs() {
		return this.outputDirectories;
	}

	@Override
	public void addLiteral(String literal) {
		if (!Strings.isEmpty(literal)) {
			this.additionalLiterals.add(literal);
		}
	}

	@Override
	public Set<String> getLiterals() {
		return this.additionalLiterals;
	}

	@Override
	public void addKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.additionalKeywords.add(keyword);
		}
	}

	@Override
	public Set<String> getKeywords() {
		return this.additionalKeywords;
	}

	@Override
	public void addIgnoreKeyword(String keyword) {
		if (!Strings.isEmpty(keyword)) {
			this.excludedKeywords.add(keyword);
		}
	}

	@Override
	public Set<String> getIgnoredKeywords() {
		return this.excludedKeywords;
	}

	@Override
	public void addPunctuation(String symbol) {
		if (!Strings.isEmpty(symbol)) {
			this.additionalPunctuation.add(symbol);
		}
	}

	@Override
	public Set<String> getPunctuation() {
		return this.additionalPunctuation;
	}

	@Override
	public void generate(Grammar grammar, XpandExecutionContext ctx) {
		for (ExternalLanguageSpecificationGenerator generator : this.generators) {
			generator.generate(grammar);
		}
	}

}
