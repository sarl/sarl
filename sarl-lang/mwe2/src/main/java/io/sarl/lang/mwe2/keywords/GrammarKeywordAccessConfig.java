/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.mwe2.keywords;

import java.util.HashSet;
import java.util.Set;

import com.google.common.base.Strings;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import org.eclipse.xtext.xtext.generator.IGuiceAwareGeneratorComponent;

/**
 * A configuration for the accessor to the grammar keywords.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version mwe2 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid mwe2
 */
public class GrammarKeywordAccessConfig implements IGuiceAwareGeneratorComponent {

	private boolean googleInjectionTypes = true;

	private Set<String> additionalLiterals = new HashSet<>();

	private Set<String> additionalKeywords = new HashSet<>();

	private Set<String> ignoredKeywords = new HashSet<>();

	private String protectionSymbol = "^"; //$NON-NLS-1$

	private boolean isDependencyGrammarInheritance;

	@Override
	public void initialize(Injector injector) {
		//
	}

	/** Change the symbol for protected keywords.
	 *
	 * @param symbol the symbol.
	 */
	public void setKeywordProtectionSymbol(String symbol) {
		if (!Strings.isNullOrEmpty(symbol)) {
			this.protectionSymbol = symbol;
		}
	}

	/** Change the symbol for protected keywords.
	 *
	 * @return the symbol.
	 */
	public String getKeywordProtectionSymbol() {
		return this.protectionSymbol;
	}

	/** Set if the grammar that are included by the base grammar must provides their keywords.
	 *
	 * @param enable {@code true} if the dependency grammar are used.
	 */
	public void setDependencyGrammarInheritance(boolean enable) {
		this.isDependencyGrammarInheritance = enable;
	}

	/** Replies if the grammar that are included by the base grammar must provides their keywords.
	 *
	 * @return {@code true} if the dependency grammar are used.
	 */
	public boolean getDependencyGrammarInheritance() {
		return this.isDependencyGrammarInheritance;
	}

	/** Add a literal to be exhibit into the grammar keyword access.
	 *
	 * @param literal the literal to add.
	 */
	public void addLiteral(String literal) {
		if (!Strings.isNullOrEmpty(literal)) {
			this.additionalLiterals.add(literal);
		}
	}

	/** Replies the manually added literals.
	 *
	 * @return the literals.
	 */
	public Set<String> getLiterals() {
		return this.additionalLiterals;
	}

	/** Add a keyword to be exhibit into the grammar keyword access.
	 *
	 * @param keyword the keyword to add.
	 */
	public void addKeyword(String keyword) {
		if (!Strings.isNullOrEmpty(keyword)) {
			this.additionalKeywords.add(keyword);
		}
	}

	/** Replies the manually added keywords.
	 *
	 * @return the keywords.
	 */
	public Set<String> getKeywords() {
		return this.additionalKeywords;
	}

	/** Ignore a keyword.
	 *
	 * @param keyword the keyword to be ignored.
	 */
	public void addIgnoreKeyword(String keyword) {
		if (!Strings.isNullOrEmpty(keyword)) {
			this.ignoredKeywords.add(keyword);
		}
	}

	/** Replies the ignored keywords.
	 *
	 * @return the ignored keywords.
	 */
	public Set<String> getIgnoredKeywords() {
		return this.ignoredKeywords;
	}


	/** Replies if the types for injection must be from the Google Guice.
	 *
	 * @return {@code true} if the injection types are from Google Guice, otherwise {@code false}.
	 * @since 0.14
	 */
	public boolean getGoolgeInjectionTypes() {
		return this.googleInjectionTypes;
	}

	/** Replies if the types for injection must be from the Google Guice.
	 *
	 * @param isGoogle {@code true} if the injection types are from Google Guice, otherwise {@code false}.
	 * @since 0.14
	 */
	public void setGoolgeInjectionTypes(boolean isGoogle) {
		this.googleInjectionTypes = isGoogle;
	}

	/** Replies the type for {@code @Inject}.
	 *
	 * @return the inject annotation type.
	 * @since 0.14
	 */
	public Class<?> getInjectType() {
		if (getGoolgeInjectionTypes()) {
			return Inject.class;
		}
		return javax.inject.Inject.class;
	}

	/** Replies the type for {@code @Named}.
	 *
	 * @return the named annotation type.
	 * @since 0.14
	 */
	public Class<?> getNamedType() {
		if (getGoolgeInjectionTypes()) {
			return Named.class;
		}
		return javax.inject.Named.class;
	}

	/** Replies the type for {@code @Provider}.
	 *
	 * @return the provider annotation type.
	 * @since 0.14
	 */
	public Class<?> getProviderType() {
		if (getGoolgeInjectionTypes()) {
			return Provider.class;
		}
		return javax.inject.Provider.class;
	}

	/** Replies the type for {@code @Singleton}.
	 *
	 * @return the singleton annotation type.
	 * @since 0.14
	 */
	public Class<?> getSingletonType() {
		if (getGoolgeInjectionTypes()) {
			return Singleton.class;
		}
		return javax.inject.Singleton.class;
	}

}
