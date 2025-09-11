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

package io.sarl.lang.mwe2.keywords;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import com.google.common.base.Strings;

import io.sarl.lang.mwe2.inject.InjectionAPI;

/**
 * A configuration for the accessor to the grammar keywords.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class GrammarKeywordAccessConfig extends GrammarKeywordAccessKeywordConfig {

	private InjectionAPI injectionAPI = InjectionAPI.getDefault();

	private String protectionSymbol = "^"; //$NON-NLS-1$

	private boolean isDependencyGrammarInheritance;

	private boolean removeKeywordsDefinedInScopes = true;

	private final List<NamedGrammarKeywordAccessConfig> scopes = new ArrayList<>();

	/** Replies the configurations that are associated to named scopes.
	 *
	 * @return the configurations.
	 * @since 0.15
	 */
	public List<NamedGrammarKeywordAccessConfig> getScopes() {
		return this.scopes;
	}

	/** Add the configurations that are associated to named scopes.
	 *
	 * @param configuration the new configurations.
	 * @since 0.15
	 */
	public void addScope(NamedGrammarKeywordAccessConfig configuration) {
		if (configuration != null) {
			this.scopes.add(configuration);
		}
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

	/** Replies the injection API to be considered in the generated code.
	 *
	 * @return the injection API.
	 * @since 0.15
	 */
	public InjectionAPI getInjectionAPI() {
		return this.injectionAPI;
	}

	/** Change the injection API to be considered in the generated code.
	 *
	 * @param api the injection API. If it is {@code null}, the value replied by {@link InjectionAPI#getDeclaringClass()} is assumed.
	 * @since 0.15
	 */
	public void setInjectionAPI(InjectionAPI api) {
		if (api == null) {
			this.injectionAPI = InjectionAPI.getDefault();
		} else {
			this.injectionAPI = api;
		}
	}

	/** Replies if the types for injection must be from the Google Guice.
	 *
	 * @return {@code true} if the injection types are from Google Guice, otherwise {@code false}.
	 * @since 0.14
	 * @deprecated Replaced by {@link #getInjectionAPI()}.
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public boolean getGoogleInjectionTypes() {
		return this.injectionAPI == InjectionAPI.GOOGLE_GUICE;
	}

	/** Replies if the types for injection must be from the Google Guice.
	 *
	 * @param isGoogle {@code true} if the injection types are from Google Guice, otherwise {@code false}.
	 * @since 0.14
	 * @deprecated Replaced by {@link #setInjectionAPI(InjectionAPI)}.
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public void setGoogleInjectionTypes(boolean isGoogle) {
		this.injectionAPI = isGoogle ? InjectionAPI.GOOGLE_GUICE : InjectionAPI.getDefault();
	}

	/** Replies the type for {@code @Inject}.
	 *
	 * @return the inject annotation type.
	 * @since 0.14
	 * @deprecated Replaced by {@link InjectionAPI#getInjectType()}
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public Class<?> getInjectType() {
		return getInjectionAPI().getInjectType();
	}

	/** Replies the type for {@code @Named}.
	 *
	 * @return the named annotation type.
	 * @since 0.14
	 * @deprecated Replaced by {@link InjectionAPI#getInjectType()}
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public Class<?> getNamedType() {
		return getInjectionAPI().getNamedType();
	}

	/** Replies the type for {@code @Provider}.
	 *
	 * @return the provider annotation type.
	 * @since 0.14
	 * @deprecated Replaced by {@link InjectionAPI#getInjectType()}
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public Class<?> getProviderType() {
		return getInjectionAPI().getProviderType();
	}

	/** Replies the type for {@code @Singleton}.
	 *
	 * @return the singleton annotation type.
	 * @since 0.14
	 * @deprecated Replaced by {@link InjectionAPI#getInjectType()}
	 */
	@Deprecated(since = "0.15", forRemoval = true)
	public Class<?> getSingletonType() {
		return getInjectionAPI().getSingletonType();
	}

	/** Replies if the keywords defined in a scope are removed from the root accessor.
	 *
	 * @return {@code true} if keywords are removed.
	 * @since 0.15
	 */
	public boolean getRemoveKeywordsDefinedInScopes() {
		return this.removeKeywordsDefinedInScopes;
	}

	/** Change if the keywords defined in a scope are removed from the root accessor.
	 *
	 * @param remove {@code true} if keywords are removed.
	 * @since 0.15
	 */
	public void setRemoveKeywordsDefinedInScopes(boolean remove) {
		this.removeKeywordsDefinedInScopes = remove;
	}

	/** Replies all the keywords that are defined in a scope.
	 *
	 * @return the scoped keywords.
	 * @since 0.15
	 */
	public Set<String> getAllKeywordsDefinedInScope() {
		final var all = new TreeSet<String>();
		for (final var scope : getScopes()) {
			all.addAll(scope.getKeywords());
		}
		return all;
	}

}
