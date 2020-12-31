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

package io.sarl.lang.extralanguage;

import java.util.Collection;

import org.eclipse.xtext.generator.IOutputConfigurationProvider;

import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorProvider;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageKeywordProvider;
import io.sarl.lang.extralanguage.validator.IExtraLanguageValidatorProvider;

/** Provider of a contribution as an extra-language generator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public interface IExtraLanguageContribution {

	/** Replies if the given identifier corresponds to this contribution.
	 *
	 * @param identifier the identifier to test.
	 * @return {@code true} if the identifier is accepted. {@code false} otherwise.
	 */
	default boolean isAcceptedIdentifier(String identifier) {
		if (identifier != null) {
			final Iterable<String> ids = getIdentifiers();
			if (ids != null) {
				for (final String id : ids) {
					if (identifier.equalsIgnoreCase(id)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/** Replies the identifiers that correspond to this contribution.
	 *
	 * @return the identifiers.
	 */
	default Collection<String> getIdentifiers() {
		return null;
	}

	/** Replies the provider of a generator.
	 *
	 * @return the provider of a extra-language generator.
	 */
	IExtraLanguageGeneratorProvider getGeneratorProvider();

	/** Replies the provider of a validator.
	 *
	 * @return the provider of a extra-language validator.
	 */
	IExtraLanguageValidatorProvider getValidatorProvider();

	/** Replies the provider of an output configuration related to the extra-language generator.
	 *
	 * @return the provider of an output configuration.
	 */
	IOutputConfigurationProvider getOutputConfigurationProvider();

	/** Replies the provider of the extra-language keywords.
	 *
	 * @return the provider of keywords.
	 */
	IExtraLanguageKeywordProvider getKeywordProvider();

}
