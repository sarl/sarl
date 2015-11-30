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

import java.util.Set;

/**
 * A container of {@link ExternalLanguageSpecificationGenerator}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ExternalLanguageSpecificationContext {

	/** Add the given generator of a external language specification.
	 *
	 * @param generator the generator.
	 */
	void addGenerator(ExternalLanguageSpecificationGenerator generator);

	/** Set the name of the language.
	 *
	 * @param name the language name.
	 */
	void setLanguage(String name);

	/** Replies the name of the language.
	 *
	 * @return the language name.
	 */
	String getLanguage();

	/** Indicates if the native types must be added in the keyword list.
	 *
	 * @param addNativeTypes <code>true</code> for adding the native types.
	 */
	void setAddNativeTypes(boolean addNativeTypes);

	/** Replies if the native types must be added in the keyword list.
	 *
	 * @return <code>true</code> for adding the native types.
	 */
	boolean getAddNativeTypes();

	/** Add a directory into which the external specifications must be written.
	 *
	 * @param directory the output directory.
	 */
	void addOutput(String directory);

	/** Replies the output directories.
	 *
	 * @return the directories.
	 */
	Set<String> getOutputs();


	/** Add a literal that is not inside the SARL grammar.
	 *
	 * @param literal the additional literal.
	 */
	void addLiteral(String literal);

	/** Replies the literals that are not inside the SARL grammar.
	 *
	 * @return the additional literals.
	 */
	Set<String> getLiterals();

	/** Add a keyword that is not inside the SARL grammar.
	 *
	 * @param keyword the additional keyword.
	 */
	void addKeyword(String keyword);

	/** Replies the keywords that are not inside the SARL grammar.
	 *
	 * @return the additional keywords.
	 */
	Set<String> getKeywords();

	/** Ignore a keyword to ignore.
	 *
	 * @param keyword the keyword to ignore.
	 */
	void addIgnoreKeyword(String keyword);

	/** Replies the keywords to ignore.
	 *
	 * @return the keywords to ignore.
	 */
	Set<String> getIgnoredKeywords();

	/** Add a punctuation symbol that is not inside the SARL grammar.
	 *
	 * @param symbol the additional punctuation symbol.
	 */
	void addPunctuation(String symbol);

	/** Replies the punctuation symbols that are not inside the SARL grammar.
	 *
	 * @return the additional punctuation symbols.
	 */
	Set<String> getPunctuation();

}
