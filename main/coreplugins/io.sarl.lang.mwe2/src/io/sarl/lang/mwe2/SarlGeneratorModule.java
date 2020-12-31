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

package io.sarl.lang.mwe2;

import com.google.inject.Binder;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.DefaultGeneratorModule;

import io.sarl.lang.mwe2.codebuilder.config.CodeBuilderConfig;
import io.sarl.lang.mwe2.externalspec.ExternalHighlightingConfig;
import io.sarl.lang.mwe2.keywords.GrammarKeywordAccessConfig;

/**
 * The generation module for SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SarlGeneratorModule extends DefaultGeneratorModule {

	private ExternalHighlightingConfig highlighting = new ExternalHighlightingConfig();

	private CodeBuilderConfig codeBuilderConfig = new CodeBuilderConfig();

	private GrammarKeywordAccessConfig grammarKeywordConfig = new GrammarKeywordAccessConfig();

	/** Configure the injection of the highlighting configuration.
	 *
	 * @param binder the injection binder.
	 */
	public void configureExternalHighlightingConfig(Binder binder) {
		binder.bind(ExternalHighlightingConfig.class).toInstance(this.highlighting);
	}

	/** Set the configuration for the external highlighting tools.
	 *
	 * @param highlighting the configuration.
	 */
	public void setHighlighting(ExternalHighlightingConfig highlighting) {
		if (highlighting != null) {
			this.highlighting = highlighting;
		}
	}

	/** Replies the configuration for the external highlighting tools.
	 *
	 * @return the configuration.
	 */
	@Pure
	public ExternalHighlightingConfig getHighlighting() {
		return this.highlighting;
	}

	/** Configure the injection of the Ecore type creator.
	 *
	 * @param binder the injection binder.
	 */
	public void configureCodeBuilder(Binder binder) {
		binder.bind(CodeBuilderConfig.class).toInstance(this.codeBuilderConfig);
	}

	/** Set the configuration for the Ecore type creator.
	 *
	 * @param config the configuration.
	 */
	public void setCodeBuilder(CodeBuilderConfig config) {
		if (config != null) {
			this.codeBuilderConfig = config;
		}
	}

	/** Replies the configuration for the Ecore type creator.
	 *
	 * @return the configuration.
	 */
	@Pure
	public CodeBuilderConfig getCodeBuilder() {
		return this.codeBuilderConfig;
	}

	/** Configure the injection of the grammar keyword access configuration.
	 *
	 * @param binder the injection binder.
	 */
	public void configureGrammarKeywordAccess(Binder binder) {
		binder.bind(GrammarKeywordAccessConfig.class).toInstance(this.grammarKeywordConfig);
	}

	/** Set the configuration for the grammar keyword access configuration.
	 *
	 * @param config the configuration.
	 */
	public void setGrammarKeywordAccess(GrammarKeywordAccessConfig config) {
		if (config != null) {
			this.grammarKeywordConfig = config;
		}
	}

	/** Replies the configuration for the grammar keyword access configuration.
	 *
	 * @return the configuration.
	 */
	@Pure
	public GrammarKeywordAccessConfig getGrammarKeywordAccess() {
		return this.grammarKeywordConfig;
	}

}
