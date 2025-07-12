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

package io.sarl.bspl.lang.mwe2;

import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xtext.generator.DefaultGeneratorModule;

import com.google.inject.Binder;

import io.sarl.lang.mwe2.keywords.GrammarKeywordAccessConfig;

/**
 * The generation module for SARL-BSPL.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class BsplGeneratorModule extends DefaultGeneratorModule {

	private GrammarKeywordAccessConfig grammarKeywordConfig = new GrammarKeywordAccessConfig();

	/** Configure the injection of the grammar keyword access configuration.
	 *
	 * @param binder the injection binder.
	 */
	public void configureGrammarKeywordAccess(Binder binder) {
		binder.bind(GrammarKeywordAccessConfig.class).toInstance(this.grammarKeywordConfig);
	}

	/** Set the configuration for the grammar keyword access.
	 *
	 * @param config the configuration.
	 */
	public void setGrammarKeywordAccess(GrammarKeywordAccessConfig config) {
		if (config != null) {
			this.grammarKeywordConfig = config;
		}
	}

	/** Replies the configuration for the grammar keyword access.
	 *
	 * @return the configuration.
	 */
	@Pure
	public GrammarKeywordAccessConfig getGrammarKeywordAccess() {
		return this.grammarKeywordConfig;
	}

}
