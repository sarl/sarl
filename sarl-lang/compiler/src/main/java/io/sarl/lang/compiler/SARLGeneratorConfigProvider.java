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

package io.sarl.lang.compiler;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.GeneratorConfigProvider;

/** SARL-specific provider of a generator configuration.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.4
 */
public class SARLGeneratorConfigProvider extends GeneratorConfigProvider {

	@Inject
	@Named(Constants.LANGUAGE_NAME)
	private String languageId;

	private JavaVersion defaultVersion;

	@Override
	public GeneratorConfig get(EObject context) {
		// Search for the Eclipse configuration
		final var resourceSet = EcoreUtil2.getResourceSet(context);
		if (resourceSet != null) {
			final var adapter = GeneratorConfigProvider.GeneratorConfigAdapter.findInEmfObject(resourceSet);
			if (adapter != null && adapter.getLanguage2GeneratorConfig().containsKey(this.languageId)) {
				return adapter.getLanguage2GeneratorConfig().get(this.languageId);
			}
		}
		// Create the default configuration
		final var config = createDefaultGeneratorConfig();
		return config;
	}

	/** Invoked for creating the default generator configuration.
	 *
	 * @return the configuration.
	 */
	protected GeneratorConfig createDefaultGeneratorConfig() {
		final var config = new GeneratorConfig();
		if (this.defaultVersion == null) {
			this.defaultVersion = JavaVersion.fromQualifier(System.getProperty("java.specification.version")); //$NON-NLS-1$
			if (this.defaultVersion != null) {
				config.setJavaSourceVersion(this.defaultVersion);
			}
		} else {
			config.setJavaSourceVersion(this.defaultVersion);
		}
		return config;
	}

}
