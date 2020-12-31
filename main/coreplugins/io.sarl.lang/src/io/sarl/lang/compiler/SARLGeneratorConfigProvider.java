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

package io.sarl.lang.compiler;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.GeneratorConfigProvider;

/** SARL-specific provider of a generator configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
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
		final ResourceSet resourceSet = EcoreUtil2.getResourceSet(context);
		if (resourceSet != null) {
			final GeneratorConfigProvider.GeneratorConfigAdapter adapter =
					GeneratorConfigProvider.GeneratorConfigAdapter.findInEmfObject(resourceSet);
			if (adapter != null && adapter.getLanguage2GeneratorConfig().containsKey(this.languageId)) {
				return adapter.getLanguage2GeneratorConfig().get(this.languageId);
			}
		}
		// Create the default configuration
		final GeneratorConfig config = createDefaultGeneratorConfig();
		return config;
	}

	/** Invoked for creating the default generator configuration.
	 *
	 * @return the configuration.
	 */
	protected GeneratorConfig createDefaultGeneratorConfig() {
		final GeneratorConfig config = new GeneratorConfig();
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
