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

package io.sarl.eclipse.pythongenerator.configuration;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.resource.Resource;

import io.sarl.lang.pythongenerator.PyGeneratorPlugin;
import io.sarl.lang.pythongenerator.configuration.PyGeneratorConfiguration;
import io.sarl.lang.pythongenerator.configuration.PyGeneratorConfigurationProvider;
import io.sarl.lang.ui.compiler.ProjectAdapter;
import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;


/** Provider of a configuration for the SARL-to-Python generator.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.pythongenerator 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.pythongenerator
 * @since 0.8
 */
@Singleton
public class PyGeneratorUiConfigurationProvider extends PyGeneratorConfigurationProvider {

	@Inject
	private ExtraLanguagePreferenceAccess preferences;

	@Override
	protected PyGeneratorConfiguration createConfiguration(Resource resource) {
		final var config = super.createConfiguration(resource);
		var project = ProjectAdapter.getProject(resource);
		project = this.preferences.ifSpecificConfiguration(PyGeneratorPlugin.PREFERENCE_ID, project);
		final var store = this.preferences.getPreferenceStore(project);
		PyPreferenceAccess.loadPreferences(config, store);
		return config;
	}

}
