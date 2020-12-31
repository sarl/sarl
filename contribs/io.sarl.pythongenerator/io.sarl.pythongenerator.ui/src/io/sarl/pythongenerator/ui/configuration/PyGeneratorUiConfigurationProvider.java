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

package io.sarl.pythongenerator.ui.configuration;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.core.resources.IProject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jface.preference.IPreferenceStore;

import io.sarl.lang.ui.compiler.ProjectAdapter;
import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;
import io.sarl.pythongenerator.generator.PyGeneratorPlugin;
import io.sarl.pythongenerator.generator.configuration.PyGeneratorConfiguration;
import io.sarl.pythongenerator.generator.configuration.PyGeneratorConfigurationProvider;


/** Provider of a configuration for the SARL-to-Python generator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class PyGeneratorUiConfigurationProvider extends PyGeneratorConfigurationProvider {

	@Inject
	private ExtraLanguagePreferenceAccess preferences;

	@Override
	protected PyGeneratorConfiguration createConfiguration(Resource resource) {
		final PyGeneratorConfiguration config = super.createConfiguration(resource);
		IProject project = ProjectAdapter.getProject(resource);
		project = this.preferences.ifSpecificConfiguration(PyGeneratorPlugin.PREFERENCE_ID, project);
		final IPreferenceStore store = this.preferences.getPreferenceStore(project);
		PyPreferenceAccess.loadPreferences(config, store);
		return config;
	}

}
