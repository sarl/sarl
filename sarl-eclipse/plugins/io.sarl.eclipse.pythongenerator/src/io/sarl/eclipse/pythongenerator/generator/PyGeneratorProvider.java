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

package io.sarl.eclipse.pythongenerator.generator;

import com.google.inject.Injector;
import com.google.inject.Singleton;

import io.sarl.eclipse.pythongenerator.configuration.PyGeneratorUiConfigurationProvider;
import io.sarl.lang.pythongenerator.PyGeneratorPlugin;
import io.sarl.lang.pythongenerator.generator.PyGenerator;
import io.sarl.lang.ui.extralanguage.compiler.AbstractExtraLanguageGeneratorProvider;

/** The provider of a generator from SARL to the Python language.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
public class PyGeneratorProvider extends AbstractExtraLanguageGeneratorProvider {

	@Override
	protected PyGenerator createGeneratorInstance(Injector injector) {
		final var generator = injector.getInstance(PyGenerator.class);
		final var configuration = injector.getInstance(PyGeneratorUiConfigurationProvider.class);
		generator.setPyGeneratorConfigurationProvider(configuration);
		return generator;
	}

	@Override
	protected String getPreferenceID() {
		return PyGeneratorPlugin.PREFERENCE_ID;
	}

}
