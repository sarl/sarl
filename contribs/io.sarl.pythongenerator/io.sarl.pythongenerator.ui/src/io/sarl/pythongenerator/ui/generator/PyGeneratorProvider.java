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

package io.sarl.pythongenerator.ui.generator;

import com.google.inject.Injector;
import com.google.inject.Singleton;

import io.sarl.lang.ui.extralanguage.compiler.AbstractExtraLanguageGeneratorProvider;
import io.sarl.pythongenerator.generator.PyGeneratorPlugin;
import io.sarl.pythongenerator.generator.configuration.IPyGeneratorConfigurationProvider;
import io.sarl.pythongenerator.generator.generator.PyGenerator;
import io.sarl.pythongenerator.ui.configuration.PyGeneratorUiConfigurationProvider;

/** The provider of a generator from SARL to the Python language.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
public class PyGeneratorProvider extends AbstractExtraLanguageGeneratorProvider {

	@Override
	protected PyGenerator createGeneratorInstance(Injector injector) {
		final PyGenerator generator = injector.getInstance(PyGenerator.class);
		final IPyGeneratorConfigurationProvider configuration = injector.getInstance(PyGeneratorUiConfigurationProvider.class);
		generator.setPyGeneratorConfigurationProvider(configuration);
		return generator;
	}

	@Override
	protected String getPreferenceID() {
		return PyGeneratorPlugin.PREFERENCE_ID;
	}

}
