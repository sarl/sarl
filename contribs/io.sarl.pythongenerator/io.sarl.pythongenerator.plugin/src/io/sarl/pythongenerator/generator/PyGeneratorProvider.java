/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.pythongenerator.generator;

import javax.inject.Singleton;

import com.google.inject.Injector;

import io.sarl.lang.ui.compiler.extra.AbstractExtraLanguageGeneratorProvider;
import io.sarl.pythongenerator.PyGeneratorPlugin;

/** Provider the Python generator if is it enabled.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
public class PyGeneratorProvider extends AbstractExtraLanguageGeneratorProvider<PyGenerator> {

	@Override
	protected PyGenerator createGeneratorInstance(Injector injector) {
		return injector.getInstance(PyGenerator.class);
	}

	@Override
	protected String getPluginID() {
		return PyGeneratorPlugin.PLUGIN_ID;
	}

}
