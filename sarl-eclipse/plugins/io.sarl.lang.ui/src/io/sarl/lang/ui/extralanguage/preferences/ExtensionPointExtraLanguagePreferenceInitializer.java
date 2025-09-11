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

package io.sarl.lang.ui.extralanguage.preferences;

import java.util.List;
import java.util.stream.Collectors;

import com.google.inject.Singleton;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;

import io.sarl.apputils.eclipseextensions.Extensions;
import io.sarl.lang.ui.SARLUiConfig;


/** Provide the output configuration from the SARL code and the extra languages.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.6
 */
@Singleton
public class ExtensionPointExtraLanguagePreferenceInitializer implements IPreferenceStoreInitializer {

	private static final String EXTENSION_POINT_PREFERENCE_INITIALIZER_ATTRIBUTE = "preferences"; //$NON-NLS-1$

	private List<IPreferenceStoreInitializer> initializers;

	@Override
	public void initialize(IPreferenceStoreAccess access) {
		if (this.initializers == null) {
			this.initializers = Extensions.getExtensions(
					SARLUiConfig.NAMESPACE, SARLUiConfig.EXTENSION_POINT_EXTRA_LANGUAGE_GENERATORS,
					EXTENSION_POINT_PREFERENCE_INITIALIZER_ATTRIBUTE,
					IPreferenceStoreInitializer.class)
				.collect(Collectors.toList());
		}
		for (final var initializer : this.initializers) {
			initializer.initialize(access);
		}
	}

}
