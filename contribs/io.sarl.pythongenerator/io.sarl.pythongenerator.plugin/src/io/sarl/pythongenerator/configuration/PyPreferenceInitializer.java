/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.pythongenerator.configuration;

import com.google.inject.Singleton;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;

import io.sarl.lang.compiler.extra.IExtraLanguageConversionInitializer;
import io.sarl.lang.ui.compiler.extra.preferences.ExtraLanguagePreferenceAccess;
import io.sarl.pythongenerator.PyGeneratorPlugin;
import io.sarl.pythongenerator.generator.PyInitializers;


/** Initializer for the generator's preferences.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
public class PyPreferenceInitializer implements IPreferenceStoreInitializer {

	@Override
	public void initialize(IPreferenceStoreAccess access) {
		final IPreferenceStore store = access.getWritablePreferenceStore();
		initializeEnable(store);
		initializeTypeConversion(store);
		initializeFeatureNameConversion(store);
	}

	private static void initializeEnable(IPreferenceStore store) {
		final String key = ExtraLanguagePreferenceAccess.getPrefixedKey(PyGeneratorPlugin.PLUGIN_ID,
				ExtraLanguagePreferenceAccess.ENABLED_PROPERTY);
		store.setDefault(key, false);
	}

	private static void initializeTypeConversion(IPreferenceStore store) {
		final IExtraLanguageConversionInitializer tcInitializer = PyInitializers.getTypeConverterInitializer();
		final String preferenceValue = ExtraLanguagePreferenceAccess.toConverterPreferenceValue(tcInitializer);
		final String key = ExtraLanguagePreferenceAccess.getPrefixedKey(PyGeneratorPlugin.PLUGIN_ID,
				ExtraLanguagePreferenceAccess.TYPE_CONVERSION_PROPERTY);
		store.setDefault(key, preferenceValue);
	}

	private static void initializeFeatureNameConversion(IPreferenceStore store) {
		final IExtraLanguageConversionInitializer fnInitializer = PyInitializers.getFeatureNameConverterInitializer();
		final String preferenceValue = ExtraLanguagePreferenceAccess.toConverterPreferenceValue(fnInitializer);
		final String key = ExtraLanguagePreferenceAccess.getPrefixedKey(PyGeneratorPlugin.PLUGIN_ID,
				ExtraLanguagePreferenceAccess.FEATURE_NAME_CONVERSION_PROPERTY);
		store.setDefault(key, preferenceValue);
	}

}
