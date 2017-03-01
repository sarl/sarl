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

package io.sarl.lang.ui.preferences;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;

import io.sarl.lang.generator.GeneratorConfig2;

/** Accessors to the preferences for the SARL builder.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLBuilderPreferenceAccess {

	/**
	 * Preference identifier for generating <code>@Inline</code>.
	 */
	public static final String PREF_GENERATE_INLINE = "io.sarl.builder.generateInlineAnnotation"; //$NON-NLS-1$

	/**
	 * Preference identifier for using the expression interpreter when generating <code>@Inline</code>.
	 */
	public static final String PREF_USE_EXPRESSION_INTERPRETER =
			"io.sarl.builder.useExpressionInterpreterForInlineAnnotation"; //$NON-NLS-1$

	/** Load the generator configuration from the preferences.
	 *
	 * @param generatorConfig the configuration to set up.
	 * @param context the context of the building.
	 */
	@SuppressWarnings("static-method")
	public void loadBuilderPreferences(GeneratorConfig2 generatorConfig, IProject context) {
		final IPreferenceStore preferenceStore = SARLPreferences.getSARLPreferencesFor(context);
		if (preferenceStore != null) {
			if (preferenceStore.contains(PREF_GENERATE_INLINE)) {
				generatorConfig.setGenerateInlineAnnotation(preferenceStore.getBoolean(PREF_GENERATE_INLINE));
			}
			if (generatorConfig.isGenerateInlineAnnotation()
					&& preferenceStore.contains(PREF_USE_EXPRESSION_INTERPRETER)) {
				generatorConfig.setUseExpressionInterpreterForInlineAnnotation(preferenceStore.getBoolean(
						PREF_USE_EXPRESSION_INTERPRETER));
			}
		}
	}

	/** Initializer of the preference store.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Initializer implements IPreferenceStoreInitializer {

		@Override
		public void initialize(IPreferenceStoreAccess preferenceStoreAccess) {
			final IPreferenceStore store = preferenceStoreAccess.getWritablePreferenceStore();
			store.setDefault(PREF_GENERATE_INLINE, true);
			store.setDefault(PREF_USE_EXPRESSION_INTERPRETER, true);
		}

	}

}
