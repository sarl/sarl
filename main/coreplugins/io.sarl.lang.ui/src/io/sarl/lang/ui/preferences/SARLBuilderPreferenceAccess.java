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

package io.sarl.lang.ui.preferences;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;

import io.sarl.lang.SARLConfig;
import io.sarl.lang.compiler.GeneratorConfig2;

/** Accessors to the preferences for the SARL builder.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLBuilderPreferenceAccess {

	/**
	 * Preference identifier for the folder name in which unit test sources are generated.
	 * @since 0.8
	 */
	public static final String PREF_GENERATED_TEST_SOURCE_FOLDER = "io.sarl.builder.generatedTestSourceFolder"; //$NON-NLS-1$

	/**
	 * Preference identifier for generating <code>@Inline</code>.
	 */
	public static final String PREF_GENERATE_INLINE = "io.sarl.builder.generateInlineAnnotation"; //$NON-NLS-1$

	/**
	 * Preference identifier for using the expression interpreter when generating <code>@Inline</code>.
	 */
	public static final String PREF_USE_EXPRESSION_INTERPRETER =
			"io.sarl.builder.useExpressionInterpreterForInlineAnnotation"; //$NON-NLS-1$

	/**
	 * Preference identifier for generating <code>@Pure</code>.
	 */
	public static final String PREF_GENERATE_PURE = "io.sarl.builder.generatePureAnnotation"; //$NON-NLS-1$

	/**
	 * Preference identifier for generating the equality test functions.
	 */
	public static final String PREF_GENERATE_EQUALITY_TEST_FUNCTIONS = "io.sarl.builder.generateEqualityTestFunctions"; //$NON-NLS-1$

	/**
	 * Preference identifier for generating the toString functions.
	 * @since 0.8
	 */
	public static final String PREF_GENERATE_TOSTRING_FUNCTIONS = "io.sarl.builder.generateToStringFunctions"; //$NON-NLS-1$

	/**
	 * Preference identifier for generating the clone  functions.
	 * @since 0.8
	 */
	public static final String PREF_GENERATE_CLONE_FUNCTIONS = "io.sarl.builder.generateCloneFunctions"; //$NON-NLS-1$

	/**
	 * Preference identifier for generating the serial number fields.
	 * @since 0.8
	 */
	public static final String PREF_GENERATE_SERIAL_NUMBER_FIELDS = "io.sarl.builder.generateSerialNumberFields"; //$NON-NLS-1$

	/** Load the generator configuration from the preferences.
	 *
	 * @param generatorConfig the configuration to set up.
	 * @param context the context of the building.
	 */
	@SuppressWarnings({"static-method", "checkstyle:npathcomplexity"})
	public void loadBuilderPreferences(GeneratorConfig2 generatorConfig, IProject context) {
		final IPreferenceStore preferenceStore = SARLPreferences.getSARLPreferencesFor(context);
		if (preferenceStore != null) {
			if (preferenceStore.contains(PREF_GENERATED_TEST_SOURCE_FOLDER)) {
				generatorConfig.setGeneratedTestSourceFolder(preferenceStore.getString(PREF_GENERATED_TEST_SOURCE_FOLDER));
			}
			if (preferenceStore.contains(PREF_GENERATE_INLINE)) {
				generatorConfig.setGenerateInlineAnnotation(preferenceStore.getBoolean(PREF_GENERATE_INLINE));
			}
			if (generatorConfig.isGenerateInlineAnnotation()
					&& preferenceStore.contains(PREF_USE_EXPRESSION_INTERPRETER)) {
				generatorConfig.setUseExpressionInterpreterForInlineAnnotation(preferenceStore.getBoolean(
						PREF_USE_EXPRESSION_INTERPRETER));
			}
			if (preferenceStore.contains(PREF_GENERATE_PURE)) {
				generatorConfig.setGeneratePureAnnotation(preferenceStore.getBoolean(PREF_GENERATE_PURE));
			}
			if (preferenceStore.contains(PREF_GENERATE_EQUALITY_TEST_FUNCTIONS)) {
				generatorConfig.setGenerateEqualityTestFunctions(preferenceStore.getBoolean(PREF_GENERATE_EQUALITY_TEST_FUNCTIONS));
			}
			if (preferenceStore.contains(PREF_GENERATE_TOSTRING_FUNCTIONS)) {
				generatorConfig.setGenerateToStringFunctions(preferenceStore.getBoolean(PREF_GENERATE_TOSTRING_FUNCTIONS));
			}
			if (preferenceStore.contains(PREF_GENERATE_CLONE_FUNCTIONS)) {
				generatorConfig.setGenerateCloneFunctions(preferenceStore.getBoolean(PREF_GENERATE_CLONE_FUNCTIONS));
			}
			if (preferenceStore.contains(PREF_GENERATE_SERIAL_NUMBER_FIELDS)) {
				generatorConfig.setGenerateSerialNumberFields(preferenceStore.getBoolean(PREF_GENERATE_SERIAL_NUMBER_FIELDS));
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
			store.setDefault(PREF_GENERATED_TEST_SOURCE_FOLDER, SARLConfig.FOLDER_TEST_SOURCE_GENERATED);
			store.setDefault(PREF_GENERATE_INLINE, GeneratorConfig2.DEFAULT_GENERATE_INLINE_ANNOTATION);
			store.setDefault(PREF_USE_EXPRESSION_INTERPRETER, GeneratorConfig2.DEFAULT_USE_EXPRESSION_INTERPRETER_FOR_INLINE_ANNOTATION);
			store.setDefault(PREF_GENERATE_PURE, GeneratorConfig2.DEFAULT_GENERATE_PURE_ANNOTATION);
			store.setDefault(PREF_GENERATE_EQUALITY_TEST_FUNCTIONS, GeneratorConfig2.DEFAULT_GENERATE_EQUALITY_TEST_FUNCTIONS);
			store.setDefault(PREF_GENERATE_TOSTRING_FUNCTIONS, GeneratorConfig2.DEFAULT_GENERATE_TOSTRING_FUNCTION);
			store.setDefault(PREF_GENERATE_CLONE_FUNCTIONS, GeneratorConfig2.DEFAULT_GENERATE_CLONE_FUNCTION);
			store.setDefault(PREF_GENERATE_SERIAL_NUMBER_FIELDS, GeneratorConfig2.DEFAULT_GENERATE_SERIAL_NUMBER_FIELD);
		}

	}

}
