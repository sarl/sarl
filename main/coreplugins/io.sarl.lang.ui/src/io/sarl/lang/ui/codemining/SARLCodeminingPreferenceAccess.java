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

package io.sarl.lang.ui.codemining;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;

import io.sarl.lang.ui.preferences.AbstractPreferenceAccess;

/** Preferences for the code mining support of SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class SARLCodeminingPreferenceAccess extends AbstractPreferenceAccess {

	/** Prefix for the preference keys.
	 */
	public static final String PREFIX = SARLCodeminingPreferenceAccess.class.getPackage().getName() + "."; //$NON-NLS-1$

	/** Key for saving the enabling state of the code mining feature.
	 */
	public static final String CODEMINING_PROPERTY =  PREFIX + "codemining"; //$NON-NLS-1$

	/** Key for saving the temp enabling state of the code mining feature.
	 */
	public static final String CODEMINING_TEMPORARY_PROPERTY =  PREFIX + "codemining.temporary"; //$NON-NLS-1$

	/** Default value for saving the enabling state of the code mining feature.
	 */
	public static final boolean CODEMINING_DEFAULT_VALUE = true;

	/** Key for saving the enabling state of the code mining feature of the action return type.
	 * @since 0.12
	 */
	public static final String CODEMINING_ACTION_RETURN_TYPE_PROPERTY =  PREFIX + "codemining.actionReturnType"; //$NON-NLS-1$

	/** Default value for saving the enabling state of the code mining feature of the action return type.
	 * @since 0.12
	 */
	public static final boolean CODEMINING_ACTION_RETURN_TYPE_VALUE = true;

	/** Key for saving the enabling state of the code mining feature of the field type.
	 * @since 0.12
	 */
	public static final String CODEMINING_FIELD_TYPE_PROPERTY =  PREFIX + "codemining.fieldType"; //$NON-NLS-1$

	/** Default value for saving the enabling state of the code mining feature of the field type.
	 * @since 0.12
	 */
	public static final boolean CODEMINING_FIELD_TYPE_VALUE = true;

	/** Key for saving the enabling state of the code mining feature of the variable type.
	 * @since 0.12
	 */
	public static final String CODEMINING_VARIABLE_TYPE_PROPERTY =  PREFIX + "codemining.variableType"; //$NON-NLS-1$

	/** Default value for saving the enabling state of the code mining feature of the variable type.
	 * @since 0.12
	 */
	public static final boolean CODEMINING_VARIABLE_TYPE_VALUE = true;

	/** Key for saving the enabling state of the code mining feature of the function's argument names.
	 * @since 0.12
	 */
	public static final String CODEMINING_FEATURECALL_ARGUMENT_NAME_PROPERTY =  PREFIX + "codemining.featureCallArgumentName"; //$NON-NLS-1$

	/** Default value for saving the enabling state of the code mining feature of the function's argument names.
	 * @since 0.12
	 */
	public static final boolean CODEMINING_FEATURECALL_ARGUMENT_NAME_VALUE = true;

	/** Replies if the code mining feature is enable into the SARL editor.
	 *
	 * @return {@code true} if it is enabled.
	 * @see #isCodeminingDisabledOrTemporaryDisabled()
	 */
	public boolean isCodeminingEnabled() {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		return store.getBoolean(CODEMINING_PROPERTY);
	}

	/** Enable or disable the code mining feature into the SARL editor.
	 *
	 * @param enable is {@code true} if it is enabled; {@code false} if it is disable; {@code null}
	 *     to restore the default value.
	 * @since 0.8
	 * @see #getWritablePreferenceStore(Object)
	 */
	public void setCodeminingEnabled(Boolean enable) {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		if (enable == null) {
			store.setToDefault(CODEMINING_PROPERTY);
		} else {
			store.setValue(CODEMINING_PROPERTY, enable.booleanValue());
		}
	}

	/** Replies if the code mining feature is enable into the SARL editor for the action return types.
	 *
	 * @return {@code true} if it is enabled.
	 * @since 0.12
	 */
	public boolean isCodeminingActionReturnTypeEnabled() {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		return store.getBoolean(CODEMINING_ACTION_RETURN_TYPE_PROPERTY);
	}

	/** Enable or disable the code mining feature into the SARL editor for the action return types.
	 *
	 * @param enable is {@code true} if it is enabled; {@code false} if it is disable; {@code null}
	 *     to restore the default value.
	 * @since 0.12
	 */
	public void setCodeminingActionReturnTypeEnabled(Boolean enable) {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		if (enable == null) {
			store.setToDefault(CODEMINING_ACTION_RETURN_TYPE_PROPERTY);
		} else {
			store.setValue(CODEMINING_ACTION_RETURN_TYPE_PROPERTY, enable.booleanValue());
		}
	}

	/** Replies if the code mining feature is enable into the SARL editor for the field types.
	 *
	 * @return {@code true} if it is enabled.
	 * @since 0.12
	 */
	public boolean isCodeminingFieldTypeEnabled() {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		return store.getBoolean(CODEMINING_FIELD_TYPE_PROPERTY);
	}

	/** Enable or disable the code mining feature into the SARL editor for the field types.
	 *
	 * @param enable is {@code true} if it is enabled; {@code false} if it is disable; {@code null}
	 *     to restore the default value.
	 * @since 0.12
	 */
	public void setCodeminingFieldTypeEnabled(Boolean enable) {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		if (enable == null) {
			store.setToDefault(CODEMINING_FIELD_TYPE_PROPERTY);
		} else {
			store.setValue(CODEMINING_FIELD_TYPE_PROPERTY, enable.booleanValue());
		}
	}

	/** Replies if the code mining feature is enable into the SARL editor for the variable types.
	 *
	 * @return {@code true} if it is enabled.
	 * @since 0.12
	 */
	public boolean isCodeminingVariableTypeEnabled() {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		return store.getBoolean(CODEMINING_VARIABLE_TYPE_PROPERTY);
	}

	/** Enable or disable the code mining feature into the SARL editor for the variable types.
	 *
	 * @param enable is {@code true} if it is enabled; {@code false} if it is disable; {@code null}
	 *     to restore the default value.
	 * @since 0.12
	 */
	public void setCodeminingVariableTypeEnabled(Boolean enable) {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		if (enable == null) {
			store.setToDefault(CODEMINING_VARIABLE_TYPE_PROPERTY);
		} else {
			store.setValue(CODEMINING_VARIABLE_TYPE_PROPERTY, enable.booleanValue());
		}
	}

	/** Replies if the code mining feature is enable into the SARL editor for the function's argument names.
	 *
	 * @return {@code true} if it is enabled.
	 * @since 0.12
	 */
	public boolean isCodeminingFeatureCallArgumentNameEnabled() {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		return store.getBoolean(CODEMINING_FEATURECALL_ARGUMENT_NAME_PROPERTY);
	}

	/** Enable or disable the code mining feature into the SARL editor for the function's argument names.
	 *
	 * @param enable is {@code true} if it is enabled; {@code false} if it is disable; {@code null}
	 *     to restore the default value.
	 * @since 0.12
	 */
	public void setCodeminingFeatureCallArgumentNameEnabled(Boolean enable) {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		if (enable == null) {
			store.setToDefault(CODEMINING_FEATURECALL_ARGUMENT_NAME_PROPERTY);
		} else {
			store.setValue(CODEMINING_FEATURECALL_ARGUMENT_NAME_PROPERTY, enable.booleanValue());
		}
	}

	@Override
	public void setToDefault(Object context) {
		final IPreferenceStore store = getWritablePreferenceStore(context);
		store.setToDefault(CODEMINING_PROPERTY);
		store.setToDefault(CODEMINING_ACTION_RETURN_TYPE_PROPERTY);
		store.setToDefault(CODEMINING_FIELD_TYPE_PROPERTY);
		store.setToDefault(CODEMINING_VARIABLE_TYPE_PROPERTY);
		store.setToDefault(CODEMINING_FEATURECALL_ARGUMENT_NAME_PROPERTY);
	}

	/** Initializer of the preferences for the SARL code mining feature.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	public static class Initializer implements IPreferenceStoreInitializer {
		@Override
		public void initialize(IPreferenceStoreAccess access) {
			final IPreferenceStore store = access.getWritablePreferenceStore();
			store.setDefault(CODEMINING_PROPERTY, CODEMINING_DEFAULT_VALUE);
			store.setDefault(CODEMINING_ACTION_RETURN_TYPE_PROPERTY, CODEMINING_ACTION_RETURN_TYPE_VALUE);
			store.setDefault(CODEMINING_FIELD_TYPE_PROPERTY, CODEMINING_FIELD_TYPE_VALUE);
			store.setDefault(CODEMINING_VARIABLE_TYPE_PROPERTY, CODEMINING_VARIABLE_TYPE_VALUE);
			store.setDefault(CODEMINING_FEATURECALL_ARGUMENT_NAME_PROPERTY, CODEMINING_FEATURECALL_ARGUMENT_NAME_VALUE);
		}
	}

}
