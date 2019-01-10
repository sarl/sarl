/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

	/** Key for saving the enabling state of the codemining feature.
	 */
	public static final String CODEMINING_PROPERTY =  PREFIX + "codemining"; //$NON-NLS-1$

	/** Default value for saving the enabling state of the codemining feature.
	 */
	public static final boolean CODEMINING_DEFAULT_VALUE = true;


	/** Replies if the codemining feature is enable into the SARL editor.
	 *
	 * @return {@code true} if it is enabled.
	 */
	public boolean isCodeminingEnabled() {
		final IPreferenceStore store = getWritablePreferenceStore(null);
		return store.getBoolean(CODEMINING_PROPERTY);
	}

	/** Enable or disable the codemining feature into the SARL editor.
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

	@Override
	public void setToDefault(Object context) {
		final IPreferenceStore store = getWritablePreferenceStore(context);
		store.setToDefault(CODEMINING_PROPERTY);
	}

	/** Initializer of the preferences for the SARL codemining feature.
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
			access.getWritablePreferenceStore().setDefault(CODEMINING_PROPERTY, CODEMINING_DEFAULT_VALUE);
		}
	}

}
