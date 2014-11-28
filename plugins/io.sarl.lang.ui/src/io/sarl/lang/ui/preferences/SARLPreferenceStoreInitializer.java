/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.xtext.preferences.PreferenceKey;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;
import org.eclipse.xtext.ui.editor.preferences.PreferenceConstants;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;

import com.google.inject.Inject;
import com.google.inject.Singleton;

/** Initialize the preference store with SARL specific information.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SARLPreferenceStoreInitializer implements IPreferenceStoreInitializer, IPropertyChangeListener {

	private IPreferenceStoreAccess preferenceStoreAccess;

	@Inject
	private ConfigurableIssueCodesProvider issueCodes;

	@Override
	public void initialize(IPreferenceStoreAccess preferenceStoreAccess) {
		this.preferenceStoreAccess = preferenceStoreAccess;
		setupIssueCodesDefaults(preferenceStoreAccess);

		IPreferenceStore preferenceStore = org.eclipse.jdt.ui.PreferenceConstants.getPreferenceStore();
		preferenceStore.addPropertyChangeListener(this);
		preferenceStoreAccess.getWritablePreferenceStore().setDefault(
				PreferenceConstants.EDITOR_SUB_WORD_NAVIGATION,
				preferenceStore.getBoolean(org.eclipse.jdt.ui.PreferenceConstants.EDITOR_SUB_WORD_NAVIGATION));

	}

	private void setupIssueCodesDefaults(IPreferenceStoreAccess preferenceStoreAccess) {
		for (PreferenceKey prefKey : this.issueCodes.getConfigurableIssueCodes().values()) {
			preferenceStoreAccess.getWritablePreferenceStore().setDefault(prefKey.getId(), prefKey.getDefaultValue());
		}
	}

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		if (this.preferenceStoreAccess == null) {
			return;
		}
		if (org.eclipse.jdt.ui.PreferenceConstants.EDITOR_SUB_WORD_NAVIGATION.equalsIgnoreCase(event.getProperty())) {
			this.preferenceStoreAccess.getWritablePreferenceStore().setValue(PreferenceConstants.EDITOR_SUB_WORD_NAVIGATION,
					Boolean.valueOf(event.getNewValue().toString()));
		}

	}

}
