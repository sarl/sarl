/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;
import org.eclipse.xtext.ui.editor.preferences.PreferenceConstants;
import org.eclipse.xtext.validation.ConfigurableIssueCodesProvider;

import io.sarl.lang.ui.codemining.SARLCodeminingPreferenceAccess;
import io.sarl.lang.ui.editor.SARLSourceViewerPreferenceAccess;
import io.sarl.lang.ui.extralanguage.preferences.ExtensionPointExtraLanguagePreferenceInitializer;

/** Initialize the preference store with SARL specific information.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 */
@Singleton
public class SARLPreferenceStoreInitializer implements IPreferenceStoreInitializer, IPropertyChangeListener {

	private IPreferenceStoreAccess preferenceStoreAccess;

	@Inject
	private ConfigurableIssueCodesProvider issueCodes;

	@Inject
	private ExtensionPointExtraLanguagePreferenceInitializer extraLanguagePreferenceInitializer;

	@Inject
	private SARLSourceViewerPreferenceAccess.Initializer sourceViewerInitializer;

	@Inject
	private SARLCodeminingPreferenceAccess.Initializer codeminingInitializer;

	@Override
	public void initialize(IPreferenceStoreAccess preferenceStoreAccess) {
		this.preferenceStoreAccess = preferenceStoreAccess;

		// Initialize the default visibilities for the optional issue codes.
		setupIssueCodesDefaults(preferenceStoreAccess);

		final var preferenceStore = org.eclipse.jdt.ui.PreferenceConstants.getPreferenceStore();
		preferenceStore.addPropertyChangeListener(this);

		// Copy the Subword navigation from the JDT plugin.
		preferenceStoreAccess.getWritablePreferenceStore().setDefault(
				PreferenceConstants.EDITOR_SUB_WORD_NAVIGATION,
				preferenceStore.getBoolean(org.eclipse.jdt.ui.PreferenceConstants.EDITOR_SUB_WORD_NAVIGATION));

		// Initialize the editor preferences
		setupSourceViewerDefaults(preferenceStoreAccess);
		setupCodeminingDefaults(preferenceStoreAccess);

		// Initialize the generators for the extra languages.
		setupExtraLanguageGeneratorDefaults(preferenceStoreAccess);
	}

	private void setupIssueCodesDefaults(IPreferenceStoreAccess preferenceStoreAccess) {
		final var store = preferenceStoreAccess.getWritablePreferenceStore();
		for (final var prefKey : this.issueCodes.getConfigurableIssueCodes().values()) {
			store.setDefault(prefKey.getId(), prefKey.getDefaultValue());
		}
	}

	private void setupExtraLanguageGeneratorDefaults(IPreferenceStoreAccess preferenceStoreAccess) {
		this.extraLanguagePreferenceInitializer.initialize(preferenceStoreAccess);
	}

	private void setupSourceViewerDefaults(IPreferenceStoreAccess preferenceStoreAccess) {
		this.sourceViewerInitializer.initialize(preferenceStoreAccess);
	}

	private void setupCodeminingDefaults(IPreferenceStoreAccess preferenceStoreAccess) {
		this.codeminingInitializer.initialize(preferenceStoreAccess);
	}

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		if (this.preferenceStoreAccess == null) {
			return;
		}
		// Copy the Subword navigation from the JDT plugin.
		if (org.eclipse.jdt.ui.PreferenceConstants.EDITOR_SUB_WORD_NAVIGATION.equalsIgnoreCase(event.getProperty())) {
			this.preferenceStoreAccess.getWritablePreferenceStore().setValue(PreferenceConstants.EDITOR_SUB_WORD_NAVIGATION,
					Boolean.parseBoolean(event.getNewValue().toString()));
		}

	}

}
