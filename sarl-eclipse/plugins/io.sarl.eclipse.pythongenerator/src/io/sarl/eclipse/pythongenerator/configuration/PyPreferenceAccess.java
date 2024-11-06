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

package io.sarl.eclipse.pythongenerator.configuration;

import org.eclipse.jface.preference.IPreferenceStore;

import io.sarl.lang.pythongenerator.PyGeneratorPlugin;
import io.sarl.lang.pythongenerator.configuration.PyGeneratorConfiguration;
import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;

/** Preferences for the Python generators.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.pythongenerator 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.pythongenerator
 * @since 0.8
 */
public final class PyPreferenceAccess {

	/** Key for saving the compliance state of the Python generator regarding Jython.
	 */
	public static final String JYTHON_COMPLIANCE_PROPERTY = "jythonCompliance"; //$NON-NLS-1$

	private PyPreferenceAccess() {
		//
	}

	/** Load the generator configuration from the preferences.
	 *
	 * @param generatorConfig the configuration to set up.
	 * @param store the preference store access.
	 */
	public static void loadPreferences(PyGeneratorConfiguration generatorConfig, IPreferenceStore store) {
		final var key = ExtraLanguagePreferenceAccess.getPrefixedKey(PyGeneratorPlugin.PREFERENCE_ID,
				PyPreferenceAccess.JYTHON_COMPLIANCE_PROPERTY);
		if (store.contains(key)) {
			generatorConfig.setImplicitJvmTypes(store.getBoolean(key));
		}
	}

}
