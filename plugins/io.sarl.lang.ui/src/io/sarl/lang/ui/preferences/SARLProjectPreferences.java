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

import io.sarl.lang.ui.internal.SARLActivator;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.xtext.builder.EclipseOutputConfigurationProvider;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;

import com.google.inject.Injector;

/** Utilities related to the preferences on a SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SARLProjectPreferences {

	private SARLProjectPreferences() {
		//
	}

	/** Replies the preference store for the given project.
	 *
	 * @param project - the project.
	 * @return the preference store or <code>null</code>.
	 */
	public static IPreferenceStore getPreferencesFor(IProject project) {
		if (project != null) {
			Injector injector = SARLActivator.getInstance().getInjector(SARLActivator.IO_SARL_LANG_SARL);
			IPreferenceStoreAccess preferenceStoreAccess = injector.getInstance(IPreferenceStoreAccess.class);
			return preferenceStoreAccess.getWritablePreferenceStore(project);
		}
		return null;
	}

	/** Replies the Xtext output configurations related to the given project.
	 *
	 * @param project - the project.
	 * @return the Xtext output configurations.
	 */
	public static Set<OutputConfiguration> getXtextConfigurationsFor(IProject project) {
		Injector injector = SARLActivator.getInstance().getInjector(SARLActivator.IO_SARL_LANG_SARL);
		EclipseOutputConfigurationProvider configurationProvider =
				injector.getInstance(EclipseOutputConfigurationProvider.class);
		return configurationProvider.getOutputConfigurations(project);
	}

}
