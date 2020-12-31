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

package io.sarl.lang.ui.extralanguage.preferences;

import java.util.ArrayList;
import java.util.List;

import com.google.inject.Singleton;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreInitializer;

import io.sarl.lang.ui.SARLUiConfig;
import io.sarl.lang.ui.internal.LangActivator;


/** Provide the output configuration from the SARL code and the extra languages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
public class ExtensionPointExtraLanguagePreferenceInitializer implements IPreferenceStoreInitializer {

	private static final String EXTENSION_POINT_PREFERENCE_INITIALIZER_ATTRIBUTE = "preferences"; //$NON-NLS-1$

	private List<IPreferenceStoreInitializer> initializers;

	@Override
	public void initialize(IPreferenceStoreAccess access) {
		if (this.initializers == null) {
			this.initializers = new ArrayList<>();
			final IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
					SARLUiConfig.NAMESPACE, SARLUiConfig.EXTENSION_POINT_EXTRA_LANGUAGE_GENERATORS);
			if (extensionPoint != null) {
				Object obj;
				for (final IConfigurationElement element : extensionPoint.getConfigurationElements()) {
					try {
						obj = element.createExecutableExtension(EXTENSION_POINT_PREFERENCE_INITIALIZER_ATTRIBUTE);
						if (obj instanceof IPreferenceStoreInitializer) {
							this.initializers.add((IPreferenceStoreInitializer) obj);
						}
					} catch (CoreException exception) {
						LangActivator.getInstance().getLog().log(new Status(
								IStatus.WARNING,
								LangActivator.getInstance().getBundle().getSymbolicName(),
								exception.getLocalizedMessage(),
								exception));
					}
				}
			}
		}
		for (final IPreferenceStoreInitializer initializer : this.initializers) {
			initializer.initialize(access);
		}
	}

}
