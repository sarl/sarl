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

package io.sarl.lang.ui.compiler.extra;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.google.inject.Singleton;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;
import org.eclipse.xtext.generator.OutputConfiguration;

import io.sarl.lang.compiler.SarlOutputConfigurationProvider;
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
public class ExtensionPointExtraLanguageOutputConfigurationProvider extends SarlOutputConfigurationProvider {

	private static final String EXTENSION_POINT_CONFIGURATION_ATTRIBUTE = "configuration"; //$NON-NLS-1$

	private List<IOutputConfigurationProvider> providers;

	@Override
	public Set<OutputConfiguration> getOutputConfigurations() {
		final Set<OutputConfiguration> configurations = super.getOutputConfigurations();
		if (this.providers == null) {
			this.providers = new ArrayList<>();
			final IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
					SARLUiConfig.NAMESPACE, SARLUiConfig.EXTENSION_POINT_EXTRA_LANGUAGE_GENERATORS);
			if (extensionPoint != null) {
				Object obj;
				for (final IConfigurationElement element : extensionPoint.getConfigurationElements()) {
					try {
						obj = element.createExecutableExtension(EXTENSION_POINT_CONFIGURATION_ATTRIBUTE);
						if (obj instanceof IOutputConfigurationProvider) {
							this.providers.add((IOutputConfigurationProvider) obj);
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
		for (final IOutputConfigurationProvider provider : this.providers) {
			configurations.addAll(provider.getOutputConfigurations());
		}
		return configurations;
	}

}
