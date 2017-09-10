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

import javax.inject.Singleton;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IGenerator2;
import org.eclipse.xtext.generator.IGeneratorContext;

import io.sarl.lang.compiler.extra.IExtraLanguageGeneratorProvider;
import io.sarl.lang.ui.SARLUiConfig;
import io.sarl.lang.ui.internal.LangActivator;

/** Implementation of the provider of the extra language generators that replies no generator.
 *
 * <p>The generators are provided by the extension points.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
public class ExtensionPointExtraLanguageGeneratorProvider implements IExtraLanguageGeneratorProvider {

	private static final String EXTENSION_POINT_GENERATOR_ATTRIBUTE = "generator"; //$NON-NLS-1$

	private List<IExtraLanguageGeneratorProvider> providers;

	@Override
	public Iterable<IGenerator2> getGenerators(IGeneratorContext context, Resource resource) {
		final List<IGenerator2> generators = new ArrayList<>();
		if (this.providers == null) {
			this.providers = new ArrayList<>();
			final IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
					SARLUiConfig.NAMESPACE, SARLUiConfig.EXTENSION_POINT_EXTRA_LANGUAGE_GENERATORS);
			if (extensionPoint != null) {
				Object obj;
				for (final IConfigurationElement element : extensionPoint.getConfigurationElements()) {
					//final String typeName = element.getAttribute(EXTENSION_POINT_GENERATOR_ATTRIBUTE);
					try {
						obj = element.createExecutableExtension(EXTENSION_POINT_GENERATOR_ATTRIBUTE);
						if (obj instanceof IExtraLanguageGeneratorProvider) {
							this.providers.add((IExtraLanguageGeneratorProvider) obj);
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
		for (final IExtraLanguageGeneratorProvider provider : this.providers) {
			for (final IGenerator2 gen : provider.getGenerators(context, resource)) {
				generators.add(gen);
			}
		}
		return generators;
	}

}
