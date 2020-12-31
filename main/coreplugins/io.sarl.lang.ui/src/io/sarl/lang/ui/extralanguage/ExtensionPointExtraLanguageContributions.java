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

package io.sarl.lang.ui.extralanguage;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.function.Predicate;

import com.google.inject.Singleton;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;

import io.sarl.lang.extralanguage.IExtraLanguageContribution;
import io.sarl.lang.extralanguage.IExtraLanguageContributions;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorProvider;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageKeywordProvider;
import io.sarl.lang.extralanguage.validator.IExtraLanguageValidatorProvider;
import io.sarl.lang.ui.SARLUiConfig;
import io.sarl.lang.ui.internal.LangActivator;

/** Tool for obtaining all the contributions as an extra-language generators.
 *
 * <p>The contributions are obtained from the definitions of Eclipse extension points.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@Singleton
public class ExtensionPointExtraLanguageContributions implements IExtraLanguageContributions {

	private static final String EXTENSION_POINT_GENERATOR_ATTRIBUTE = "generator"; //$NON-NLS-1$

	private static final String EXTENSION_POINT_VALIDATOR_ATTRIBUTE = "validator"; //$NON-NLS-1$

	private static final String EXTENSION_POINT_KEYWORDS_ATTRIBUTE = "keywords"; //$NON-NLS-1$

	private static final String EXTENSION_POINT_OUTPUT_CONFIGURATION_ATTRIBUTE = "outputConfiguration"; //$NON-NLS-1$

	private Collection<IExtraLanguageContribution> contributions;

	@Override
	public void setContributionChecker(Predicate<IExtraLanguageContribution> checker) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Collection<IExtraLanguageContribution> getContributions() {
		if (this.contributions == null) {
			this.contributions = new ArrayList<>();

			final IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
					SARLUiConfig.NAMESPACE, SARLUiConfig.EXTENSION_POINT_EXTRA_LANGUAGE_GENERATORS);
			if (extensionPoint != null) {
				for (final IConfigurationElement element : extensionPoint.getConfigurationElements()) {
					try {
						final IExtraLanguageGeneratorProvider generator = getType(
								IExtraLanguageGeneratorProvider.class, element, EXTENSION_POINT_GENERATOR_ATTRIBUTE);
						final IExtraLanguageValidatorProvider validator = getType(
								IExtraLanguageValidatorProvider.class, element, EXTENSION_POINT_VALIDATOR_ATTRIBUTE);
						final IOutputConfigurationProvider configuration = getType(
								IOutputConfigurationProvider.class, element, EXTENSION_POINT_OUTPUT_CONFIGURATION_ATTRIBUTE);
						final IExtraLanguageKeywordProvider keywords = getType(
								IExtraLanguageKeywordProvider.class, element, EXTENSION_POINT_KEYWORDS_ATTRIBUTE);
						this.contributions.add(new ExtensionPointContribution(generator, validator,
								configuration, keywords));
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
		return this.contributions;
	}

	private static <T> T getType(Class<T> type, IConfigurationElement element, String attributeName) throws CoreException {
		final Object obj = element.createExecutableExtension(attributeName);
		if (type.isInstance(obj)) {
			return type.cast(obj);
		}
		LangActivator.getInstance().getLog().log(new Status(
				IStatus.WARNING,
				LangActivator.getInstance().getBundle().getSymbolicName(),
				MessageFormat.format(
						Messages.ExtensionPointExtraLanguageContributions_0,
						attributeName,
						SARLUiConfig.NAMESPACE + "." + SARLUiConfig.EXTENSION_POINT_EXTRA_LANGUAGE_GENERATORS))); //$NON-NLS-1$
		return null;
	}

	/** Contribution description.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	private static class ExtensionPointContribution implements IExtraLanguageContribution {

		private final IExtraLanguageGeneratorProvider generator;

		private final IExtraLanguageValidatorProvider validator;

		private final IOutputConfigurationProvider configuration;

		private final IExtraLanguageKeywordProvider keywords;

		ExtensionPointContribution(IExtraLanguageGeneratorProvider generator, IExtraLanguageValidatorProvider validator,
				IOutputConfigurationProvider configuration, IExtraLanguageKeywordProvider keywords) {
			this.generator = generator;
			this.validator = validator;
			this.configuration = configuration;
			this.keywords = keywords;
		}

		@Override
		public IExtraLanguageGeneratorProvider getGeneratorProvider() {
			return this.generator;
		}

		@Override
		public IExtraLanguageValidatorProvider getValidatorProvider() {
			return this.validator;
		}

		@Override
		public IOutputConfigurationProvider getOutputConfigurationProvider() {
			return this.configuration;
		}

		@Override
		public IExtraLanguageKeywordProvider getKeywordProvider() {
			return this.keywords;
		}

	}

}
