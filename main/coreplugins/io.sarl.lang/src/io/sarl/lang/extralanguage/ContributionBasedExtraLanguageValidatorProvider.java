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

package io.sarl.lang.extralanguage;

import java.util.ArrayList;
import java.util.List;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.resource.Resource;

import io.sarl.lang.extralanguage.validator.AbstractExtraLanguageValidator;
import io.sarl.lang.extralanguage.validator.IExtraLanguageValidatorProvider;

/** Provider of the extra-language validators.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class ContributionBasedExtraLanguageValidatorProvider implements IExtraLanguageValidatorProvider {

	@Inject
	private IExtraLanguageContributions source;

	private List<IExtraLanguageValidatorProvider> providers;

	@Override
	public List<AbstractExtraLanguageValidator> getValidators(Resource resource) {
		final List<AbstractExtraLanguageValidator> validators = new ArrayList<>();
		if (this.providers == null) {
			this.providers = new ArrayList<>();
			for (final IExtraLanguageContribution contribution : this.source.getContributions()) {
				final IExtraLanguageValidatorProvider provider = contribution.getValidatorProvider();
				if (provider != null) {
					this.providers.add(provider);
				}
			}
		}
		for (final IExtraLanguageValidatorProvider provider:  this.providers) {
			for (final AbstractExtraLanguageValidator validator : provider.getValidators(resource)) {
				validators.add(validator);
			}
		}
		return validators;
	}

}
