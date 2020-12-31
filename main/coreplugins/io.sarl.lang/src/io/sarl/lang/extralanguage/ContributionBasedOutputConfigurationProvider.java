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
import java.util.Set;

import com.google.inject.Inject;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;
import org.eclipse.xtext.generator.OutputConfiguration;

import io.sarl.lang.compiler.SarlOutputConfigurationProvider;

/** Provider of the output configuration that supports the extra-language configurations.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class ContributionBasedOutputConfigurationProvider extends SarlOutputConfigurationProvider {

	@Inject
	private IExtraLanguageContributions source;

	private List<IOutputConfigurationProvider> providers;

	@Override
	public Set<OutputConfiguration> getOutputConfigurations() {
		final Set<OutputConfiguration> configurations = super.getOutputConfigurations();
		if (this.providers == null) {
			this.providers = new ArrayList<>();
			for (final IExtraLanguageContribution contribution : this.source.getContributions()) {
				final IOutputConfigurationProvider provider = contribution.getOutputConfigurationProvider();
				if (provider != null) {
					this.providers.add(provider);
				}
			}
		}
		for (final IOutputConfigurationProvider provider : this.providers) {
			configurations.addAll(provider.getOutputConfigurations());
		}
		return configurations;
	}

}
