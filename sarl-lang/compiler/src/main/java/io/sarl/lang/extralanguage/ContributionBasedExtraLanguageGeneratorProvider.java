/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
import org.eclipse.xtext.generator.IGeneratorContext;

import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorProvider;
import io.sarl.lang.extralanguage.compiler.IRootGenerator;

/** Provider of the extra-language generators.
 *
 * @author $Author: sgalland$
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.8
 */
public class ContributionBasedExtraLanguageGeneratorProvider implements IExtraLanguageGeneratorProvider {

	@Inject
	private IExtraLanguageContributions source;

	private List<IExtraLanguageGeneratorProvider> providers;

	@Override
	public Iterable<IRootGenerator> getGenerators(IGeneratorContext context, Resource resource) {
		final var generators = new ArrayList<IRootGenerator>();
		if (this.providers == null) {
			this.providers = new ArrayList<>();
			for (final var contribution : this.source.getContributions()) {
				final var provider = contribution.getGeneratorProvider();
				if (provider != null) {
					this.providers.add(provider);
				}
			}
		}
		for (final var provider : this.providers) {
			for (final var gen : provider.getGenerators(context, resource)) {
				generators.add(gen);
			}
		}
		return generators;
	}

}
