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

package io.sarl.lang.ui.extralanguage.compiler;

import java.lang.ref.SoftReference;
import java.util.Collections;

import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IGeneratorContext;

import io.sarl.lang.extralanguage.compiler.IExtraLanguageGeneratorProvider;
import io.sarl.lang.extralanguage.compiler.IRootGenerator;
import io.sarl.lang.ui.compiler.ProjectAdapter;
import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;

/** Abstract implementation of a generator provider.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.6
 */
@Singleton
public abstract class AbstractExtraLanguageGeneratorProvider implements IExtraLanguageGeneratorProvider {

	@Inject
	private Injector injector;

	@Inject
	private ExtraLanguagePreferenceAccess preferences;

	private SoftReference<IRootGenerator> generator;

	/** Create a generator instance.
	 *
	 * @param injector the injector.
	 * @return the instance.
	 */
	protected abstract IRootGenerator createGeneratorInstance(Injector injector);

	/** Replies the identifier of the container of the generator's preferences.
	 *
	 * @return the identifier.
	 */
	protected abstract String getPreferenceID();

	@Override
	public Iterable<IRootGenerator> getGenerators(IGeneratorContext context, Resource resource) {
		final var project = ProjectAdapter.getProject(resource);
		if (this.preferences.isGeneratorEnabled(
				getPreferenceID(),
				project)) {
			IRootGenerator gen;
			synchronized (this) {
				gen = this.generator == null ? null : this.generator.get();
				if (gen == null) {
					gen = createGeneratorInstance(this.injector);
					this.generator = new SoftReference<>(gen);
				}
			}
			return Collections.singletonList(gen);
		}
		return Collections.emptyList();
	}

}
