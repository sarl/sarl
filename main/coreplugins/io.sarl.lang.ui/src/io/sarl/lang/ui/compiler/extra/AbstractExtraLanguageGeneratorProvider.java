/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

import java.lang.ref.SoftReference;
import java.util.Collections;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.google.inject.Injector;
import org.eclipse.core.resources.IProject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IGenerator2;
import org.eclipse.xtext.generator.IGeneratorContext;

import io.sarl.lang.compiler.extra.IExtraLanguageGeneratorProvider;
import io.sarl.lang.compiler.extra.IRootGenerator;
import io.sarl.lang.ui.compiler.ProjectAdapter;
import io.sarl.lang.ui.compiler.extra.preferences.ExtraLanguagePreferenceAccess;

/** Abstract implementation of a generator provider.
 *
 * @param <T> the type of the root generator.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
public abstract class AbstractExtraLanguageGeneratorProvider<T extends IRootGenerator>
		implements IExtraLanguageGeneratorProvider {

	@Inject
	private Injector injector;

	@Inject
	private ExtraLanguagePreferenceAccess preferences;

	private SoftReference<T> generator;

	/** Create a generator instance.
	 *
	 * @param injector the injector.
	 * @return the instance.
	 */
	protected abstract T createGeneratorInstance(Injector injector);

	/** Replies the identifier of the container of the generator's preferences.
	 *
	 * @return the identifier.
	 */
	protected abstract String getPreferenceID();

	@Override
	public Iterable<IGenerator2> getGenerators(IGeneratorContext context, Resource resource) {
		final IProject project = ProjectAdapter.getProject(resource);
		if (this.preferences.isGeneratorEnabled(
				getPreferenceID(),
				project)) {
			T gen;
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
