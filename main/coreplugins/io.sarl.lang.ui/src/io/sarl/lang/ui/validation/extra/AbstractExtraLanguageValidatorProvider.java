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

package io.sarl.lang.ui.validation.extra;

import java.lang.ref.SoftReference;
import java.util.Collections;
import java.util.List;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.google.inject.Injector;
import org.eclipse.core.resources.IProject;
import org.eclipse.emf.ecore.resource.Resource;

import io.sarl.lang.ui.compiler.extra.ProjectAdapter;
import io.sarl.lang.ui.compiler.extra.preferences.ExtraLanguagePreferenceAccess;
import io.sarl.lang.validation.extra.AbstractExtraLanguageValidator;
import io.sarl.lang.validation.extra.IExtraLanguageValidatorProvider;

/** Abstract implementation of a validator provider.
 *
 * @param <T> the type of the validator.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
public abstract class AbstractExtraLanguageValidatorProvider<T extends AbstractExtraLanguageValidator>
	implements IExtraLanguageValidatorProvider {

	@Inject
	private Injector injector;

	@Inject
	private ExtraLanguagePreferenceAccess preferences;

	private SoftReference<T> validator;

	/** Create a validator instance.
	 *
	 * @param injector the injector.
	 * @return the instance.
	 */
	protected abstract T createValidatorInstance(Injector injector);

	/** Replies the plugin identifier.
	 *
	 * @return the plugin identifier.
	 */
	protected abstract String getPluginID();

	@Override
	public List<AbstractExtraLanguageValidator> getValidators(Resource resource) {
		final IProject project = ProjectAdapter.getProject(resource);
		if (this.preferences.isGeneratorEnabled(
				getPluginID(),
				project)) {
			T val;
			synchronized (this) {
				val = this.validator == null ? null : this.validator.get();
				if (val == null) {
					val = createValidatorInstance(this.injector);
					this.validator = new SoftReference<>(val);
				}
			}
			return Collections.singletonList(val);
		}
		return Collections.emptyList();
	}

}
