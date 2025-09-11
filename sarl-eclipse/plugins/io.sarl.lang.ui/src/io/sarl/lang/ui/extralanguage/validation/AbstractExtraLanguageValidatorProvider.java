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

package io.sarl.lang.ui.extralanguage.validation;

import java.lang.ref.SoftReference;
import java.util.Collections;
import java.util.List;

import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.resource.Resource;

import io.sarl.lang.extralanguage.validator.AbstractExtraLanguageValidator;
import io.sarl.lang.extralanguage.validator.IExtraLanguageValidatorProvider;
import io.sarl.lang.ui.compiler.ProjectAdapter;
import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;

/** Abstract implementation of a validator provider.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 * @since 0.6
 */
@Singleton
public abstract class AbstractExtraLanguageValidatorProvider implements IExtraLanguageValidatorProvider {

	@Inject
	private Injector injector;

	@Inject
	private ExtraLanguagePreferenceAccess preferences;

	private SoftReference<AbstractExtraLanguageValidator> validator;

	/** Create a validator instance.
	 *
	 * @param injector the injector.
	 * @return the instance.
	 */
	protected abstract AbstractExtraLanguageValidator createValidatorInstance(Injector injector);

	/** Replies the identifier of the container of preferences.
	 *
	 * @return the identifier.
	 */
	protected abstract String getPreferenceID();

	@Override
	public List<AbstractExtraLanguageValidator> getValidators(Resource resource) {
		final var project = ProjectAdapter.getProject(resource);
		if (this.preferences.isGeneratorEnabled(
				getPreferenceID(),
				project)) {
			AbstractExtraLanguageValidator val;
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
