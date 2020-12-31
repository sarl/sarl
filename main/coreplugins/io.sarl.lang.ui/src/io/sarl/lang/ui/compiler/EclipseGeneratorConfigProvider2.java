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

package io.sarl.lang.ui.compiler;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IStorage;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.Pair;

import io.sarl.lang.compiler.GeneratorConfig2;
import io.sarl.lang.compiler.IGeneratorConfigProvider2;
import io.sarl.lang.ui.preferences.SARLBuilderPreferenceAccess;

/** Provider of a generator configuration v2.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.4
 */
public class EclipseGeneratorConfigProvider2 implements IGeneratorConfigProvider2 {

	@Inject
	private IStorage2UriMapper storage2UriMapper;

	@Inject
	private SARLBuilderPreferenceAccess preferenceAccess;

	@Override
	public GeneratorConfig2 get(EObject context) {
		final GeneratorConfig2 config = new GeneratorConfig2();
		IProject project = null;
		if (context.eResource() != null) {
			final Pair<IStorage, IProject> pair = Iterables.getFirst(
					this.storage2UriMapper.getStorages(context.eResource().getURI()), null);
			if (pair != null) {
				project = pair.getSecond();
			}
		}
		this.preferenceAccess.loadBuilderPreferences(config, project);
		return config;
	}

}
