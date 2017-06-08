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

package io.sarl.pythongenerator.generator;

import java.util.Collections;

import javax.inject.Inject;

import org.eclipse.core.resources.IProject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.generator.IGenerator2;
import org.eclipse.xtext.generator.IGeneratorContext;

import io.sarl.lang.generator.extra.IExtraLanguageGeneratorProvider;
import io.sarl.lang.ui.generator.extra.ProjectAdapter;
import io.sarl.lang.ui.generator.extra.preferences.ExtraLanguagePreferenceAccess;
import io.sarl.pythongenerator.PyGeneratorPlugin;

/** Provider the Python generator if is it enabled.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class PyGeneratorProvider implements IExtraLanguageGeneratorProvider {

	@Inject
	private PyGenerator generator;

	@Inject
	private ExtraLanguagePreferenceAccess preferences;

	@Override
	public Iterable<IGenerator2> getGenerators(IGeneratorContext context, Resource resource) {
		final IProject project = ProjectAdapter.getProject(resource);
		if (this.preferences.isGeneratorEnabled(
				PyGeneratorPlugin.PLUGIN_ID,
				project)) {
			return Collections.singletonList(this.generator);
		}
		return Collections.emptyList();
	}

}
