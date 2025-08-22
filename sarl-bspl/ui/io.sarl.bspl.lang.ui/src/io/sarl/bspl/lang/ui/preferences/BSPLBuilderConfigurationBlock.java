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

package io.sarl.bspl.lang.ui.preferences;

import java.util.Set;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.eclipse.core.resources.IProject;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.xbase.ui.builder.XbaseBuilderConfigurationBlock;

import io.sarl.apputils.uiextensions.outputconfig.IContextualOutputConfigurationProvider3;

/** Preference page that permits to configure the BSPL builder.
 *
 * <p>This page extends the Xbase page.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("restriction")
public class BSPLBuilderConfigurationBlock extends XbaseBuilderConfigurationBlock {

	@Inject
	@Named("bspl")
	private IContextualOutputConfigurationProvider3 configurationProvider;

	/** Replies the output configurations for the given project.
	 *
	 * <p>This function filters the output configurations in order to never reply one
	 * associated to a extra language output.
	 *
	 * @param project the project.
	 * @return the output configurations associated to the given project.
	 */
	@Override
	protected Set<OutputConfiguration> getOutputConfigurations(IProject project) {
		return this.configurationProvider.getOutputConfigurations(project);
	}

}
