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

package io.sarl.apputils.eclipseextensions.projectconfig;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.sarl.apputils.eclipseextensions.EclipseExtensionsPlugin;
import io.sarl.apputils.eclipseextensions.Extensions;

/**
 * Tools for the fragments of the configuration for a SARL project.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version eclipseextensions 0.15.1 20250911-224825
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid eclipseextensions
 * @since 0.15
 */
public final class ProjectConfigurationFragments {

	/**
	 * Name of the extension points for the configuration fragments.
	 */
	public static final String EXTENSION_POINT_PROJECT_CONFIGURATION_FRAGMENT = "projectConfigurationFragment"; //$NON-NLS-1$

	/** Constructor.
	 */
	private ProjectConfigurationFragments() {
		//
	}

	/** Replies the fragments that are defined as extensions in the given Eclipse plugin.
	 *
	 * @return the list of the fragment.
	 */
	public static List<ProjectConfigurationFragment> getConfigurationFragmentsFromExtension() {
		return getConfigurationFragmentStreamFromExtension().collect(Collectors.toList());
	}

	/** Replies the fragments that are defined as extensions in the given Eclipse plugin.
	 *
	 * @return the list of the fragment.
	 */
	public static Stream<ProjectConfigurationFragment> getConfigurationFragmentStreamFromExtension() {
		return Extensions.getExtensions(
				EclipseExtensionsPlugin.PLUGIN_ID, EXTENSION_POINT_PROJECT_CONFIGURATION_FRAGMENT,
				"class", ProjectConfigurationFragment.class); //$NON-NLS-1$
	}

}
