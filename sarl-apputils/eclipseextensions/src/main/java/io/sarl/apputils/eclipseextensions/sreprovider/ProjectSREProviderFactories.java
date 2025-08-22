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

package io.sarl.apputils.eclipseextensions.sreprovider;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import io.sarl.apputils.eclipseextensions.EclipseExtensionsPlugin;
import io.sarl.apputils.eclipseextensions.Extensions;

/**
 * Tools for provides of SREs.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public final class ProjectSREProviderFactories {

	/**
	 * Name of the extension points for the SRE provider factories.
	 */
	public static final String EXTENSION_POINT_PROJECT_SRE_PROVIDER_FACTORY = "projectSREProviderFactory"; //$NON-NLS-1$

	/** Constructor.
	 */
	private ProjectSREProviderFactories() {
		//
	}

	/** Replies the fragments that are defined as extensions in the given Eclipse plugin.
	 *
	 * @return the list of the fragment.
	 */
	public static List<ProjectSREProviderFactory> getSREProviderFactoriesFromExtension() {
		return getSREProviderFactoryStreamFromExtension().collect(Collectors.toList());
	}

	/** Replies the fragments that are defined as extensions in the given Eclipse plugin.
	 *
	 * @return the list of the fragment.
	 */
	public static Stream<ProjectSREProviderFactory> getSREProviderFactoryStreamFromExtension() {
		return Extensions.getExtensions(
				EclipseExtensionsPlugin.PLUGIN_ID, EXTENSION_POINT_PROJECT_SRE_PROVIDER_FACTORY,
				"class", ProjectSREProviderFactory.class); //$NON-NLS-1$
	}

}
