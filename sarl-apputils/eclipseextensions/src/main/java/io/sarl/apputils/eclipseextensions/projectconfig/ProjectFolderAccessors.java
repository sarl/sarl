/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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
 * Tools for accessing to the project folders.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15.1
 */
public final class ProjectFolderAccessors {

	/**
	 * Name of the extension points for the configuration fragments.
	 */
	public static final String EXTENSION_POINT_PROJECT_FOLDER_ACCESSOR = "projectFolderAccessor"; //$NON-NLS-1$

	/** Constructor.
	 */
	private ProjectFolderAccessors() {
		//
	}

	/** Replies the accessors that are defined as extensions in the given Eclipse plugin.
	 *
	 * @return the list of the accessors.
	 */
	public static List<ProjectFolderAccessor> getProjectFolderAccessorsFromExtension() {
		return getProjectFolderAccessorStreamFromExtension().collect(Collectors.toList());
	}

	/** Replies the accessors that are defined as extensions in the given Eclipse plugin.
	 *
	 * @return the list of the accessors.
	 */
	public static Stream<ProjectFolderAccessor> getProjectFolderAccessorStreamFromExtension() {
		return Extensions.getExtensions(
				EclipseExtensionsPlugin.PLUGIN_ID, EXTENSION_POINT_PROJECT_FOLDER_ACCESSOR,
				"class", ProjectFolderAccessor.class); //$NON-NLS-1$
	}

}
