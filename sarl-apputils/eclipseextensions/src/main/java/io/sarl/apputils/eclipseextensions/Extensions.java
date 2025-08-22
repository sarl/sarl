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

package io.sarl.apputils.eclipseextensions;

import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Stream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;

import io.sarl.apputils.uiextensions.UiExtensionsPlugin;

/**
 * Tools for the Eclipse extensions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public final class Extensions {

	/** Constructor.
	 */
	private Extensions() {
		//
	}

	/** Replies the extensions that are defined in the given Eclipse plugin.
	 *
	 * @param pluginId the identifier of the plugin that defines the extension.
	 * @param extensionId the identifier of the extension.
	 * @param fieldName the name of the field that describes the type of the extension to be created.
	 * @param extensionType the type of the extension.
	 * @return the list of the extensions.
	 */
	public static <T> Stream<T> getExtensions(String pluginId, String extensionId, String fieldName, Class<T> extensionType) {
		final var extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(pluginId, extensionId);
		if (extensionPoint != null) {
			return Arrays.asList(extensionPoint.getConfigurationElements()).stream()
				.map(element -> {
					try {
						final var obj = element.createExecutableExtension(fieldName);
						if (obj != null && extensionType.isInstance(obj)) {
							return extensionType.cast(obj);
						}
						final var plugin = UiExtensionsPlugin.getDefault();
						final var status = plugin.createStatus(IStatus.ERROR,
							"Cannot instance extension point: " + element.getName()); //$NON-NLS-1$
						plugin.getLog().log(status);
					} catch (CoreException e) {
						final var plugin = UiExtensionsPlugin.getDefault();
						final var status = plugin.createStatus(IStatus.ERROR, e);
						plugin.getLog().log(status);
					}
					return null;
				})
				.filter(it -> it != null);
		}
		return Collections.<T>emptyList().stream();
	}

	/** Replies the extensions that are defined in the given Eclipse plugin.
	 *
	 * @param pluginId the identifier of the plugin that defines the extension.
	 * @param extensionId the identifier of the extension.
	 * @return the list of the extensions' informations.
	 */
	public static Stream<IConfigurationElement> getExtensions(String pluginId, String extensionId) {
		final var extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(pluginId, extensionId);
		if (extensionPoint != null) {
			return Arrays.asList(extensionPoint.getConfigurationElements()).stream();
		}
		return Collections.<IConfigurationElement>emptyList().stream();
	}

}
