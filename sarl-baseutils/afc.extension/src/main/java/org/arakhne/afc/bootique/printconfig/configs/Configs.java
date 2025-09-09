/*
 * $Id$
 * This file is a part of the Arakhne Foundation Classes, http://www.arakhne.org/afc
 *
 * Copyright (c) 2000-2012 Stephane GALLAND.
 * Copyright (c) 2005-10, Multiagent Team, Laboratoire Systemes et Transports,
 *                        Universite de Technologie de Belfort-Montbeliard.
 * Copyright (c) 2013-2025 The original authors, and other authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.arakhne.afc.bootique.printconfig.configs;

import java.io.File;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import io.bootique.di.Injector;
import io.bootique.meta.MetadataNode;
import io.bootique.meta.config.ConfigMetadataNode;
import io.bootique.meta.module.ModuleMetadata;
import io.bootique.meta.module.ModulesMetadata;

/**
 * Constants and utilities for configurations.
 *
 * @author $Author: sgalland$
 * @version afc.extension 0.15.0 20250909-115746
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid afc.extension
 * @since 15.0
 */
public final class Configs {

	private Configs() {
		//
	}

	/** Extract the configuration metadata nodes from the given metadata.
	 *
	 * @param modulesMetadata the metadata of the bootique modules.
	 * @return the configuration metadata nodes.
	 */
	public static List<ConfigMetadataNode> extractConfigs(ModulesMetadata modulesMetadata) {
		final var modules = modulesMetadata
                .getModules()
                .stream()
                .collect(Collectors.toList());
		return modules.stream()
                .map(ModuleMetadata::getConfigs)
                .flatMap(Collection::stream)
                .sorted(Comparator.comparing(MetadataNode::getName))
                .collect(Collectors.toList());
	}

	/** Add a config to a Yaml configuration map.
	 *
	 * @param content the Yaml configuration map.
	 * @param config the configuration.
	 * @param injector the injector to be used for creating the configuration objects.
	 */
	public static void defineConfig(Map<String, Object> content, ConfigMetadataNode config, Injector injector) {
		assert content != null;
		assert config != null;
		final var type = (Class<?>) config.getType();
		final var sectionName = config.getName();
		final var setPattern = Pattern.compile("^set([A-Z])([a-zA-Z0-9]+)$"); //$NON-NLS-1$
		Object theConfig = null;
		for (final var setterMethod : type.getMethods()) {
			final var matcher = setPattern.matcher(setterMethod.getName());
			if (matcher.matches()) {
				final var firstLetter = matcher.group(1);
				final var rest = matcher.group(2);
				final var getterName = "get" + firstLetter + rest; //$NON-NLS-1$
				Method getterMethod = null;
				try {
					getterMethod = type.getMethod(getterName);
				} catch (Throwable exception) {
					//
				}
				if (getterMethod != null && Modifier.isPublic(getterMethod.getModifiers())
						&& !Modifier.isAbstract(getterMethod.getModifiers())
						&& !Modifier.isStatic(getterMethod.getModifiers())) {
					if (theConfig == null) {
						theConfig = injector.getInstance(type);
					}
					try {
						if (theConfig != null) {
							final var value = filterValue(getterMethod.getReturnType(),
									getterMethod.invoke(theConfig));
							final var id = sectionName + "." + firstLetter.toLowerCase() + rest; //$NON-NLS-1$
							defineScalar(content, id, value);
						}
					} catch (Throwable exception) {
						//
					}
				}
			}
		}
	}

	private static Object filterValue(Class<?> type, Object value) {
		if (value == null) {
			return ""; //$NON-NLS-1$
		}
		if (File.class.isAssignableFrom(type)) {
			return ((File) value).getAbsolutePath();
		}
		if (URI.class.isAssignableFrom(type)) {
			return ((URI) value).toString();
		}
		if (URL.class.isAssignableFrom(type)) {
			return ((URL) value).toExternalForm();
		}
		return value;
	}

	/** Add a scalar to a Yaml configuration map.
	 *
	 * @param content the Yaml configuration map.
	 * @param bootiqueVariable the name of the bootique variable.
	 * @param value the value.
	 * @throws Exception if a map cannot be created internally.
	 */
	public static void defineScalar(Map<String, Object> content, String bootiqueVariable, Object value) throws Exception {
		final var elements = bootiqueVariable.split("\\."); //$NON-NLS-1$
		final var entry = getScalarParent(content, elements);
		entry.put(elements[elements.length - 1], value);
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> getScalarParent(Map<String, Object> content, String[] elements) throws Exception {
		var entry = content;
		for (var i = 0; i < elements.length - 1; ++i) {
			final var val = entry.get(elements[i]);
			if (val instanceof Map<?, ?> mentry) {
				entry = (Map<String, Object>) mentry;
			} else {
				final var newElement = content.getClass().getDeclaredConstructor().newInstance();
				entry.put(elements[i], newElement);
				entry = newElement;
			}
		}
		assert entry != null;
		return entry;
	}

}
