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

package io.sarl.maven.bqextension.configs;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URL;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;

/**
 * Constants and utilities for all factories.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public final class Configs {

	/** Prefix {@code sarl.} for properties.
	 */
	public static final String SARL_ROOT_PROPERTY_PREFIX = "sarl."; //$NON-NLS-1$

	/** Prefix {@code bq.} for properties.
	 */
	public static final String BOOTIQUE_PROPERTY_PREFIX = "bq."; //$NON-NLS-1$

	private Configs() {
		//
	}

	/** Replies the name of an property that corresponds to the name of a bootique variable.
	 *
	 * @param bootiqueVariable the name of the bootique variable.
	 * @return the name of the property.
	 */
	public static String toPropertyName(String bootiqueVariable) {
		if (Strings.isNullOrEmpty(bootiqueVariable)) {
			return null;
		}
		return BOOTIQUE_PROPERTY_PREFIX + bootiqueVariable;
	}

	/** Replies the name of an environment variable that corresponds to the name of a bootique variable.
	 *
	 * @param bootiqueVariable the name of the bootique variable.
	 * @return the name of the environment variable.
	 */
	public static String toEnvironmentVariableName(String bootiqueVariable) {
		if (Strings.isNullOrEmpty(bootiqueVariable)) {
			return null;
		}
		return bootiqueVariable.toUpperCase().replaceAll("[^a-zA-Z0-9_]+", "_"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Replies the name of an property that corresponds to the name of a bootique variable.
	 *
	 * @param bootiqueVariable the name of the bootique variable.
	 * @return the name of the property.
	 */
	public static String basename(String bootiqueVariable) {
		final int idx = bootiqueVariable.lastIndexOf('.');
		if (idx >= 0) {
			return bootiqueVariable.substring(idx + 1);
		}
		return bootiqueVariable;
	}

	/** Add a config to a Yaml configuration map.
	 *
	 * @param content the Yaml configuration map.
	 * @param config the configuration.
	 */
	public static void defineConfig(Map<String, Object> content, Config config) {
		assert content != null;
		assert config != null;
		final Class<?> type = config.getClass();
		final String sectionName = config.getSectionName();
		final Pattern setPattern = Pattern.compile("^set([A-Z])([a-zA-Z0-9]+)$"); //$NON-NLS-1$
		for (final Method setterMethod : type.getMethods()) {
			final Matcher matcher = setPattern.matcher(setterMethod.getName());
			if (matcher.matches()) {
				final String firstLetter = matcher.group(1);
				final String rest = matcher.group(2);
				final String getterName = "get" + firstLetter + rest; //$NON-NLS-1$
				Method getterMethod = null;
				try {
					getterMethod = type.getMethod(getterName);
				} catch (Throwable exception) {
					//
				}
				if (getterMethod != null && Modifier.isPublic(getterMethod.getModifiers())
						&& !Modifier.isAbstract(getterMethod.getModifiers())
						&& !Modifier.isStatic(getterMethod.getModifiers())) {
					try {
						final Object value = filterValue(getterMethod.getReturnType(), getterMethod.invoke(config));
						final String id = sectionName + "." + firstLetter.toLowerCase() + rest; //$NON-NLS-1$
						defineScalar(content, id, value);
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
		final String[] elements = bootiqueVariable.split("\\."); //$NON-NLS-1$
		final Map<String, Object> entry = getScalarParent(content, elements);
		entry.put(elements[elements.length - 1], value);
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> getScalarParent(Map<String, Object> content, String[] elements) throws Exception {
		Map<String, Object> entry = content;
		for (int i = 0; i < elements.length - 1; ++i) {
			final Object val = entry.get(elements[i]);
			if (val instanceof Map<?, ?>) {
				entry = (Map<String, Object>) val;
			} else {
				final Map<String, Object> newElement = content.getClass().newInstance();
				entry.put(elements[i], newElement);
				entry = newElement;
			}
		}
		assert entry != null;
		return entry;
	}

	/** Replies the identifier of the Yaml section.
	 *
	 * @param type the configuration type.
	 * @return the section name.
	 */
	public static String getSectionName(Class<? extends Config> type) {
		assert type != null;
		try {
			final Field field = type.getDeclaredField("PREFIX"); //$NON-NLS-1$
			if (Modifier.isPublic(field.getModifiers()) && String.class.equals(field.getType())) {
				final Object value = field.get(null);
				if (value != null) {
					return value.toString();
				}
			}
		} catch (Throwable exception) {
			//
		}
		throw new IllegalStateException();
	}

}
