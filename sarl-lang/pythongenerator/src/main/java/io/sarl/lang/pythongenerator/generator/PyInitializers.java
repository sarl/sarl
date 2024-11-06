/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.pythongenerator.generator;

import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.extralanguage.compiler.AbstractExtraLanguageGenerator;
import io.sarl.lang.extralanguage.compiler.AbstractExtraLanguageGenerator.ExtraLanguageSupportModule;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageConversionInitializer;

/** Initializers for Python 3.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version pythongenerator 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid pythongenerator
 * @since 0.6
 */
public final class PyInitializers {

	private static final String FEATURE_NAME_PATTERN = "^.*\\.([a-zA-Z_$*]+)(?:\\(.*?\\))?$"; //$NON-NLS-1$

	private static final Pattern FEATURE_NAME_PAT = Pattern.compile(FEATURE_NAME_PATTERN);

	private static final String TYPE_NAME_PATTERN = "^.*[.$]([^.$]+)$"; //$NON-NLS-1$

	private static final Pattern TYPE_NAME_PAT = Pattern.compile(TYPE_NAME_PATTERN);

	private static final String TYPE_CONVERSION_FILENAME = "conversions/type_conversion.properties"; //$NON-NLS-1$

	private static final String FEATURE_CONVERSION_FILENAME = "conversions/feature_name_conversion.properties"; //$NON-NLS-1$

	private PyInitializers() {
		//
	}

	private static List<Pair<String, String>> loadPropertyFile(String filename, ExtraLanguageSupportModule supportModule) {
		return AbstractExtraLanguageGenerator.loadPropertyFile(filename, supportModule, PyInitializers.class);
	}

	/** Replies the initializer for the type converter.
	 *
	 * @param supportModule the support module that is providing the accessors to the plugin components.
	 * @return the initializer.
	 * @since 0.14
	 */
	public static IExtraLanguageConversionInitializer getTypeConverterInitializer(ExtraLanguageSupportModule supportModule) {
		return it -> {
			final var properties = loadPropertyFile(TYPE_CONVERSION_FILENAME, supportModule);
			if (!properties.isEmpty()) {
				for (final var entry : properties) {
					final var source = Objects.toString(entry.getKey());
					final var target = Objects.toString(entry.getValue());
					final String baseName;
					final var matcher = TYPE_NAME_PAT.matcher(source);
					if (matcher.find()) {
						baseName = matcher.group(1);
					} else {
						baseName = source;
					}
					it.apply(baseName, source, target);
				}
			}
		};
	}

	/** Replies the initializer for the feature name converter.
	 *
	 * @param supportModule the support module that is providing the accessors to the plugin components.
	 * @return the initializer.
	 * @since 0.14
	 */
	public static IExtraLanguageConversionInitializer getFeatureNameConverterInitializer(ExtraLanguageSupportModule supportModule) {
		return it -> {
			final var properties = loadPropertyFile(FEATURE_CONVERSION_FILENAME, supportModule);
			if (!properties.isEmpty()) {
				for (final var entry : properties) {
					final var source = Objects.toString(entry.getKey());
					final var target = Objects.toString(entry.getValue());
					final var matcher = FEATURE_NAME_PAT.matcher(source);
					final String featureName;
					if (matcher.find()) {
						featureName = matcher.group(1);
					} else {
						featureName = source;
					}
					it.apply(featureName, source, target);
				}
			}
		};
	}

}
