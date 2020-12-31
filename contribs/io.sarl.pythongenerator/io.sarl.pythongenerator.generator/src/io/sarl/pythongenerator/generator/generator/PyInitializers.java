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

package io.sarl.pythongenerator.generator.generator;

import java.util.List;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.extralanguage.compiler.AbstractExtraLanguageGenerator;
import io.sarl.lang.extralanguage.compiler.IExtraLanguageConversionInitializer;
import io.sarl.pythongenerator.generator.PyGeneratorPlugin;

/** Initializers for Python 3.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
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

	private static List<Pair<String, String>> loadPropertyFile(String filename) {
		return AbstractExtraLanguageGenerator.loadPropertyFile(filename,
			PyGeneratorPlugin.getDefault(),
			PyInitializers.class,
			exception -> PyGeneratorPlugin.getDefault().createStatus(IStatus.ERROR, exception));
	}

	/** Replies the initializer for the type converter.
	 *
	 * @return the initializer.
	 */
	public static IExtraLanguageConversionInitializer getTypeConverterInitializer() {
		return it -> {
			final List<Pair<String, String>> properties = loadPropertyFile(TYPE_CONVERSION_FILENAME);
			if (!properties.isEmpty()) {
				for (final Pair<String, String> entry : properties) {
					final String source = Objects.toString(entry.getKey());
					final String target = Objects.toString(entry.getValue());
					final String baseName;
					final Matcher matcher = TYPE_NAME_PAT.matcher(source);
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
	 * @return the initializer.
	 */
	public static IExtraLanguageConversionInitializer getFeatureNameConverterInitializer() {
		return it -> {
			final List<Pair<String, String>> properties = loadPropertyFile(FEATURE_CONVERSION_FILENAME);
			if (!properties.isEmpty()) {
				for (final Pair<String, String> entry : properties) {
					final String source = Objects.toString(entry.getKey());
					final String target = Objects.toString(entry.getValue());
					final Matcher matcher = FEATURE_NAME_PAT.matcher(source);
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
