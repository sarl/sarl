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

package io.sarl.lang.extralanguage.compiler;

import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Pattern;
import javax.inject.Inject;

import io.sarl.lang.core.Agent;

/** Converter from Jvm type to the extra language type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public final class ExtraLanguageTypeConverter {

	private static final String PACKAGE_SEPARATOR = "."; //$NON-NLS-1$

	private static final String IMPLICIT_PACKAGE;

	private final IExtraLanguageConversionInitializer initializer;

	private final IExtraLanguageGeneratorContext context;

	@Inject
	private TypeConverterRuleReader converterReader;

	private Map<String, String> mapping;

	private boolean isImplicitSarlTypes = true;

	private boolean isImplicitJvmTypes;

	static {
		final String[] basePackage = Agent.class.getPackage().getName().split(Pattern.quote(PACKAGE_SEPARATOR));
		final StringBuilder name = new StringBuilder();
		for (int i = 0; i < 2 && i < basePackage.length; ++i) {
			name.append(basePackage[i]).append(PACKAGE_SEPARATOR);
		}
		IMPLICIT_PACKAGE = name.toString();
	}

	/** Constructor.
	 *
	 * @param initializer the initializer.
	 * @param context the generation context.
	 */
	public ExtraLanguageTypeConverter(IExtraLanguageConversionInitializer initializer, IExtraLanguageGeneratorContext context) {
		this.initializer = initializer;
		this.context = context;
	}

	/** Replies if the SARL types ({@code io.sarl.*}) are implicitly supported by this converter.
	 *
	 * @return {@code true} if the implicit types are converted. {@code false} if no implicit type is converted.
	 * @since 0.8
	 * @see #isImplicitSarlTypes()
	 */
	public boolean isImplicitSarlTypes() {
		return this.isImplicitSarlTypes;
	}

	/** Set if the SARL types ({@code io.sarl.*}) are implicitly supported by this converter.
	 *
	 * @param enable {@code true} if the implicit types are converted. {@code false} if no implicit type is converted.
	 * @since 0.8
	 * @see #setImplicitJvmTypes(boolean)
	 */
	public void setImplicitSarlTypes(boolean enable) {
		this.isImplicitSarlTypes = enable;
	}

	/** Replies if the JVM types (not in {@code io.sarl.*}) are implicitly supported by this converter.
	 *
	 * @return {@code true} if the implicit types are converted. {@code false} if no implicit type is converted.
	 * @since 0.8
	 * @see #isImplicitSarlTypes()
	 */
	public boolean isImplicitJvmTypes() {
		return this.isImplicitJvmTypes;
	}

	/** Set if the JVM types (not in {@code io.sarl.*}) are implicitly supported by this converter.
	 *
	 * @param enable {@code true} if the implicit types are converted. {@code false} if no implicit type is converted.
	 * @since 0.8
	 * @see #setImplicitSarlTypes(boolean)
	 */
	public void setImplicitJvmTypes(boolean enable) {
		this.isImplicitJvmTypes = enable;
	}

	/** Reset the mapping definition to its default content.
	 */
	public void reset() {
		this.mapping = null;
	}

	/** Build the mapping table.
	 *
	 * @return the mapping table.
	 */
	protected Map<String, String> initMapping() {
		final Map<String, String> map = new TreeMap<>();
		if (!this.converterReader.initializeConversions(map, this.context) && this.initializer != null) {
			this.initializer.initializeConversions((simpleName, source, target) -> {
				map.put(source,  target);
			});
		}
		return map;
	}

	/** Indicates if the given name has a mapping to the extra language.
	 *
	 * @param type the type to convert.
	 * @return {@code true} if the mapping exists.
	 */
	public boolean hasConversion(String type) {
		if ((isImplicitSarlTypes() && type.startsWith(IMPLICIT_PACKAGE))
			|| isImplicitJvmTypes()) {
			return true;
		}
		if (this.mapping == null) {
			this.mapping = initMapping();
		}
		return this.mapping.containsKey(type);
	}

	/** Convert the given type to its equivalent in the extra language.
	 *
	 * @param type the type to convert.
	 * @return the conversion result, or {@code null} if no equivalent exist.
	 */
	public String convert(String type) {
		if (isImplicitSarlTypes() && type.startsWith(IMPLICIT_PACKAGE)) {
			return type;
		}
		if (this.mapping == null) {
			this.mapping = initMapping();
		}
		final String map = this.mapping.get(type);
		if (map != null) {
			if (map.isEmpty() && !isImplicitJvmTypes()) {
				return null;
			}
			return map;
		}
		if (isImplicitJvmTypes()) {
			return type;
		}
		return null;
	}

	/** Reader of the conversion rules.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class TypeConverterRuleReader {

		/** initialize the conversions mapping.
		 *
		 * @param result the result.
		 * @param context the generation context.
		 * @return {@code true} if rules are read.
		 */
		@SuppressWarnings("static-method")
		public boolean initializeConversions(Map<String, String> result, IExtraLanguageGeneratorContext context) {
			return false;
		}

	}

}
