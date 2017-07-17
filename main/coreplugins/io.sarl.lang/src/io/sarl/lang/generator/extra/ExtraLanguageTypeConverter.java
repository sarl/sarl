/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.generator.extra;

import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

import javax.inject.Inject;

/** Converter from Jvm type to the extra language type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public final class ExtraLanguageTypeConverter {

	private final IExtraLanguageConversionInitializer initializer;

	private final IExtraLanguageGeneratorContext context;

	private final String pluginID;

	@Inject
	private TypeConverterRuleReader converterReader;

	private Map<String, String> mapping;

	/** Constructor.
	 *
	 * @param initializer the initializer.
	 * @param pluginID the identifier of the generator's plugin.
	 * @param context the generation ccontext.
	 */
	public ExtraLanguageTypeConverter(IExtraLanguageConversionInitializer initializer,
			String pluginID, IExtraLanguageGeneratorContext context) {
		this.initializer = initializer;
		this.context = context;
		this.pluginID = pluginID;
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
		if (!this.converterReader.initializeConversions(map, this.pluginID, this.context) && this.initializer != null) {
			this.initializer.initializeConversions((simpleName, source, target) -> {
				map.put(source,  target);
			});
		}
		return map;
	}

	/** Replies the type mapping.
	 *
	 * @return unmodifiable map of the type mapping.
	 */
	public Map<String, String> getMapping() {
		if (this.mapping == null) {
			this.mapping = initMapping();
		}
		return Collections.unmodifiableMap(this.mapping);
	}

	/** Indicates if the given name has a mapping to the extra language.
	 *
	 * @param type the type to convert.
	 * @return {@code true} if the mapping exists.
	 */
	public boolean hasConversion(String type) {
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
		if (this.mapping == null) {
			this.mapping = initMapping();
		}
		final String map = this.mapping.get(type);
		if (map != null) {
			if (map.isEmpty()) {
				return null;
			}
			return map;
		}
		return type;
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
		 * @param pluginID the identifier of the generator's plugin.
		 * @param context the generation context.
		 * @return {@code true} if rules are read.
		 */
		@SuppressWarnings("static-method")
		public boolean initializeConversions(Map<String, String> result, String pluginID,
				IExtraLanguageGeneratorContext context) {
			return false;
		}

	}

}
