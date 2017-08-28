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

package io.sarl.lang.ui.generator.extra.preferences;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.StringTokenizer;

import com.google.inject.Inject;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.internal.ui.preferences.OptionsConfigurationBlock;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.editor.preferences.PreferenceConstants;
import org.eclipse.xtext.ui.editor.preferences.PreferenceStoreAccessImpl;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;

import io.sarl.lang.compilation.generator.extra.IExtraLanguageConversionInitializer;
import io.sarl.lang.ui.generator.extra.properties.AbstractGeneratorConfigurationBlock;

/** Preferences for the extra language generators.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ExtraLanguagePreferenceAccess {

	/** Key for saving the enabling state of the extra language generator.
	 */
	public static final String GENERATOR_PREFERENCE_TAG = "extraLanguageGenerator"; //$NON-NLS-1$

	/** Key for saving the enabling state of the extra language generator.
	 */
	public static final String ENABLED_PROPERTY = "enabled"; //$NON-NLS-1$

	/** Key for saving the type conversions.
	 */
	public static final String TYPE_CONVERSION_PROPERTY = "typeConversions"; //$NON-NLS-1$

	/** Key for saving the feature name conversions.
	 */
	public static final String FEATURE_NAME_CONVERSION_PROPERTY = "featureNameConversions"; //$NON-NLS-1$

	private static final String IS_PROJECT_SPECIFIC = "is_project_specific"; //$NON-NLS-1$

	private static final String PREFERENCE_SEPARATOR = ":";  //$NON-NLS-1$

	private PreferenceStoreAccessImpl preferenceStoreAccess;

	/** Change the preference accessor.
	 *
	 * <p>The parameter is a preference store implementation in order to have access to the correct preference set.
	 * It is an implementation choice from {@link OptionsConfigurationBlock}.
	 *
	 * @param preferenceStoreAccess the accessor.
	 */
	@Inject
	public void setPreferenceStoreAccess(PreferenceStoreAccessImpl preferenceStoreAccess) {
		this.preferenceStoreAccess = preferenceStoreAccess;
	}

	/** Replies the preference accessor.
	 *
	 * @return the accessor.
	 */
	@Inject
	public IPreferenceStoreAccess getPreferenceStoreAccess() {
		return this.preferenceStoreAccess;
	}

	/** Create a preference key according to the Xtext option block standards.
	 *
	 * @param pluginID the identifier of the plugin of the generator.
	 * @param preferenceName the name of the preference.
	 * @return the key.
	 */
	private static String getXtextKey(String pluginID, String preferenceName) {
		return GENERATOR_PREFERENCE_TAG + PreferenceConstants.SEPARATOR + pluginID
				+ PreferenceConstants.SEPARATOR + preferenceName;
	}

	/** Create a preference key.
	 *
	 * @param pluginID the identifier of the plugin of the generator.
	 * @param preferenceName the name of the preference.
	 * @return the key.
	 */
	public static String getPrefixedKey(String pluginID, String preferenceName) {
		return getXtextKey(getPropertyPrefix(pluginID), preferenceName);
	}

	/** Replies the preference value from the given store.
	 *
	 * @param store the preference storE.
	 * @param pluginID the identifier of the plugin of the generator.
	 * @param preferenceName the name of the preference.
	 * @return the key.
	 */
	public static String getString(IPreferenceStore store, String pluginID, String preferenceName) {
		return store.getString(getPrefixedKey(pluginID, preferenceName));
	}

	/** Replies the preference value from the given store.
	 *
	 * @param store the preference storE.
	 * @param pluginID the identifier of the plugin of the generator.
	 * @param preferenceName the name of the preference.
	 * @return the key.
	 */
	public static boolean getBoolean(IPreferenceStore store, String pluginID, String preferenceName) {
		return store.getBoolean(getPrefixedKey(pluginID, preferenceName));
	}

	/** Compute a property prefix.
	 *
	 * @param pluginID the plugin ID.
	 * @return the property prefix.
	 */
	public static String getPropertyPrefix(String pluginID) {
		if (pluginID == null) {
			return null;
		}
		return pluginID.replaceAll("[^a-zA-Z0-9_.]+", "_"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Replies the writable preference store to be used for the extra language generators.
	 *
	 * <p>This function does not test if the given project has a specific configuration.
	 * The project's configuration is systematically replied.
	 *
	 * @param project the context. If {@code null}, the global context is assumed.
	 * @return the modifiable preference store.
	 * @see #getPreferenceStore(IProject)
	 */
	public IPreferenceStore getWritablePreferenceStore(IProject project) {
		return getPreferenceStoreAccess().getWritablePreferenceStore(project);
	}

	/** Replies the readable preference store to be used for the extra language generators.
	 *
	 * <p>This function does not test if the given project has a specific configuration.
	 * The project's configuration is systematically replied.
	 *
	 * @param project the context. If {@code null}, the global context is assumed.
	 * @return the unmodifiable preference store.
	 * @see #getWritablePreferenceStore(IProject)
	 */
	public IPreferenceStore getPreferenceStore(IProject project) {
		return getPreferenceStoreAccess().getContextPreferenceStore(project);
	}

	/** Replies if the project has specific configuration for extra language generation provided by the given plugin.
	 *
	 * <p>This code is copied from {@link AbstractGeneratorConfigurationBlock} and its super types.
	 *
	 * @param pluginID the identifier of the extra language generator plugin.
	 * @param project the context.
	 * @return {@code true} if the given project has a specific configuration. {@code false} if
	 *     the general configuration should be used.
	 */
	public boolean hasProjectSpecificOptions(String pluginID, IProject project) {
		final IPreferenceStore store = getWritablePreferenceStore(project);
		// Compute the key
		String key = IS_PROJECT_SPECIFIC;
		if (pluginID != null) {
			key = getPropertyPrefix(pluginID) + "." + IS_PROJECT_SPECIFIC; //$NON-NLS-1$
		}
		// backward compatibility
		final boolean oldSettingsUsed = store.getBoolean(IS_PROJECT_SPECIFIC);
		final boolean newSettingsValue = store.getBoolean(key);
		if (oldSettingsUsed && !newSettingsValue) {
			store.setValue(key, true);
			return true;
		}
		return newSettingsValue;
	}

	/** Filter the project according to the specific configuration.
	 *
	 * <p>If the given project has a specific configuration, it is replied.
	 * Otherwise, {@code null} is replied.
	 *
	 * @param pluginID the identifier of the extra language generator plugin.
	 * @param project the context. If {@code null}, the global context is assumed.
	 * @return the unmodifiable preference store.
	 */
	public IProject ifSpecificConfiguration(String pluginID, IProject project) {
		if (project != null && hasProjectSpecificOptions(pluginID, project)) {
			return project;
		}
		return null;
	}

	/** Replies if the extr language generator is enabled.
	 *
	 * @param pluginID the identifier of the plugin that is associated to the generator.
	 * @param project the context.
	 * @return {@code true} if it is enabled.
	 */
	public boolean isGeneratorEnabled(String pluginID, IProject project) {
		final IProject prj = ifSpecificConfiguration(pluginID, project);
		final IPreferenceStore store = getPreferenceStore(prj);
		return getBoolean(store, pluginID, ENABLED_PROPERTY);
	}

	/** Parse the given input which is the preference string representation.
	 *
	 * @param input the string representation from the preferences.
	 * @param output the function to call for saving the parsed element. The first parameter is the element to be
	 *     converted. The second parameter is the target string.
	 * @return {@code true} if a data was parsed. {@code false} if no value was parsed.
	 */
	public static boolean parseConverterPreferenceValue(String input, Procedure2<String, String> output) {
		final StringTokenizer tokenizer = new StringTokenizer(input, PREFERENCE_SEPARATOR);
		String key = null;
		boolean foundValue = false;
		while (tokenizer.hasMoreTokens()) {
			final String token = tokenizer.nextToken();
			if (key != null) {
				output.apply(key,  token);
				foundValue = true;
				key = null;
			} else {
				key = token;
			}
		}
		return foundValue;
	}

	/** Generate the string representation of the type conversions in order to be
	 * saved into the preferences.
	 *
	 * @param input the type conversions.
	 * @return the string representation.
	 */
	public static String toConverterPreferenceValue(Iterator<String> input) {
		final StringBuilder builder = new StringBuilder();
		while (input.hasNext()) {
			final String value = input.next();
			if (builder.length() > 0) {
				builder.append(PREFERENCE_SEPARATOR);
			}
			builder.append(value);
		}
		return builder.toString();
	}

	/** Generate the string representation of the type conversions in order to be
	 * saved into the preferences.
	 *
	 * @param input the type conversions.
	 * @return the string representation.
	 */
	public static String toConverterPreferenceValue(Map<String, String> input) {
		return toConverterPreferenceValue(new ConversionIterator(input));
	}

	/** Generate the string representation of the type conversions in order to be
	 * saved into the preferences.
	 *
	 * @param input the type conversions.
	 * @return the string representation.
	 */
	public static String toConverterPreferenceValue(IExtraLanguageConversionInitializer input) {
		final StringBuilder builder = new StringBuilder();
		input.initializeConversions((baseName, source, target) -> {
			if (builder.length() > 0) {
				builder.append(PREFERENCE_SEPARATOR);
			}
			builder.append(source);
			builder.append(PREFERENCE_SEPARATOR);
			builder.append(target);
		});
		return builder.toString();
	}

	/** Iterator on conversions.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class ConversionIterator implements Iterator<String> {

		private final Iterator<Entry<String, String>> entries;

		/** Constructor.
		 *
		 * @param conversions the mapping definition.
		 */
		public ConversionIterator(Map<String, String> conversions) {
			this.entries = conversions.entrySet().iterator();
		}

		@Override
		public boolean hasNext() {
			return this.entries.hasNext();
		}

		@Override
		public String next() {
			final Entry<String, String> entry = this.entries.next();
			final String source = entry.getKey();
			final String target = entry.getValue();
			return source + PREFERENCE_SEPARATOR + target;
		}

	}

}
