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

package io.sarl.bspl.lang.ui.preferences;

import java.util.Objects;
import java.util.Set;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.xtext.builder.EclipseOutputConfigurationProvider;
import org.eclipse.xtext.builder.preferences.BuilderPreferenceAccess;
import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.ui.editor.preferences.IPreferenceStoreAccess;
import org.eclipse.xtext.ui.preferences.OptionsConfigurationBlock;

import io.sarl.bspl.lang.ui.internal.LangActivator;

/** Utilities related to the preferences related to BSPL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("restriction")
public final class BSPLPreferences {

	/** Define the configuration entry for project specific entries.
	 *
	 * @see OptionsConfigurationBlock#IS_PROJECT_SPECIFIC
	 */
	public static final String IS_PROJECT_SPECIFIC = "is_project_specific"; //$NON-NLS-1$

	private BSPLPreferences() {
		//
	}

	/** Replies the preference store for the given project.
	 *
	 * @param project the project.
	 * @return the preference store or {@code null}.
	 */
	public static IPreferenceStore getBSPLPreferencesFor(IProject project) {
		if (project != null) {
			final var injector = LangActivator.getInstance().getInjector(LangActivator.IO_SARL_BSPL_LANG_BSPL);
			final var preferenceStoreAccess = injector.getInstance(IPreferenceStoreAccess.class);
			return preferenceStoreAccess.getWritablePreferenceStore(project);
		}
		return null;
	}

	/** Replies the Xtext output configurations related to the given project.
	 *
	 * @param project the project.
	 * @return the Xtext output configurations.
	 */
	public static Set<OutputConfiguration> getXtextConfigurationsFor(IProject project) {
		final var injector = LangActivator.getInstance().getInjector(LangActivator.IO_SARL_BSPL_LANG_BSPL);
		final var configurationProvider = injector.getInstance(EclipseOutputConfigurationProvider.class);
		return configurationProvider.getOutputConfigurations(project);
	}

	/** Configure the given project for using the system-wide
	 * configuration related to BSPL.
	 *
	 * @param project the project.
	 */
	public static void setSystemBSPLConfigurationFor(IProject project) {
		final var preferenceStore = getBSPLPreferencesFor(project);
		preferenceStore.setValue(IS_PROJECT_SPECIFIC, false);
	}

	/** Configure the given project for using a specific configuration
	 * related to BSPL.
	 *
	 * @param project the project; never {@code null}.
	 * @param outputPath the path where BSPL compiler is generating the SARL code; may be {@code null}
	 *      if {@code testOutputPath} is not {@code null}.
	 */
	public static void setSpecificBSPLConfigurationFor(IProject project, IPath outputPath) {
		assert project != null;

		final var preferenceStore = getBSPLPreferencesFor(project);
		// Force to use a specific configuration for the BSPL
		preferenceStore.setValue(IS_PROJECT_SPECIFIC, true);

		// Loop on the Xtext configurations embedded in the BSPL compiler.
		String key;
		for (final var projectConfiguration : getXtextConfigurationsFor(project)) {
			final var name = projectConfiguration.getName();

			//
			// OUTPUT PATH
			key = BuilderPreferenceAccess.getKey(
					projectConfiguration,
					EclipseOutputConfigurationProvider.OUTPUT_DIRECTORY);
			if (Objects.equals(name, IFileSystemAccess.DEFAULT_OUTPUT)) {
				if (outputPath != null) {
					preferenceStore.setValue(key, outputPath.toOSString());
				}
			}

			//
			// CREATE THE OUTPUT DIRECTORY
			key = BuilderPreferenceAccess.getKey(
					projectConfiguration,
					EclipseOutputConfigurationProvider.OUTPUT_CREATE_DIRECTORY);
			preferenceStore.setValue(key, true);

			//
			// OVERWRITE THE EXISTING FILES
			key = BuilderPreferenceAccess.getKey(
					projectConfiguration,
					EclipseOutputConfigurationProvider.OUTPUT_OVERRIDE);
			preferenceStore.setValue(key, true);

			//
			// SET GENERATED FILES AS DERIVED
			key = BuilderPreferenceAccess.getKey(
					projectConfiguration,
					EclipseOutputConfigurationProvider.OUTPUT_DERIVED);
			preferenceStore.setValue(key, true);

			//
			// CLEAN THE GENERATED FILES
			key = BuilderPreferenceAccess.getKey(
					projectConfiguration,
					EclipseOutputConfigurationProvider.OUTPUT_CLEANUP_DERIVED);
			preferenceStore.setValue(key, true);

			//
			// CLEAN THE OUTPUT DIRECTORY
			key = BuilderPreferenceAccess.getKey(
					projectConfiguration,
					EclipseOutputConfigurationProvider.OUTPUT_CLEAN_DIRECTORY);
			preferenceStore.setValue(key, true);
		}
	}

	/** Replies the output path for the generated sources that is registered inside the project's preferences.
	 * If the project has no specific configuration, replies {@code null}.
	 *
	 * @param project the project.
	 * @return the output path for BSPL compiler if the project has a specific configuration,
	 *     otherwise {@code null}.
	 */
	public static IPath getBSPLOutputPathFor(IProject project) {
		assert project != null;
		final var preferenceStore = getBSPLPreferencesFor(project);
		if (preferenceStore.getBoolean(IS_PROJECT_SPECIFIC)) {
			final var projectConfiguration = Iterables.find(
				getXtextConfigurationsFor(project),
				it -> Objects.equals(it.getName(), IFileSystemAccess.DEFAULT_OUTPUT));
			if (projectConfiguration != null) {
				final var key = BuilderPreferenceAccess.getKey(
						projectConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_DIRECTORY);
				final var path = preferenceStore.getString(key);
				if (!Strings.isNullOrEmpty(path)) {
					return Path.fromOSString(path);
				}
			}
		}
		return null;
	}

	/** Replies the BSPL output path in the global preferences.
	 *
	 * @return the output path for BSPL compiler in the global preferences.
	 */
	public static IPath getGlobalBSPLOutputPath() {
		final var injector = LangActivator.getInstance().getInjector(LangActivator.IO_SARL_BSPL_LANG_BSPL);
		final var configurationProvider =
				injector.getInstance(IOutputConfigurationProvider.class);
		final var config = Iterables.find(
			configurationProvider.getOutputConfigurations(),
			it -> Objects.equals(it.getName(), IFileSystemAccess.DEFAULT_OUTPUT));
		if (config != null) {
			final var path = config.getOutputDirectory();
			if (!Strings.isNullOrEmpty(path)) {
				final var pathObject = Path.fromOSString(path);
				if (pathObject != null) {
					return pathObject;
				}
			}
		}
		throw new IllegalStateException("No global preferences found for BSPL."); //$NON-NLS-1$
	}

}
