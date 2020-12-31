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

package io.sarl.lang.ui.preferences;

import java.util.Objects;
import java.util.Set;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.inject.Injector;
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

import io.sarl.lang.SARLConfig;
import io.sarl.lang.ui.internal.LangActivator;

/** Utilities related to the preferences related to SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SARLPreferences {

	/** Define the configuration entry for project specific entries.
	 *
	 * @see OptionsConfigurationBlock#IS_PROJECT_SPECIFIC
	 */
	public static final String IS_PROJECT_SPECIFIC = "is_project_specific"; //$NON-NLS-1$

	private SARLPreferences() {
		//
	}

	/** Replies the preference store for the given project.
	 *
	 * @param project the project.
	 * @return the preference store or {@code null}.
	 */
	public static IPreferenceStore getSARLPreferencesFor(IProject project) {
		if (project != null) {
			final Injector injector = LangActivator.getInstance().getInjector(LangActivator.IO_SARL_LANG_SARL);
			final IPreferenceStoreAccess preferenceStoreAccess = injector.getInstance(IPreferenceStoreAccess.class);
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
		final Injector injector = LangActivator.getInstance().getInjector(LangActivator.IO_SARL_LANG_SARL);
		final EclipseOutputConfigurationProvider configurationProvider =
				injector.getInstance(EclipseOutputConfigurationProvider.class);
		return configurationProvider.getOutputConfigurations(project);
	}

	/** Configure the given project for using the system-wide
	 * configuration related to SARL.
	 *
	 * @param project the project.
	 */
	public static void setSystemSARLConfigurationFor(IProject project) {
		final IPreferenceStore preferenceStore = getSARLPreferencesFor(project);
		preferenceStore.setValue(IS_PROJECT_SPECIFIC, false);
	}

	/** Configure the given project for using a specific configuration
	 * related to SARL.
	 *
	 * @param project the project; never {@code null}.
	 * @param outputPath the path where SARL compiler is generating the Java code; may be {@code null}
	 *      if {@code testOutputPath} is not {@code null}.
	 * @param testOutputPath the path where SARL compiler is generating the Java code for testing; may be {@code null}
	 *      if {@code outputPath} is not {@code null}.
	 * @since 0.11
	 */
	public static void setSpecificSARLConfigurationFor(
			IProject project,
			IPath outputPath, IPath testOutputPath) {
		assert project != null;

		final IPreferenceStore preferenceStore = getSARLPreferencesFor(project);
		// Force to use a specific configuration for the SARL
		preferenceStore.setValue(IS_PROJECT_SPECIFIC, true);

		// Loop on the Xtext configurations embedded in the SARL compiler.
		String key;
		for (final OutputConfiguration projectConfiguration : getXtextConfigurationsFor(project)) {
			final String name = projectConfiguration.getName();

			//
			// OUTPUT PATH
			key = BuilderPreferenceAccess.getKey(
					projectConfiguration,
					EclipseOutputConfigurationProvider.OUTPUT_DIRECTORY);
			if (Objects.equals(name, SARLConfig.TEST_OUTPUT_CONFIGURATION)) {
				if (testOutputPath != null) {
					preferenceStore.setValue(key, testOutputPath.toOSString());
				}
			} else if (Objects.equals(name, IFileSystemAccess.DEFAULT_OUTPUT)) {
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
	 * @return the output path for SARL compiler if the project has a specific configuration,
	 *     otherwise {@code null}.
	 */
	public static IPath getSARLOutputPathFor(IProject project) {
		assert project != null;
		final IPreferenceStore preferenceStore = getSARLPreferencesFor(project);
		if (preferenceStore.getBoolean(IS_PROJECT_SPECIFIC)) {
			final OutputConfiguration projectConfiguration = Iterables.find(
				getXtextConfigurationsFor(project),
				it -> Objects.equals(it.getName(), IFileSystemAccess.DEFAULT_OUTPUT));
			if (projectConfiguration != null) {
				final String key = BuilderPreferenceAccess.getKey(
						projectConfiguration,
						EclipseOutputConfigurationProvider.OUTPUT_DIRECTORY);
				final String path = preferenceStore.getString(key);
				if (!Strings.isNullOrEmpty(path)) {
					return Path.fromOSString(path);
				}
			}
		}
		return null;
	}

	/** Replies the SARL output path in the global preferences.
	 *
	 * @return the output path for SARL compiler in the global preferences.
	 */
	public static IPath getGlobalSARLOutputPath() {
		final Injector injector = LangActivator.getInstance().getInjector(LangActivator.IO_SARL_LANG_SARL);
		final IOutputConfigurationProvider configurationProvider =
				injector.getInstance(IOutputConfigurationProvider.class);
		final OutputConfiguration config = Iterables.find(
			configurationProvider.getOutputConfigurations(),
			it -> Objects.equals(it.getName(), IFileSystemAccess.DEFAULT_OUTPUT));
		if (config != null) {
			final String path = config.getOutputDirectory();
			if (!Strings.isNullOrEmpty(path)) {
				final IPath pathObject = Path.fromOSString(path);
				if (pathObject != null) {
					return pathObject;
				}
			}
		}
		throw new IllegalStateException("No global preferences found for SARL."); //$NON-NLS-1$
	}

	/** Replies the SARL output path for test in the global preferences.
	 *
	 * @return the output path for SARL compiler in the global preferences.
	 * @since 0.8
	 */
	public static IPath getGlobalSARLTestOutputPath() {
		final Injector injector = LangActivator.getInstance().getInjector(LangActivator.IO_SARL_LANG_SARL);
		final IOutputConfigurationProvider configurationProvider =
				injector.getInstance(IOutputConfigurationProvider.class);
		final OutputConfiguration config = Iterables.find(
			configurationProvider.getOutputConfigurations(),
			it -> Objects.equals(it.getName(), SARLConfig.TEST_OUTPUT_CONFIGURATION));
		if (config != null) {
			final String path = config.getOutputDirectory();
			if (!Strings.isNullOrEmpty(path)) {
				final IPath pathObject = Path.fromOSString(path);
				if (pathObject != null) {
					return pathObject;
				}
			}
		}
		throw new IllegalStateException("No global preferences found for SARL."); //$NON-NLS-1$
	}

}
