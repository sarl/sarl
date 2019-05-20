/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.sarlc.modules.configs;

import static io.bootique.BQCoreModule.extend;
import static io.sarl.lang.sarlc.configs.SarlcConfig.BOOT_CLASSPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.CLASSPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.CLASS_OUTPUT_PATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.EXTRA_GENERATOR_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.OUTPUT_PATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.WORKING_PATH_NAME;

import java.io.File;
import java.text.MessageFormat;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.bootique.config.ConfigurationFactory;
import io.bootique.meta.application.OptionMetadata;
import org.arakhne.afc.bootique.variables.VariableDecls;
import org.arakhne.afc.bootique.variables.VariableNames;
import org.eclipse.core.runtime.Path;

import io.sarl.lang.SARLConfig;
import io.sarl.lang.sarlc.Constants;
import io.sarl.lang.sarlc.commands.ExtraLanguageListCommand;
import io.sarl.lang.sarlc.configs.SarlcConfig;

/**
 * Module for creating and configuring the general/root sarlc configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class SarlcConfigModule extends AbstractModule {

	private static final String CLASSPATH_LONG_OPTION = "classpath"; //$NON-NLS-1$

	private static final String CLASSPATH_SHORT_OPTION = "cp"; //$NON-NLS-1$

	@Override
	protected void configure() {
		VariableDecls.extend(binder()).declareVar(OUTPUT_PATH_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				Constants.SARL_OUTPUT_DIRECTORY_OPTION,
				MessageFormat.format(Messages.SarlcConfigModule_0, Constants.PROGRAM_NAME,
						Constants.SARL_OUTPUT_DIRECTORY_OPTION,
						Path.fromPortableString(SARLConfig.FOLDER_SOURCE_GENERATED).toFile().getPath()))
				.configPath(OUTPUT_PATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_1)
				.build());

		VariableDecls.extend(binder()).declareVar(CLASS_OUTPUT_PATH_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				Constants.JAVA_OUTPUT_DIRECTORY_OPTION,
				MessageFormat.format(Messages.SarlcConfigModule_6, Constants.PROGRAM_NAME,
						Constants.JAVA_OUTPUT_DIRECTORY_OPTION,
						Path.fromPortableString(SARLConfig.FOLDER_BIN).toFile().getPath()))
				.configPath(CLASS_OUTPUT_PATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_1)
				.build());

		VariableDecls.extend(binder()).declareVar(WORKING_PATH_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"workingdir", Messages.SarlcConfigModule_2) //$NON-NLS-1$
				.configPath(WORKING_PATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_1)
				.build());

		VariableDecls.extend(binder()).declareVar(CLASSPATH_NAME);
		final String cpDescription = MessageFormat.format(Messages.SarlcConfigModule_3,
				VariableNames.toEnvironmentVariableName(CLASSPATH_NAME), CLASSPATH_SHORT_OPTION,
				CLASSPATH_LONG_OPTION);
		extend(binder()).addOptions(OptionMetadata.builder(
				CLASSPATH_LONG_OPTION, cpDescription)
				.configPath(CLASSPATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build(),
				OptionMetadata.builder(
				CLASSPATH_SHORT_OPTION, cpDescription)
				.configPath(CLASSPATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build());

		VariableDecls.extend(binder()).declareVar(BOOT_CLASSPATH_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"bootclasspath", //$NON-NLS-1$
				MessageFormat.format(Messages.SarlcConfigModule_5, File.pathSeparator))
				.configPath(BOOT_CLASSPATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build());

		VariableDecls.extend(binder()).declareVar(EXTRA_GENERATOR_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"generator", //$NON-NLS-1$
				MessageFormat.format(Messages.SarlcConfigModule_7,
						ExtraLanguageListCommand.EXTRA_LANGUAGE_LIST_OPTION_SHORT_NAME,
						File.pathSeparator))
				.configPath(EXTRA_GENERATOR_NAME)
				.valueRequired(Messages.SarlcConfigModule_8)
				.build());

	}

	/** Replies the instance of the sarl configuration.
	 *
	 * @param configFactory accessor to the bootique factory.
	 * @param injector the current injector.
	 * @return the path configuration accessor.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public SarlcConfig getSarlcConfig(ConfigurationFactory configFactory, Injector injector) {
		final SarlcConfig config = SarlcConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

}
