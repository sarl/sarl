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

package io.sarl.lang.sarlc.modules.configs;

import static io.bootique.BQCoreModule.extend;
import static io.sarl.lang.sarlc.configs.SarlcConfig.BOOT_CLASSPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.CLASSPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.CLASS_OUTPUT_PATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.EXTRA_GENERATOR_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.MODULEPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.OUTPUT_PATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.TEMP_DIRECTORY_NAME;

import java.io.File;
import java.text.MessageFormat;

import javax.inject.Singleton;

import io.bootique.config.ConfigurationFactory;
import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Provides;
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
public class SarlcConfigModule implements BQModule {

	private static final String CLASSPATH_LONG_OPTION = "classpath"; //$NON-NLS-1$

	private static final String CLASSPATH_SHORT_OPTION = "cp"; //$NON-NLS-1$

	private static final String MODULEPATH_LONG_OPTION = "modulepath"; //$NON-NLS-1$

	private static final String MODULEPATH_SHORT_OPTION = "p"; //$NON-NLS-1$

	private static final String TEMP_DIR_OPTION = "tempdir"; //$NON-NLS-1$

	private static final String BOOTCLASSPATH_OPTION = "boot-classpath"; //$NON-NLS-1$

	private static final String GENERATOR_OPTION = "generator"; //$NON-NLS-1$

	@Override
	public void configure(Binder binder) {
		VariableDecls.extend(binder).declareVar(OUTPUT_PATH_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				Constants.SARL_OUTPUT_DIRECTORY_OPTION,
				MessageFormat.format(Messages.SarlcConfigModule_0, Constants.PROGRAM_NAME,
						Constants.SARL_OUTPUT_DIRECTORY_OPTION,
						Path.fromPortableString(SARLConfig.FOLDER_SOURCE_GENERATED).toFile().getPath()))
				.valueRequired(Messages.SarlcConfigModule_1)
				.build())
			.mapConfigPath(Constants.SARL_OUTPUT_DIRECTORY_OPTION, OUTPUT_PATH_NAME);

		VariableDecls.extend(binder).declareVar(CLASS_OUTPUT_PATH_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				Constants.JAVA_OUTPUT_DIRECTORY_OPTION,
				MessageFormat.format(Messages.SarlcConfigModule_6, Constants.PROGRAM_NAME,
						Constants.JAVA_OUTPUT_DIRECTORY_OPTION,
						Path.fromPortableString(SARLConfig.FOLDER_BIN).toFile().getPath()))
				.valueRequired(Messages.SarlcConfigModule_1)
				.build())
			.mapConfigPath(Constants.JAVA_OUTPUT_DIRECTORY_OPTION, CLASS_OUTPUT_PATH_NAME);

		VariableDecls.extend(binder).declareVar(TEMP_DIRECTORY_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				TEMP_DIR_OPTION, Messages.SarlcConfigModule_2)
				.valueRequired(Messages.SarlcConfigModule_1)
				.build())
			.mapConfigPath(TEMP_DIR_OPTION, TEMP_DIRECTORY_NAME);

		VariableDecls.extend(binder).declareVar(CLASSPATH_NAME);
		final String cpDescription = MessageFormat.format(Messages.SarlcConfigModule_3,
				VariableNames.toEnvironmentVariableName(CLASSPATH_NAME), CLASSPATH_SHORT_OPTION,
				CLASSPATH_LONG_OPTION);
		extend(binder).addOption(OptionMetadata.builder(
				CLASSPATH_LONG_OPTION, cpDescription)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build())
			.mapConfigPath(CLASSPATH_LONG_OPTION, CLASSPATH_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				CLASSPATH_SHORT_OPTION, cpDescription)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build())
			.mapConfigPath(CLASSPATH_SHORT_OPTION, CLASSPATH_NAME);

		VariableDecls.extend(binder).declareVar(MODULEPATH_NAME);
		final String mpDescription = MessageFormat.format(Messages.SarlcConfigModule_9,
				VariableNames.toEnvironmentVariableName(MODULEPATH_NAME), MODULEPATH_SHORT_OPTION,
				MODULEPATH_LONG_OPTION);
		extend(binder).addOption(OptionMetadata.builder(
				MODULEPATH_LONG_OPTION, mpDescription)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build())
			.mapConfigPath(MODULEPATH_LONG_OPTION, MODULEPATH_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				MODULEPATH_SHORT_OPTION, mpDescription)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build())
			.mapConfigPath(MODULEPATH_SHORT_OPTION, MODULEPATH_NAME);

		VariableDecls.extend(binder).declareVar(BOOT_CLASSPATH_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				BOOTCLASSPATH_OPTION,
				MessageFormat.format(Messages.SarlcConfigModule_5, File.pathSeparator))
				.valueRequired(Messages.SarlcConfigModule_4)
				.build())
			.mapConfigPath(BOOTCLASSPATH_OPTION, BOOT_CLASSPATH_NAME);

		VariableDecls.extend(binder).declareVar(EXTRA_GENERATOR_NAME);
		extend(binder).addOption(OptionMetadata.builder(
				GENERATOR_OPTION,
				MessageFormat.format(Messages.SarlcConfigModule_7,
						ExtraLanguageListCommand.EXTRA_LANGUAGE_LIST_OPTION_SHORT_NAME,
						File.pathSeparator))
				.valueRequired(Messages.SarlcConfigModule_8)
				.build())
			.mapConfigPath(GENERATOR_OPTION, EXTRA_GENERATOR_NAME);
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
