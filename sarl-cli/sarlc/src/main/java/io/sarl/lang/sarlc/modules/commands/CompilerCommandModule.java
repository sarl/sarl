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

package io.sarl.lang.sarlc.modules.commands;

import static io.bootique.BQCoreModule.extend;

import java.io.IOException;
import java.text.MessageFormat;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import io.bootique.BQModule;
import io.bootique.config.ConfigurationFactory;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Provides;
import io.bootique.meta.application.OptionMetadata;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.configs.ProgressBarConfig;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.tools.PathDetector;
import jakarta.inject.Provider;
import jakarta.inject.Singleton;

/** Module for the compiler command.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarlc 0.15.1 20250911-224827
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarlc
 * @since 0.8
 */
public class CompilerCommandModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		final var trueFalseValues = MessageFormat.format(Messages.CompilerCommandModule_1, Boolean.TRUE.toString(), Boolean.FALSE.toString());
		extend(binder)
		.addOption(OptionMetadata.builder(
				CompilerCommand.PROGRESS_OPTION_NAME, MessageFormat.format(Messages.CompilerCommandModule_0, Boolean.TRUE))
				.valueOptionalWithDefault(trueFalseValues, Boolean.TRUE.toString())
				.build())
		.mapConfigPath(CompilerCommand.PROGRESS_OPTION_NAME, ProgressBarConfig.ENABLE);
		extend(binder).addCommand(CompilerCommand.class);
	}

	/** Provide the command for running the compiler.
	 *
	 * @param sarlCompiler the provider of a SARL batch compiler.
	 * @param configuration the provider of the general sarlc configuration.
	 * @param commandConfig the provider of the sarlc progress bar configuration.
	 * @param pathDetector the provider of the path detector that is used by sarlc tool.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public CompilerCommand provideSarlcCompilerCommand(
			Provider<SarlBatchCompiler> sarlCompiler,
			Provider<SarlcConfig> configuration,
			Provider<ProgressBarConfig> commandConfig,
			Provider<PathDetector> pathDetector) {
		return new CompilerCommand(sarlCompiler, configuration, pathDetector, commandConfig);
	}

	/** Replies the instance of the compiler command configuration.
	 *
	 * @param configFactory accessor to the bootique factory.
	 * @param injector the current injector.
	 * @return the compiler command configuration accessor.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public ProgressBarConfig provideProgressBarConfig(ConfigurationFactory configFactory, Injector injector) {
		final var config = ProgressBarConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

	/** Replies the instance of the root logger.
	 *
	 * @param configFactory the CLI configuration.
	 * @param config the progress bar configuration.
	 * @return the root logger.
	 */
	@Singleton
	@Provides
	public Logger provideRootLogger(ConfigurationFactory configFactory, Provider<ProgressBarConfig> config) {
		final var type = getClass();
		try (final var stream = type.getResourceAsStream("logging.properties")) { //$NON-NLS-1$
			LogManager.getLogManager().readConfiguration(stream);
			final var root = Logger.getAnonymousLogger();
			if (root != null) {
				final var cfg = config.get();
				if (cfg.getEnable()) {
					root.setLevel(cfg.getLevel().toJul());
				}
			}
			return root;
		} catch (IOException ex) {
			throw new Error(ex);
		}
	}

}
