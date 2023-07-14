/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
import static io.sarl.apputils.bootiqueapp.batchcompiler.lang.SARLRuntimeModule.SARL_INJECTOR_NAME;

import java.text.MessageFormat;
import java.util.logging.Logger;

import javax.inject.Named;
import javax.inject.Provider;
import javax.inject.Singleton;

import io.bootique.config.ConfigurationFactory;
import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Provides;
import io.bootique.meta.application.OptionMetadata;

import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.configs.ProgressBarConfig;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.tools.PathDetector;

/** Module for the compiler command.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class CompilerCommandModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		final String trueFalseValues = MessageFormat.format(Messages.CompilerCommandModule_1, Boolean.TRUE.toString(), Boolean.FALSE.toString());
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
	 * @param configuration the provider of the general sarlc configuration.
	 * @param pathDetector the provider of the path detector that is used by sarlc tool.
	 * @param commandConfig the provider of the sarlc progress bar configuration.
	 * @param guiceInjector injector that is used by the SARL compiler and that is different from the Bootique one.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public CompilerCommand provideSarlcCompilerCommand(Provider<SarlcConfig> configuration, Provider<PathDetector> pathDetector,
			Provider<ProgressBarConfig> commandConfig,
			@Named(SARL_INJECTOR_NAME) com.google.inject.Injector guiceInjector) {
		final com.google.inject.Provider<SarlBatchCompiler> compiler = guiceInjector.getProvider(SarlBatchCompiler.class);
		return new CompilerCommand(() -> compiler.get(), configuration, pathDetector, commandConfig);
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
		final ProgressBarConfig config = ProgressBarConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

	/** Replies the instance of the root logger.
	 *
	 * @param configFactory the CLI configuration.
	 * @param config the progress bar configuration.
	 * @return the root logger.
	 */
	@SuppressWarnings("static-method")
	@Singleton
	@Provides
	public Logger provideRootLogger(ConfigurationFactory configFactory, Provider<ProgressBarConfig> config) {
		final Logger root = Logger.getAnonymousLogger();
		if (root != null) {
			ProgressBarConfig cfg = config.get();
			if (cfg.getEnable()) {
				root.setLevel(cfg.getLevel().toJul());
			}
		}
		return root;
	}

}
