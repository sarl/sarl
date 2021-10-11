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

package io.sarl.lang.sarlc.modules.commands;

import static io.bootique.BQCoreModule.extend;

import java.util.logging.Logger;

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
		extend(binder)
			.addOption(OptionMetadata.builder(
				CompilerCommand.PROGRESS_OPTION_NAME, Messages.CompilerCommandModule_0)
				.valueOptionalWithDefault(Boolean.TRUE.toString())
				.build())
			.mapConfigPath(CompilerCommand.PROGRESS_OPTION_NAME, ProgressBarConfig.ENABLE);

		extend(binder).addCommand(CompilerCommand.class);
	}

	/** Provide the command for running the compiler.
	 *
	 * @param compiler the compiler.
	 * @param configuration the SARLC configuration.
	 * @param pathDetector the detector of paths.
	 * @param commandConfig the configuration of the command.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public CompilerCommand provideSarlcCompilerCommand(Provider<SarlBatchCompiler> compiler,
			Provider<SarlcConfig> configuration, Provider<PathDetector> pathDetector,
			Provider<ProgressBarConfig> commandConfig) {
		return new CompilerCommand(compiler, configuration, pathDetector, commandConfig);
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
	public ProgressBarConfig getProgressBarConfig(ConfigurationFactory configFactory, Injector injector) {
		final ProgressBarConfig config = ProgressBarConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

	@Singleton
	@Provides
	public Logger provideRootLogger(ConfigurationFactory configFactory, Provider<ProgressBarConfig> config) {
		final Logger root = Logger.getAnonymousLogger();
		if (root != null) {
			ProgressBarConfig cfg = config.get();
			if (cfg.getEnable()) {
				root.setLevel(cfg.getLevel());
			}
		}
		return root;
	}

}
