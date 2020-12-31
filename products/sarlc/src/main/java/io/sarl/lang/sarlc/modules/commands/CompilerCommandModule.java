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

import com.google.inject.AbstractModule;
import com.google.inject.Binding;
import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import com.google.inject.matcher.AbstractMatcher;
import com.google.inject.spi.ProvisionListener;
import io.bootique.config.ConfigurationFactory;
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
public class CompilerCommandModule extends AbstractModule {

	@Override
	protected void configure() {
		extend(binder())
			.addOption(OptionMetadata.builder(
				CompilerCommand.PROGRESS_OPTION_NAME, Messages.CompilerCommandModule_0)
				.valueOptionalWithDefault(Boolean.TRUE.toString())
				.build())
			.mapConfigPath(CompilerCommand.PROGRESS_OPTION_NAME, ProgressBarConfig.ENABLE);

		extend(binder()).addCommand(CompilerCommand.class);

		binder().bindListener(new BindingMatcher(), new LoggerProvisionListener(
				binder().getProvider(ProgressBarConfig.class)));
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

	/** Listener on logger provision.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	private static class LoggerProvisionListener implements ProvisionListener {

		private final Provider<ProgressBarConfig> commandConfig;

		/** Constructor.
		 *
		 * @param commandConfig the configuration of the compiler command.
		 */
		LoggerProvisionListener(Provider<ProgressBarConfig> commandConfig) {
			this.commandConfig = commandConfig;
		}

		@Override
		public <T> void onProvision(ProvisionInvocation<T> provision) {
			final T object = provision.provision();
			final ProgressBarConfig cfg = this.commandConfig.get();
			if (cfg.getEnable() && object instanceof Logger) {
				final Logger logger = (Logger) object;
				logger.setLevel(cfg.getLevel());
			}
		}

	}

	/** Matcher of sub types.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	private static class BindingMatcher extends AbstractMatcher<Binding<?>> {

		/** Constructor.
		 */
		BindingMatcher() {
			//
		}

		@Override
		public boolean matches(Binding<?> binding) {
			return Logger.class.isAssignableFrom(binding.getKey().getTypeLiteral().getRawType());
		}

	}

}
