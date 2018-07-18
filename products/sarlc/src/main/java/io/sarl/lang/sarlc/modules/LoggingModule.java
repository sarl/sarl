/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.lang.sarlc.modules;

import static io.bootique.BQCoreModule.extend;
import static io.sarl.lang.sarlc.configs.CompilerConfig.GENERATE_CLONE_NAME;
import static io.sarl.lang.sarlc.configs.LoggingConfig.DEFAULT_FORMAT_NAME;
import static io.sarl.lang.sarlc.configs.LoggingConfig.VERBOSE_LEVEL_NAME;

import java.text.MessageFormat;
import java.util.logging.Level;

import javax.inject.Inject;

import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import com.google.inject.name.Names;
import io.bootique.config.ConfigurationFactory;
import io.bootique.meta.application.OptionMetadata;

import io.sarl.lang.sarlc.configs.LoggingConfig;
import io.sarl.lang.sarlc.configs.LoggingConfig.LogLabel;
import io.sarl.maven.bqextension.modules.AbstractConfigModule;
import io.sarl.maven.bqextension.modules.BQConfigTypes;
import io.sarl.maven.bqextension.modules.BQModule;

/** Module for the logging services.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQModule("The basic logging system configurator.")
@BQConfigTypes(LoggingConfig.class)
public class LoggingModule extends AbstractConfigModule {

	@Override
	protected void configure() {
		for (final LogLabel label : LogLabel.values()) {
			extend(binder()).setLogLevel(label.getLabels()[0], label.toLevel());
		}
		associateEnvironmentVariable(VERBOSE_LEVEL_NAME);
		final String allLabels = LoggingConfig.getLabels(Messages.LoggingModule_0);
		associateOption(OptionMetadata.builder(
				"verbose", //$NON-NLS-1$
				MessageFormat.format(Messages.LoggingModule_1, allLabels, LoggingConfig.VERBOSE_LEVEL_VALUE))
				.configPath(GENERATE_CLONE_NAME)
				.valueRequired(Messages.LoggingModule_2));

		associateEnvironmentVariable(DEFAULT_FORMAT_NAME);

		binder().bind(Level.class).annotatedWith(Names.named(LoggingConfig.VERBOSE_LEVEL_NAME))
			.toProvider(VerboseLevelObjectProvider.class).in(Singleton.class);

		binder().bind(String.class).annotatedWith(Names.named(LoggingConfig.VERBOSE_LEVEL_NAME))
			.toProvider(VerboseLevelStringProvider.class).in(Singleton.class);
	}

	/** Replies the instance of the logging configuration.
	 *
	 * @param configFactory accessor to the bootique factory.
	 * @param injector the current injector.
	 * @return the logging configuration accessor.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public LoggingConfig getLoggingConfig(ConfigurationFactory configFactory, Injector injector) {
		final LoggingConfig config = LoggingConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

	/** Provider for the verbose level.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	public static class VerboseLevelObjectProvider implements Provider<Level> {

		@Inject
		private LoggingConfig config;

		@Override
		public Level get() {
			return this.config.getJavaLevelObject();
		}

	}

	/** Provider for the verbose level.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	public static class VerboseLevelStringProvider implements Provider<String> {

		@Inject
		private LoggingConfig config;

		@Override
		public String get() {
			return this.config.getLevel();
		}

	}

}
