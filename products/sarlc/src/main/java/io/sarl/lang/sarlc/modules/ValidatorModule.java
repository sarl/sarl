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

import static io.sarl.lang.sarlc.configs.CompilerConfig.ALL_ERRORS_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.ALL_WARNINGS_NAME;
import static io.sarl.lang.sarlc.configs.CompilerConfig.IGNORE_WARNINGS_NAME;

import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.bootique.config.ConfigurationFactory;
import io.bootique.meta.application.OptionMetadata;

import io.sarl.lang.sarlc.configs.ValidatorConfig;
import io.sarl.maven.bqextension.modules.AbstractConfigModule;
import io.sarl.maven.bqextension.modules.BQConfigTypes;
import io.sarl.maven.bqextension.modules.BQModule;

/** Module for the sarlc specific commands.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQModule("The SARL batch validator.")
@BQConfigTypes(ValidatorConfig.class)
public class ValidatorModule extends AbstractConfigModule {

	@Override
	protected void configure() {
		associateEnvironmentVariable(IGNORE_WARNINGS_NAME);
		associateOption(OptionMetadata.builder(
				"nowarn", Messages.ValidatorModule_0) //$NON-NLS-1$
				.configPath(IGNORE_WARNINGS_NAME));

		associateEnvironmentVariable(ALL_WARNINGS_NAME);
		associateOption(OptionMetadata.builder(
				"wall", Messages.ValidatorModule_2) //$NON-NLS-1$
				.configPath(ALL_WARNINGS_NAME));

		associateEnvironmentVariable(ALL_ERRORS_NAME);
		associateOption(OptionMetadata.builder(
				"werr", Messages.ValidatorModule_1) //$NON-NLS-1$
				.configPath(ALL_ERRORS_NAME));
	}

	/** Replies the instance of the validator configuration.
	 *
	 * @param configFactory accessor to the bootique factory.
	 * @param injector the current injector.
	 * @return the validator configuration accessor.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public ValidatorConfig getValidatorConfig(ConfigurationFactory configFactory, Injector injector) {
		final ValidatorConfig config = ValidatorConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

}
