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

import static io.sarl.lang.sarlc.configs.SarlcConfig.BOOT_CLASSPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.CLASSPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.CLASS_OUTPUT_PATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.COMPILER_PROGRAM_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.OUTPUT_PATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlcConfig.WORKING_PATH_NAME;

import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.bootique.config.ConfigurationFactory;
import io.bootique.meta.application.OptionMetadata;

import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.maven.bqextension.modules.AbstractConfigModule;
import io.sarl.maven.bqextension.modules.BQConfigTypes;
import io.sarl.maven.bqextension.modules.BQModule;

/**
 * Module for the sarlc tool.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQModule("The sarlc command line tool.")
@BQConfigTypes(SarlcConfig.class)
public class SarlcModule extends AbstractConfigModule {

	@Override
	protected void configure() {
		associateEnvironmentVariable(COMPILER_PROGRAM_NAME);

		associateEnvironmentVariable(OUTPUT_PATH_NAME);
		associateOption(OptionMetadata.builder(
				"directory", Messages.SarlcModule_0) //$NON-NLS-1$
				.configPath(OUTPUT_PATH_NAME)
				.valueRequired(Messages.SarlcModule_1));

		associateEnvironmentVariable(CLASS_OUTPUT_PATH_NAME);
		associateOption(OptionMetadata.builder(
				"outputdir", Messages.SarlcModule_7) //$NON-NLS-1$
				.configPath(CLASS_OUTPUT_PATH_NAME)
				.valueRequired(Messages.SarlcModule_1));

		associateEnvironmentVariable(WORKING_PATH_NAME);
		associateOption(OptionMetadata.builder(
				"workingdir", Messages.SarlcModule_2) //$NON-NLS-1$
				.configPath(WORKING_PATH_NAME)
				.valueRequired(Messages.SarlcModule_1));

		associateEnvironmentVariable(CLASSPATH_NAME);
		associateOption(OptionMetadata.builder(
				"classpath", Messages.SarlcModule_3) //$NON-NLS-1$
				.configPath(CLASSPATH_NAME)
				.valueRequired(Messages.SarlcModule_4));
		associateOption(OptionMetadata.builder(
				"cp", Messages.SarlcModule_5) //$NON-NLS-1$
				.configPath(CLASSPATH_NAME)
				.valueRequired(Messages.SarlcModule_4));

		associateEnvironmentVariable(BOOT_CLASSPATH_NAME);
		associateOption(OptionMetadata.builder(
				"bootclasspath", Messages.SarlcModule_6) //$NON-NLS-1$
				.configPath(BOOT_CLASSPATH_NAME)
				.valueRequired(Messages.SarlcModule_4));
	}

	/** Replies the instance of the path configuration.
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
