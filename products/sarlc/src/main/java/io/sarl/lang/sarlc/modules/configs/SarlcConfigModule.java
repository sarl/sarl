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

package io.sarl.lang.sarlc.modules.configs;

import static io.bootique.BQCoreModule.extend;
import static io.sarl.lang.sarlc.configs.SarlConfig.BOOT_CLASSPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlConfig.CLASSPATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlConfig.CLASS_OUTPUT_PATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlConfig.OUTPUT_PATH_NAME;
import static io.sarl.lang.sarlc.configs.SarlConfig.WORKING_PATH_NAME;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.bootique.config.ConfigurationFactory;
import io.bootique.meta.application.OptionMetadata;
import org.arakhne.afc.bootique.variables.VariableDecls;

import io.sarl.lang.sarlc.Constants;
import io.sarl.lang.sarlc.configs.SarlConfig;

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

	@Override
	protected void configure() {
		VariableDecls.extend(binder()).declareVar(OUTPUT_PATH_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				Constants.SARL_OUTPUT_DIRECTORY_OPTION, Messages.SarlcConfigModule_0)
				.configPath(OUTPUT_PATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_1)
				.build());

		VariableDecls.extend(binder()).declareVar(CLASS_OUTPUT_PATH_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				Constants.JAVA_OUTPUT_DIRECTORY_OPTION, Messages.SarlcConfigModule_7)
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
		extend(binder()).addOptions(OptionMetadata.builder(
				"classpath", Messages.SarlcConfigModule_3) //$NON-NLS-1$
				.configPath(CLASSPATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build(),
				OptionMetadata.builder(
				"cp", Messages.SarlcConfigModule_5) //$NON-NLS-1$
				.configPath(CLASSPATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build());

		VariableDecls.extend(binder()).declareVar(BOOT_CLASSPATH_NAME);
		extend(binder()).addOption(OptionMetadata.builder(
				"bootclasspath", Messages.SarlcConfigModule_6) //$NON-NLS-1$
				.configPath(BOOT_CLASSPATH_NAME)
				.valueRequired(Messages.SarlcConfigModule_4)
				.build());
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
	public SarlConfig getSarlcConfig(ConfigurationFactory configFactory, Injector injector) {
		final SarlConfig config = SarlConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

}
