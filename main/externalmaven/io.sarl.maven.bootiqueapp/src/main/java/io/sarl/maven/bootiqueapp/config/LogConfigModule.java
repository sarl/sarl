/* 
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2020 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.maven.bootiqueapp.config;

import static io.bootique.BQCoreModule.extend;

import java.text.MessageFormat;

import javax.inject.Singleton;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provides;
import io.bootique.config.ConfigurationFactory;
import io.bootique.meta.application.OptionMetadata;
import org.arakhne.afc.bootique.variables.VariableDecls;
import org.arakhne.afc.bootique.variables.VariableNames;

/** 
 * Module for creating and configuring the loggers.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class LogConfigModule extends AbstractModule {

	private static final String LOG_LONG_OPTION = "log";
	
	@Override
	public void configure() {
		VariableDecls.extend(binder()).declareVar(LogConfig.LEVEL);
		final String cpDescription = MessageFormat.format(Messages.LogConfigModule_0,
				VariableNames.toEnvironmentVariableName(LogConfig.LEVEL),
				LOG_LONG_OPTION);
		extend(binder()).addOption(OptionMetadata.builder(LOG_LONG_OPTION, cpDescription)
				.valueRequired(Messages.LogConfigModule_1)
				.build())
				.mapConfigPath(LOG_LONG_OPTION, LogConfig.LEVEL);
	}

	/** Replies the instance of the logger configuration.
	 * 
	 * @param configFactory accessor to the bootique factory.
	 * @param injector the current injector.
	 * @return the path configuration accessor.
	 */
	@Provides
	@Singleton
	public LogConfig provideLogConfig(ConfigurationFactory configFactory, Injector injector) {
		final LogConfig config = LogConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

}
