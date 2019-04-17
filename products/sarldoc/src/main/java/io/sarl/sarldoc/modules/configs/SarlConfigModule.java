/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.sarldoc.modules.configs;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import io.bootique.config.ConfigurationFactory;

import io.sarl.sarldoc.configs.SarlConfig;

/**
 * Module for creating and configuring the general/root sarldoc configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarlConfigModule extends AbstractModule {

	@Override
	protected void configure() {
		//
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
	public SarlConfig getSarldocConfig(ConfigurationFactory configFactory, Injector injector) {
		final SarlConfig config = SarlConfig.getConfiguration(configFactory);
		injector.injectMembers(config);
		return config;
	}

	/** Replies the instance of the sarl configuration.
	 *
	 * @param configFactory accessor to the bootique factory.
	 * @param injector the current injector.
	 * @return the path configuration accessor.
	 */
	@Provides
	@Singleton
	public io.sarl.lang.sarlc.configs.SarlConfig getSarlcConfig(ConfigurationFactory configFactory, Injector injector) {
		return getSarldocConfig(configFactory, injector);
	}

}
