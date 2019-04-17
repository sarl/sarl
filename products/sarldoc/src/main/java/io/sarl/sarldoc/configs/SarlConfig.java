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

package io.sarl.sarldoc.configs;

import io.bootique.annotation.BQConfig;
import io.bootique.annotation.BQConfigProperty;
import io.bootique.config.ConfigurationFactory;

import io.sarl.sarldoc.configs.subconfigs.SarldocConfig;

/**
 * Configuration for the sarldoc tool.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
@BQConfig("Configuration of the SARLC tool")
public class SarlConfig extends io.sarl.lang.sarlc.configs.SarlConfig {

	private SarldocConfig sarldocConfig;

	/** Replies the configuration for SARLC.
	 *
	 * @param configFactory the general configuration factory.
	 * @return the SARLC configuration.
	 */
	public static SarlConfig getConfiguration(ConfigurationFactory configFactory) {
		assert configFactory != null;
		return configFactory.config(SarlConfig.class, PREFIX);
	}

	/** Replies the configuration for the API documentation generator.
	 *
	 * @return the sarldoc configuration.
	 */
	public SarldocConfig getSarldoc() {
		if (this.sarldocConfig == null) {
			this.sarldocConfig = new SarldocConfig();
		}
		return this.sarldocConfig;
	}

	/** Change the configuration for the API documentation generator.
	 *
	 * @param config the sarldoc configuration.
	 */
	@BQConfigProperty("Configuration of sarldoc.")
	public void setCompiler(SarldocConfig config) {
		this.sarldocConfig = config;
	}

}
