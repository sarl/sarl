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

package io.sarl.lang.sarlc.modules.configs;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Map;

import io.bootique.BQModuleMetadata;
import io.bootique.BQModuleProvider;
import io.bootique.di.BQModule;

import io.sarl.lang.sarlc.configs.SarlcConfig;

/** Provider of the module for the sarlc configuration.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
public class SarlcConfigModuleProvider implements BQModuleProvider {

	@Override
	public BQModule module() {
		return new SarlcConfigModule();
	}

	@Override
	public Map<String, Type> configs() {
		return Collections.singletonMap(SarlcConfig.PREFIX, SarlcConfig.class);
	}

	@Override
    public BQModuleMetadata.Builder moduleBuilder() {
        return BQModuleMetadata
                .builder(module())
                .overrides(overrides())
                .providerName(name())
                .configs(configs())
                .description(Messages.SarlcConfigModuleProvider_0);
    }

}
