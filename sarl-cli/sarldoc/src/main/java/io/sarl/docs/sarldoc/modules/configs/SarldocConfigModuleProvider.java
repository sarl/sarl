/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.sarldoc.modules.configs;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Map;

import io.bootique.BQModuleMetadata;
import io.bootique.BQModuleProvider;
import io.bootique.di.BQModule;

import io.sarl.docs.sarldoc.configs.SarldocConfig;

/** Provider of the module for the general sarldoc specific configuration.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarldoc 0.13.0 20230919-093100
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarldoc
 * @since 0.10
 */
public class SarldocConfigModuleProvider implements BQModuleProvider {

	@Override
	public BQModule module() {
		return new SarldocConfigModule();
	}

	@Override
	public Map<String, Type> configs() {
		return Collections.singletonMap(SarldocConfig.PREFIX, SarldocConfig.class);
	}

	@Override
    public BQModuleMetadata.Builder moduleBuilder() {
        return BQModuleMetadata
                .builder(module())
                .overrides(overrides())
                .providerName(name())
                .configs(configs())
                .description(Messages.SarldocConfigModuleProvider_0);
    }

}
