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

package io.sarl.docs.sarldoc.modules.internal;

import io.bootique.BQModuleMetadata;
import io.bootique.BQModuleProvider;
import io.bootique.di.BQModule;

/** Provider of the module for injecting the dynamic class loader.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarldoc 0.13.0 20230919-093100
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarldoc
 * @since 0.10
 */
public class SarldocDynamicClassLoaderModuleProvider implements BQModuleProvider {

	@Override
	public BQModule module() {
		return new SarldocDynamicClassLoaderModule();
	}

	@Override
    public BQModuleMetadata.Builder moduleBuilder() {
        return BQModuleMetadata
                .builder(module())
                .overrides(overrides())
                .providerName(name())
                .configs(configs())
                .description(Messages.SarldocDynamicClassLoaderModuleProvider_0);
    }

}
