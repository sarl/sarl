/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.sarlc.modules.commands;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.Map;

import io.bootique.BQModule;
import io.bootique.BQModuleMetadata;
import io.bootique.BQModuleProvider;
import io.sarl.lang.sarlc.configs.ProgressBarConfig;

/** Provider of the module for the compiler command.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version sarlc 0.15.0 20250909-115750
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarlc
 * @since 0.8
 */
@SuppressWarnings("removal")
public class CompilerCommandModuleProvider implements BQModuleProvider {

	@Override
	public BQModule module() {
		return new CompilerCommandModule();
	}

	@Override
	public Map<String, Type> configs() {
		return Collections.singletonMap(ProgressBarConfig.PREFIX, ProgressBarConfig.class);
	}

	@SuppressWarnings("deprecation")
	@Override
    public BQModuleMetadata.Builder moduleBuilder() {
        return BQModuleMetadata
                .builder(module())
                .overrides(overrides())
                .providerName(name())
                .configs(configs())
                .description(Messages.CompilerCommandModuleProvider_0);
    }

}
