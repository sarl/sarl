/*
 * $Id$
 * This file is a part of the Arakhne Foundation Classes, http://www.arakhne.org/afc
 *
 * Copyright (c) 2000-2012 Stephane GALLAND.
 * Copyright (c) 2005-10, Multiagent Team, Laboratoire Systemes et Transports,
 *                        Universite de Technologie de Belfort-Montbeliard.
 * Copyright (c) 2013-2025 The original authors, and other authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.arakhne.afc.bootique.printconfig;

import org.arakhne.afc.bootique.printconfig.modules.PrintConfigCommandModule;
import org.arakhne.afc.vmutil.locale.Locale;

import io.bootique.BQModule;
import io.bootique.BQModuleMetadata;
import io.bootique.BQModuleProvider;

/** Provider of a Bootique module for configuration printer.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version afc.extension 0.15.0 20250909-115746
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid afc.extension
 * @since 15.0
 */
@SuppressWarnings({"removal", "deprecation"})
public class PrintConfigModuleProvider implements BQModuleProvider {

	@Override
	public BQModule module() {
		return new PrintConfigCommandModule();
	}

	@Override
	public BQModuleMetadata.Builder moduleBuilder() {
		return BQModuleMetadata
				.builder(module())
				.overrides(overrides())
				.providerName(name())
				.configs(configs())
				.description(Locale.getString("MODULE_DESCRIPTION")); //$NON-NLS-1$
	}

}
