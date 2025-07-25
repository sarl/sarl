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

package org.arakhne.afc.bootique.synopsishelp;

import java.util.Collection;
import java.util.Collections;

import org.arakhne.afc.bootique.synopsishelp.modules.SynopsisHelpGeneratorModule;
import org.arakhne.afc.vmutil.locale.Locale;

import io.bootique.BQCoreModule;
import io.bootique.BQModule;
import io.bootique.BQModuleMetadata;
import io.bootique.BQModuleProvider;

/** Provider of a Bootique module for synopsis help generator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 15.0
 */
@SuppressWarnings({"removal", "deprecation"})
public class SynopsisHelpModuleProvider implements BQModuleProvider {

	@Override
	public BQModule module() {
		return new SynopsisHelpGeneratorModule();
	}

	@Override
	public Collection<Class<? extends BQModule>> overrides() {
		return Collections.singleton(BQCoreModule.class);
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
