/*
 * $Id$
 * This file is a part of the Arakhne Foundation Classes, http://www.arakhne.org/afc
 *
 * Copyright (c) 2000-2012 Stephane GALLAND.
 * Copyright (c) 2005-10, Multiagent Team, Laboratoire Systemes et Transports,
 *                        Universite de Technologie de Belfort-Montbeliard.
 * Copyright (c) 2013-2022 The original authors, and other authors.
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

package org.arakhne.afc.bootique.applicationdata2.modules;

import java.util.Optional;

import io.bootique.env.DeclaredVariable;
import io.bootique.meta.config.ConfigMetadataNode;
import io.bootique.meta.config.ConfigValueMetadata;
import io.bootique.meta.module.ModuleMetadata;
import io.bootique.meta.module.ModulesMetadata;

/** Module for the compiler application metadata version 2.
 *
 * <p>This file is copied from the Bootique's original file in order to change the visibility.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 18.0
 */
public final class DeclaredVariableMetaCompiler {

	private DeclaredVariableMetaCompiler() {
		//
	}

	/** Compute the values of the declared variables.
	 *
	 * @param var the vairable.
	 * @param modulesMetadata the metadata.
	 * @return the value's metadata
	 */
	public static Optional<ConfigValueMetadata> compileIfValid(DeclaredVariable var, ModulesMetadata modulesMetadata) {
		for (final ModuleMetadata mm : modulesMetadata.getModules()) {
			// TODO: 'findConfig' does a String split over and over again as we iterate through the loop.
			//  Precalculate this once.
			final Optional<ConfigMetadataNode> cmn = mm.findConfig(var.getConfigPath());
			if (cmn.isPresent()) {
				return cmn.map(n -> compileMetadata(var, n));
			}
		}
		return Optional.empty();
	}

	private static ConfigValueMetadata compileMetadata(DeclaredVariable variable, ConfigMetadataNode configMetadata) {
		// TODO: validation... verify that the variable is bound to a value, not a collection or a map??
		return ConfigValueMetadata
				.builder(variable.getName())
				.description(variable.getDescription() != null ? variable.getDescription() : configMetadata.getDescription())
				.type(configMetadata.getType()).build();
	}

}
