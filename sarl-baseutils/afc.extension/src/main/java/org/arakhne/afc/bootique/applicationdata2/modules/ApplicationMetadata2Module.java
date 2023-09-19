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

import java.util.Set;
import javax.inject.Singleton;

import io.bootique.command.CommandManager;
import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Key;
import io.bootique.di.Provides;
import io.bootique.env.DeclaredVariable;
import io.bootique.meta.application.ApplicationMetadata;
import io.bootique.meta.application.OptionMetadata;
import io.bootique.meta.module.ModulesMetadata;

import org.arakhne.afc.bootique.applicationdata2.annotations.ApplicationDescription2;
import org.arakhne.afc.bootique.applicationdata2.annotations.DefaultApplicationName;

/** Module for the compiler application metadata version 2.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 15.0
 */
public class ApplicationMetadata2Module implements BQModule {

	@Override
	public void configure(Binder binder) {
		//
	}

	/** Provides the metadata for the application.
	 *
	 * @param commandManager the command manager.
	 * @param options the options
	 * @param declaredVars the declared variables
	 * @param modulesMetadata the module metadata.
	 * @param injector the injector.
	 * @return the application metadata.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public ApplicationMetadata provideApplicationMetadata(
			CommandManager commandManager,
			Set<OptionMetadata> options,
			Set<DeclaredVariable> declaredVars,
			ModulesMetadata modulesMetadata,
			Injector injector) {

		String name;
		try {
			name = injector.getInstance(Key.get(String.class, DefaultApplicationName.class));
		} catch (Throwable exception) {
			name = null;
		}

		String description;
		try {
			description = injector.getInstance(Key.get(String.class, ApplicationDescription2.class));
		} catch (Throwable exception) {
			description  = null;
		}

		final ApplicationMetadata.Builder builder = ApplicationMetadata
			.builder(name)
			.description(description)
			.addOptions(options);

		commandManager.getAllCommands().values().forEach(mc -> {
			if (!mc.isHidden() && !mc.isDefault()) {
				builder.addCommand(mc.getCommand().getMetadata());
			}
		});

		// merge default command options with top-level app options
		commandManager.getPublicDefaultCommand().ifPresent(
			c -> builder.addOptions(c.getMetadata().getOptions()));

		declaredVars.forEach(dv -> DeclaredVariableMetaCompiler
			.compileIfValid(dv, modulesMetadata)
			.ifPresent(builder::addVariable));

		return builder.build();
	}

}
