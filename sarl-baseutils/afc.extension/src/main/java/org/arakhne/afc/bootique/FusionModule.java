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

package org.arakhne.afc.bootique;

import java.util.Set;

import javax.inject.Singleton;

import io.bootique.command.CommandManager;
import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Provides;
import io.bootique.env.DeclaredVariable;
import io.bootique.help.HelpGenerator;
import io.bootique.meta.application.ApplicationMetadata;
import io.bootique.meta.application.OptionMetadata;
import io.bootique.meta.module.ModulesMetadata;
import io.bootique.terminal.Terminal;
import org.arakhne.afc.bootique.applicationdata2.modules.ApplicationMetadata2Module;
import org.arakhne.afc.bootique.synopsishelp.modules.SynopsisHelpGeneratorModule;

/** Module for creating the help generator with synopsis.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 15.0
 * @deprecated Should be removed when the AFC modules have been moved to the main stream of AFC.
 */
@Deprecated
public class FusionModule implements BQModule {

	private final SynopsisHelpGeneratorModule module0 = new SynopsisHelpGeneratorModule();

	private final ApplicationMetadata2Module module1 = new ApplicationMetadata2Module();

	@Override
	public void configure(Binder binder) {
		this.module0.configure(binder);
		this.module1.configure(binder);
	}

	/** Provide the help generator with synopsis.
	 *
	 * @param metadata the application metadata.
	 * @param injector the injector.
	 * @param terminal the terminal description.
	 * @return the help generator.
	 */
	@Provides
	@Singleton
	public HelpGenerator provideHelpGenerator(ApplicationMetadata metadata, Injector injector, Terminal terminal) {
		return this.module0.provideHelpGenerator(metadata, injector, terminal);
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
	@Provides
	@Singleton
	public ApplicationMetadata provideApplicationMetadata(
			CommandManager commandManager,
			Set<OptionMetadata> options,
			Set<DeclaredVariable> declaredVars,
			ModulesMetadata modulesMetadata,
			Injector injector) {
		return this.module1.provideApplicationMetadata(commandManager, options, declaredVars, modulesMetadata, injector);
	}

}
