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

package org.arakhne.afc.bootique.printconfig.modules;

import static io.bootique.BQCoreModule.extend;

import javax.inject.Provider;
import javax.inject.Singleton;

import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Provides;
import io.bootique.log.BootLogger;
import io.bootique.meta.module.ModulesMetadata;

import org.arakhne.afc.bootique.printconfig.commands.PrintConfigCommand;

/** Module for the command that prints out the configuration values.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 15.0
 */
public class PrintConfigCommandModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		extend(binder).addCommand(PrintConfigCommand.class);
	}

	/** Provide the command for running the command for printing out the configuration values.
	 *
	 * @param bootLogger the boot logger.
	 * @param modulesMetadata the modules' metadata.
	 * @param injector the current injector.
	 * @return the command.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public PrintConfigCommand providePrintConfigCommand(
			Provider<BootLogger> bootLogger,
			Provider<ModulesMetadata> modulesMetadata,
			Injector injector) {
		return new PrintConfigCommand(bootLogger, modulesMetadata, injector);
	}

}
