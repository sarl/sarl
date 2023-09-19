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

package org.arakhne.afc.bootique.synopsishelp.modules;

import javax.inject.Singleton;

import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Injector;
import io.bootique.di.Key;
import io.bootique.di.Provides;
import io.bootique.help.HelpGenerator;
import io.bootique.meta.application.ApplicationMetadata;
import io.bootique.terminal.Terminal;

import org.arakhne.afc.bootique.synopsishelp.annotations.ApplicationArgumentSynopsis;
import org.arakhne.afc.bootique.synopsishelp.annotations.ApplicationDetailedDescription;
import org.arakhne.afc.bootique.synopsishelp.help.SynopsisHelpGenerator;

/** Module for creating the help generator with synopsis.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 15.0
 */
public class SynopsisHelpGeneratorModule implements BQModule {

	private static final int TTY_MIN_COLUMNS = 40;

	private static final int TTY_DEFAULT_COLUMNS = 80;

	@Override
	public void configure(Binder binder) {
		//
	}

	/** Provide the help generator with synopsis.
	 *
	 * @param metadata the application metadata.
	 * @param injector the injector.
	 * @param terminal the terminal description.
	 * @return the help generator.
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	public HelpGenerator provideHelpGenerator(
			ApplicationMetadata metadata, Injector injector, Terminal terminal) {
		int maxColumns = terminal.getColumns();
		if (maxColumns < TTY_MIN_COLUMNS) {
			maxColumns = TTY_DEFAULT_COLUMNS;
		}
		String argumentSynopsis;
		try {
			argumentSynopsis = injector.getInstance(Key.get(String.class, ApplicationArgumentSynopsis.class));
		} catch (Exception exception) {
			argumentSynopsis = null;
		}
		String detailedDescription;
		try {
			detailedDescription = injector.getInstance(Key.get(String.class, ApplicationDetailedDescription.class));
		} catch (Exception exception) {
			detailedDescription = null;
		}
		return new SynopsisHelpGenerator(metadata, argumentSynopsis, detailedDescription, maxColumns);
	}

}
