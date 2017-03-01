/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.janusproject.tests.testutils;

import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.util.Modules;

import io.janusproject.modules.StandardJanusPlatformModule;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.tests.api.ExtendedSARLInjectorProvider;

/**
 * The injection provider for Janus unit tests.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusInjectorProvider extends ExtendedSARLInjectorProvider {

	@Override
	protected Injector internalCreateInjector() {
		return new SARLStandaloneSetup() {
			@SuppressWarnings("synthetic-access")
			@Override
			public Injector createInjector() {
				return Guice.createInjector(
						Modules.override(createRuntimeModule()).with(createJanusModule()), createRuntimeTestModule());
			}
		}.createInjectorAndDoEMFRegistration();
	}

	/** Create the Janus module.
	 *
	 * @return the module.
	 */
	@SuppressWarnings("static-method")
	protected Module createJanusModule() {
		return Modules.override(new StandardJanusPlatformModule()).with(new NoLogTestingModule());
	}

}
