/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.tests.api;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.jvmmodel.JvmModelInferrerProber;

import com.google.common.base.Optional;
import com.google.inject.AbstractModule;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Provides;
import com.google.inject.util.Modules;

/** Module for injecting the singleton of {@link JvmModelInferrerProber}.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JvmModelInferrerProberInjectorProvider extends SARLInjectorProvider {

	/** {@inheritDoc}
	 */
	@Override
	protected Injector internalCreateInjector() {
		return new InternalSetup().createInjectorAndDoEMFRegistration();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class InternalSetup extends SARLStandaloneSetup {

		public InternalSetup() {
			//
		}

		@Override
		public Injector createInjector() {
			return Guice.createInjector(
					Modules.override(new io.sarl.lang.SARLRuntimeModule())
					.with(new InternalModule()));
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class InternalModule extends AbstractModule {

		public InternalModule() {
			//
		}

		@Override
		public void configure() {
			//
		}

		@SuppressWarnings("static-method")
		@Provides
		public Optional<JvmModelInferrerProber> getOptionalJvmModelInferrerProber(JvmModelInferrerProber prober) {
			return Optional.of(prober);
		}

	}

}
