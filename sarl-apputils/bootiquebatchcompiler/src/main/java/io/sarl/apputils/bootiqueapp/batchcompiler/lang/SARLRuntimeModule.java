/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.apputils.bootiqueapp.batchcompiler.lang;

import javax.inject.Named;
import javax.inject.Provider;
import javax.inject.Singleton;

import com.google.inject.Injector;
import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Provides;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.extralanguage.IExtraLanguageContributions;

/** Empty module that is defined for enabling automatic loading of modules
 * from command-line tools when their CLI options should be computed.
 *
 * @author $Author: sgalland$
 * @version bootiquebatchcompiler 0.13.0 20230919-093058
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid bootiquebatchcompiler
 * @since 0.12
 */
public final class SARLRuntimeModule implements BQModule {

	/** Name that represents the SARL injector.
	 *
	 * @since 0.13
	 */
	public static final String SARL_INJECTOR_NAME = "io.sarl.lang.compiler"; //$NON-NLS-1$

	@Override
	public void configure(Binder binder) {
		//
	}

	/** Provides the SARL injector that is based on Google Guice (not on Bootique injector). 
	 * 
	 * @return the Google Guice injector.
	 */
	@SuppressWarnings("static-method")
	@Singleton
	@Provides
	@Named(SARL_INJECTOR_NAME)
	public Injector providesSarlCompilerInjector() {
		final Injector injector = new SARLStandaloneSetup().createInjector();
		return injector;
	}

	/** Provides the extra-language contributions to the SARL compiler.
	 *
	 * @param sarlInjector the SARL injector.
	 * @return the SARL compiler.
	 * @see #providesSarlCompilerInjector()
	 */
	@SuppressWarnings("static-method")
	@Singleton
	@Provides
	public IExtraLanguageContributions providesExtraLanguageContributions(@Named(SARL_INJECTOR_NAME) Provider<Injector> sarlInjector) {
		final IExtraLanguageContributions contribs = sarlInjector.get().getInstance(IExtraLanguageContributions.class);
		return contribs;
	}
	
}
