/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.apputils.bootiqueapp.batchcompiler;

import com.google.inject.Injector;
import io.bootique.BQModuleProvider;
import io.bootique.BQRuntime;
import io.bootique.di.Key;

import io.sarl.apputils.bootiqueapp.BootiqueMain;
import io.sarl.apputils.bootiqueapp.batchcompiler.lang.SARLRuntimeModule;
import io.sarl.lang.SARLStandaloneSetup;

/** Class that implements the standard main function for running a SARL application
 * with bootique and SARL batch compiler modules.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version bootiquebatchcompiler 0.14.0 20241106-161408
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid bootiquebatchcompiler
 * @since 0.12
 */
public class BootiqueBatchCompilerMain extends BootiqueMain {

	/** Constructor.
	 *
	 * @param providers the providers of module that injects application-specific components.
	 */
	public BootiqueBatchCompilerMain(BQModuleProvider... providers) {
		super(providers);
	}

	/** Constructor.
	 *
	 * @param experimental indicates if the application is experimental.
	 * @param providers the providers of module that injects application-specific components.
	 */
	public BootiqueBatchCompilerMain(boolean experimental, BQModuleProvider... providers) {
		super(experimental, providers);
	}

	@Override
	protected BQRuntime createRuntime(String... args) {
		SARLStandaloneSetup.doPreSetup();
		final var runtime = super.createRuntime(args);
		final var injector = runtime.getInstance(Key.get(Injector.class, SARLRuntimeModule.SARL_INJECTOR_NAME));
		SARLStandaloneSetup.doPostSetup(injector);
		return runtime;
	}

}
