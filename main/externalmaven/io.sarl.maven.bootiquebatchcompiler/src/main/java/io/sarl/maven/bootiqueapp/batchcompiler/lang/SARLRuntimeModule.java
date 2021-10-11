/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.maven.bootiqueapp.batchcompiler.lang;

import javax.inject.Provider;
import javax.inject.Singleton;

import com.google.inject.Injector;
import io.bootique.di.BQModule;
import io.bootique.di.Binder;
import io.bootique.di.Provides;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;

/** Empty module that is defined for enabling automatic loading of modules
 * from command-line tools when their CLI options should be computed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class SARLRuntimeModule extends io.sarl.lang.SARLRuntimeModule implements BQModule {

	@Override
	public void configure(Binder binder) {
		//
	}

	@Singleton
	@Provides
	public Injector providesSarlCompilerInjector() {
		final Injector injector = new SARLStandaloneSetup().createInjector();
		return injector;
	}

	@Singleton
	@Provides
	public SarlBatchCompiler providesSarlCompiler(Provider<Injector> sarlInjector) {
		final SarlBatchCompiler compiler = sarlInjector.get().getInstance(SarlBatchCompiler.class);
		return compiler;
	}

}
