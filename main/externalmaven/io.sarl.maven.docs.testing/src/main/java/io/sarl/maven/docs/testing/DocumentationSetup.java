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

package io.sarl.maven.docs.testing;

import java.util.Arrays;

import com.google.inject.Binder;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Singleton;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;

import io.sarl.lang.SARLStandaloneSetup;

/** Implicitly imported extensions for the testing of the documentation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@SuppressWarnings({"checkstyle:methodname"})
public class DocumentationSetup extends SARLStandaloneSetup {

	private static Injector globalInjector;
	
	/** Construct the provider.
	 */
	public DocumentationSetup() {
		super();
	}

	/** Create the injector.
	 *
	 * @return the injector.
	 */
	public static Injector doSetup() {
		synchronized (DocumentationSetup.class) {
			if (globalInjector == null) {
				globalInjector = new DocumentationSetup().createInjectorAndDoEMFRegistration();
			}
			return globalInjector;
		}
	}

	@Override
	public Injector createInjector() {
		return createInjector(new Module[0]);
	}
	
	@Override
	public Injector createInjector(Module... modules) {
		final Module[] nmodules = Arrays.copyOf(modules, modules.length + 1);
		nmodules[nmodules.length - 1] = new DocumentationModule();
		return super.createInjector(nmodules);
	}

	/** Module for the documentation generator.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	private static class DocumentationModule implements Module {

		/**
		 */
		DocumentationModule() {
			//
		}

		@Override
		public void configure(Binder binder) {
			binder.bind(ImplicitlyImportedFeatures.class).to(DocumentationImplicitlyImportedFeatures.class).in(Singleton.class);
			binder.bind(ScriptExecutor.class).to(SarlScriptExecutor.class).in(Singleton.class);
		}
		
	}

}
