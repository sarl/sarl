/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import com.google.inject.Binder;
import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Singleton;
import com.google.inject.util.Modules;
import org.eclipse.xtext.xbase.scoping.batch.ImplicitlyImportedFeatures;

import io.sarl.lang.SARLRuntimeModule;
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
		return new DocumentationSetup().createInjectorAndDoEMFRegistration();
	}

	@Override
	public Injector createInjector() {
		return Guice.createInjector(Modules.override(new SARLRuntimeModule()).with(new DocumentationModule()));
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
		}
		
	}

}
