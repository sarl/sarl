/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

package io.sarl.maven.docs.generator;

import org.eclipse.emf.ecore.EPackage;
import org.jnario.spec.SpecRuntimeModule;
import org.jnario.spec.SpecStandaloneSetupGenerated;
import org.jnario.spec.spec.SpecPackage;

import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.util.Modules;

/**
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class SARLSpecStandaloneSetup extends SpecStandaloneSetupGenerated {

	private static Injector injector = null;

	/**
	 */
	public SARLSpecStandaloneSetup() {
		//
	}

	/** {@inheritDoc}
	 */
	@Override
	public Injector createInjectorAndDoEMFRegistration() {
		if (injector != null)
			return injector;
		EPackage.Registry.INSTANCE.put(SpecPackage.eINSTANCE.getNsURI(), SpecPackage.eINSTANCE);
		injector = Guice.createInjector(
				Modules.override(new SpecRuntimeModule()).with(new SARLDocModule()));
		new SpecStandaloneSetupGenerated().register(injector);
		return injector;
	}

}
