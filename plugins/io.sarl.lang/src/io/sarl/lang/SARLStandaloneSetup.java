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
package io.sarl.lang;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.impl.SarlFactoryImplCustom;
import io.sarl.lang.sarl.impl.SarlXtendFactoryImpl;

import org.eclipse.emf.ecore.EFactory;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.xtend.core.xtend.XtendPackage;

import com.google.inject.Injector;

/**
 * Initialization support for running Xtext languages
 * without equinox extension registry.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLStandaloneSetup extends SARLStandaloneSetupGenerated {

	@Override
	public Injector createInjectorAndDoEMFRegistration() {
		// Override the definition of the XtendFactory with the SARL-specific factory.
		EPackage.Registry.INSTANCE.put(XtendPackage.eNS_URI, new SarlXtendPackageDescriptor());
		// Override the definition of the SarlFactory with the custom factory.
		EPackage.Registry.INSTANCE.put(SarlPackage.eNS_URI, new SarlCustomPackageDescriptor());
		return super.createInjectorAndDoEMFRegistration();
	}

	/**
	 */
	public static void doSetup() {
		new SARLStandaloneSetup().createInjectorAndDoEMFRegistration();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SarlXtendPackageDescriptor implements EPackage.Descriptor {

		public SarlXtendPackageDescriptor() {
			//
		}

		@Override
		public EPackage getEPackage() {
			throw new UnsupportedOperationException();
		}

		@Override
		public EFactory getEFactory() {
			return new SarlXtendFactoryImpl();
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class SarlCustomPackageDescriptor implements EPackage.Descriptor {

		public SarlCustomPackageDescriptor() {
			//
		}

		@Override
		public EPackage getEPackage() {
			throw new UnsupportedOperationException();
		}

		@Override
		public EFactory getEFactory() {
			return new SarlFactoryImplCustom();
		}

	}

}
