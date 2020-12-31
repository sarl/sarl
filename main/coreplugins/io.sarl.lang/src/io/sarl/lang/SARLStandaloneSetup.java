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

package io.sarl.lang;

import com.google.inject.Guice;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.util.Modules;
import org.eclipse.xtend.core.XtendStandaloneSetup;

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

	/** Set up the EMF modules for the SARL language.
	 *
	 * @return the injector.
	 */
	public static Injector doSetup() {
		return new SARLStandaloneSetup().createInjectorAndDoEMFRegistration();
	}

	/** Set up the EMF modules for the SARL language.
	 *
	 * @param modules the injection modules that are overriding the standard SARL module.
	 * @return the injector.
	 * @since 0.8
	 * @see SARLRuntimeModule
	 */
	public static Injector doSetup(Module... modules) {
		return new SARLStandaloneSetup().createInjectorAndDoEMFRegistration(modules);
	}

	/** Run the pre-setup task.
	 *
	 * <p>This function is provided in order to let the caller to create the injector by hand.
	 * It is recommended to invoke {@link #doSetup()} in place of this function.
	 *
	 * @since 0.8
	 * @see #doSetup()
	 * @see #doSetup(Module...)
	 */
	public static void doPreSetup() {
		XtendStandaloneSetup.doSetup();
	}

	/** Run the post-setup task.
	 *
	 * <p>This function is provided in order to let the caller to create the injector by hand.
	 * It is recommended to invoke {@link #doSetup()} in place of this function.
	 *
	 * @param injector the injector to be used.
	 * @return the injector.
	 * @since 0.8
	 * @see #doSetup()
	 * @see #doSetup(Module...)
	 */
	public static Injector doPostSetup(Injector injector) {
		new SARLStandaloneSetup().register(injector);
		return injector;
	}

	@Override
	public Injector createInjectorAndDoEMFRegistration() {
		doPreSetup();
		final Injector injector = createInjector();
		register(injector);
		return injector;
	}

	/** Create the injector based on the given set of modules and prepare the EMF infrastructure.
	 *
	 * @param modules the injection modules that are overriding the standard SARL module.
	 * @return the injector.
	 * @since 0.8
	 * @see SARLRuntimeModule
	 */
	public Injector createInjectorAndDoEMFRegistration(Module... modules) {
		doPreSetup();
		final Injector injector = createInjector(modules);
		register(injector);
		return injector;
	}

	/** Create the injectors based on the given set of modules.
	 *
	 * @param modules the injection modules that are overriding the standard SARL module.
	 * @return the injector.
	 * @since 0.8
	 * @see SARLRuntimeModule
	 */
	@SuppressWarnings("static-method")
	public Injector createInjector(Module... modules) {
		return Guice.createInjector(Modules.override(new SARLRuntimeModule()).with(modules));
	}

}
