/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
package io.sarl.tests.api.extensions;

import static org.eclipse.xtext.util.Exceptions.throwUncheckedException;

import org.eclipse.xtext.testing.IInjectorProvider;
import org.eclipse.xtext.testing.IRegistryConfigurator;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.extensions.InjectionExtension;
import org.junit.jupiter.api.extension.Extension;
import org.junit.jupiter.api.extension.ExtensionContext;

/** JUnit 5 extension that injects the SARL components.
 *
 * <p>This implementation does not inject the elements at the same step of the JUnit 5 life-cycle
 * as the standard Xtext {@link InjectionExtension}. Moreover, this extension does
 * a restore of the injection provider in order to avoid unstable state of the injected components.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version tests.api 0.15.0 20250909-115746
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid tests.api
 * @since 0.11
 * @see InjectionExtension
 */
public class AbstractInjectorExtension implements Extension {
	
	/** Create the injector and inject the given instance.
	 *
	 * @param instance the instance to inject
	 * @param context the extension context. 
	 * @throws Exception 
	 */
	@SuppressWarnings("static-method")
	public void injectMembers(Object instance, ExtensionContext context) throws Exception {
		var injectorProvider = createInjectorProvider(context);
		if (injectorProvider != null) {
			var injector = injectorProvider.getInjector();
			if (injector != null)
				injector.injectMembers(instance);
		}
	}
	
	private static IInjectorProvider createInjectorProvider(ExtensionContext context) {
		var injectWith = context.getRequiredTestClass().getAnnotation(InjectWith.class);
		if (injectWith != null) {
			var klass = injectWith.value();
			try {
				// TODO this mode of creation is not efficient, but it ensures that the injector providers are really reset.
				final var injectorProvider = klass.getDeclaredConstructor().newInstance();
				if (injectorProvider instanceof IRegistryConfigurator rvalue) {
					rvalue.setupRegistry();
				}
				return injectorProvider;
			} catch (Exception e) {
				throwUncheckedException(e);
			}
		}
		throw new IllegalStateException("Expected valid @InjectWith annotation"); //$NON-NLS-1$
	}
	
}
