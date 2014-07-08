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
package io.sarl.docs.utils;


import io.sarl.lang.SARLInjectorProvider;

import org.eclipse.xtext.junit4.IInjectorProvider;
import org.eclipse.xtext.junit4.IRegistryConfigurator;
import org.jnario.lib.AbstractSpecCreator;

import com.google.inject.Injector;

/** Creator of specification model dedicated to SARL.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLSpecCreator extends AbstractSpecCreator {

	private Injector injector;

	private final SARLInjectorProvider injectorProvider = new SARLInjectorProvider();

	@Override
	protected <T> T create(Class<T> klass) {
		if (this.injector == null) {
			beforeSpecRun();
		}
		return this.injector.getInstance(klass);
	}

	@Override
	public void beforeSpecRun() {
		this.injector = getInjectorProvider().getInjector();
		if (getInjectorProvider() instanceof IRegistryConfigurator) {
			((IRegistryConfigurator) getInjectorProvider()).setupRegistry();
		}
	}

	@Override
	public void afterSpecRun() {
		if (getInjectorProvider() instanceof IRegistryConfigurator) {
			((IRegistryConfigurator) getInjectorProvider()).restoreRegistry();
		}
	}

	/** Replies the injector provider.
	 *
	 * @return the injector provider.
	 */
	protected IInjectorProvider getInjectorProvider() {
		return this.injectorProvider;
	}

}
