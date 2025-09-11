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

package io.sarl.lang.validation;

import org.eclipse.xtext.validation.IssueSeveritiesProvider;

import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Provider;
import com.google.inject.Singleton;

/** Provider of a configurable issue severity provider that is a singleton.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
public class ConfigurableIssueSeveritiesProviderProvider {

	private final Injector injector;

	private ConfigurableIssueSeveritiesProvider provider;
	
	/** Constructor.
	 *
	 * @param injector the injector.
	 */
	@Inject
	public ConfigurableIssueSeveritiesProviderProvider(Injector injector) {
		this.injector = injector;
	}
	
	/** Replies the SARL configuration implementation.
	 *
	 * @return the singleton.
	 */
	public synchronized ConfigurableIssueSeveritiesProvider getProvider() {
		if (this.provider == null) {
			this.provider = this.injector.getInstance(ConfigurableIssueSeveritiesProvider.class);
		}
		return this.provider;
	}

	/** Provider of a configurable issue severity provider that is a singleton.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@Singleton
	public static class SarlInterfaceProvider implements Provider<IConfigurableIssueSeveritiesProvider> {

		private final Provider<ConfigurableIssueSeveritiesProviderProvider> provider;
		
		/** Constructor.
		 *
		 * @param provider the original provider.
		 */
		@Inject
		public SarlInterfaceProvider(Provider<ConfigurableIssueSeveritiesProviderProvider> provider) {
			this.provider = provider;
		}

		@Override
		public IConfigurableIssueSeveritiesProvider get() {
			return this.provider.get().getProvider();
		}

	}

	/** Provider of a configurable issue severity provider that is a singleton.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.15
	 */
	@Singleton
	public static class XtextImplementationProvider implements Provider<IssueSeveritiesProvider> {

		private final Provider<ConfigurableIssueSeveritiesProviderProvider> provider;
		
		/** Constructor.
		 *
		 * @param provider the original provider.
		 */
		@Inject
		public XtextImplementationProvider(Provider<ConfigurableIssueSeveritiesProviderProvider> provider) {
			this.provider = provider;
		}

		@Override
		public IssueSeveritiesProvider get() {
			return this.provider.get().getProvider();
		}

	}

}
