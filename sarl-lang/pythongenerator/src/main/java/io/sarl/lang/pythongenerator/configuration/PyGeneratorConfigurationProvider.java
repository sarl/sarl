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

package io.sarl.lang.pythongenerator.configuration;

import java.util.HashMap;
import java.util.Map;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.util.internal.EmfAdaptable;
import org.eclipse.xtext.xbase.lib.Pure;


/** Provider of a configuration for the SARL-to-Python generator.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version pythongenerator 0.14.0 20241106-161406
 * @mavengroupid io.sarl.lang
 * @mavenartifactid pythongenerator
 * @since 0.8
 */
@Singleton
public class PyGeneratorConfigurationProvider implements IPyGeneratorConfigurationProvider {

	@Inject
	@Named(Constants.LANGUAGE_NAME)
	private String languageId;

	@Override
	public PyGeneratorConfiguration get(Resource context, boolean installedIfNew) {
		final ResourceSet resourceSet;
		if (context != null) {
			resourceSet = context.getResourceSet();
		} else {
			resourceSet = null;
		}
		if (resourceSet != null) {
			final var adapter = PyGeneratorConfigAdapter.findInEmfObject(resourceSet);
			if (adapter != null && adapter.getLanguage2GeneratorConfig().containsKey(this.languageId)) {
				return adapter.getLanguage2GeneratorConfig().get(this.languageId);
			}
		}
		final var config = createConfiguration(context);
		if (installedIfNew && resourceSet != null) {
			install(resourceSet, config);
		}
		return config;
	}

	/** Create an instance of configuration.
	 *
	 * @param context the context of the creation.
	 * @return the new instance.
	 */
	@SuppressWarnings("static-method")
	protected PyGeneratorConfiguration createConfiguration(Resource context) {
		return new PyGeneratorConfiguration();
	}

	/** Install the given configuration into the context.
	 *
	 * @param resourceSet the target of the installation.
	 * @param config the configuration to install.
	 * @return the old configuration if one was previously installed.
	 */
	public PyGeneratorConfiguration install(ResourceSet resourceSet, PyGeneratorConfiguration config) {
		assert config != null;
		var adapter = PyGeneratorConfigAdapter.findInEmfObject(resourceSet);
		if (adapter == null) {
			adapter = new PyGeneratorConfigAdapter();
		}
		adapter.attachToEmfObject(resourceSet);
		return adapter.getLanguage2GeneratorConfig().put(this.languageId, config);
	}

	/** Adapter for providing the SARL generator configuration.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version pythongenerator 0.14.0 20241106-161406
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid pythongenerator
	 */
	@EmfAdaptable
	public static class PyGeneratorConfigAdapter {

		private final Map<String, PyGeneratorConfiguration> language2GeneratorConfig = new HashMap<>();

		/** Find the adapter in the EMF object.
		 *
		 * @param emfObject the EMF object.
		 * @return the adapter or {@code null} if no object is available.
		 */
		public static PyGeneratorConfigAdapter findInEmfObject(final Notifier emfObject) {
			if (emfObject != null) {
				for (final var adapter : emfObject.eAdapters()) {
					if (adapter instanceof PyGeneratorConfigAdapter.PyGeneratorConfigAdapterAdapter cvalue) {
						return cvalue.get();
					}
				}
			}
			return null;
		}

		/** Remove the adapter from the given EMF object.
		 *
		 * @param emfObject the EMF object.
		 * @return the removed adapter.
		 */
		public static PyGeneratorConfigAdapter removeFromEmfObject(final Notifier emfObject) {
			final var adapters = emfObject.eAdapters();
			final var iterator = adapters.iterator();
			while (iterator.hasNext()) {
				final var adapter = iterator.next();
				if (adapter instanceof PyGeneratorConfigAdapter.PyGeneratorConfigAdapterAdapter cvalue) {
					iterator.remove();
					return cvalue.get();
				}
			}
			return null;
		}

		/** Attach this adapter to the EMF object.
		 *
		 * @param emfObject the EMF object.
		 */
		public void attachToEmfObject(Notifier emfObject) {
			final var result = findInEmfObject(emfObject);
			if (result != null) {
				throw new IllegalStateException();
			}
			final var adapter = new PyGeneratorConfigAdapter.PyGeneratorConfigAdapterAdapter(this);
			emfObject.eAdapters().add(adapter);
		}

		/** Replies the configuration map.
		 *
		 * @return the map.
		 */
		@Pure
		public Map<String, PyGeneratorConfiguration> getLanguage2GeneratorConfig() {
			return this.language2GeneratorConfig;
		}

		/** EMF Adapter.
		 *
		 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
		 * @version pythongenerator 0.14.0 20241106-161406
		 * @mavengroupid io.sarl.lang
		 * @mavenartifactid pythongenerator
		 */
		public static class PyGeneratorConfigAdapterAdapter extends AdapterImpl {

			private final PyGeneratorConfigAdapter element;

			/** Constructor.
			 *
			 * @param element the linked adapter.
			 */
			public PyGeneratorConfigAdapterAdapter(PyGeneratorConfigAdapter element) {
				this.element = element;
			}

			/** Replies the linked adapter.
			 *
			 * @return the linked adapter.
			 */
			public PyGeneratorConfigAdapter get() {
				return this.element;
			}

			@Override
			public boolean isAdapterForType(Object object) {
				return object == PyGeneratorConfigAdapter.class;
			}

		}

	}

}
