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

package io.sarl.lang.compiler;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.inject.Inject;
import com.google.inject.name.Named;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.util.internal.EmfAdaptable;
import org.eclipse.xtext.xbase.lib.Pure;

/** Provider of a generator configuration v2.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.4
 */
public class GeneratorConfigProvider2 implements IGeneratorConfigProvider2 {

	@Inject
	@Named(Constants.LANGUAGE_NAME)
	private String languageId;

	@Override
	public GeneratorConfig2 get(EObject context) {
		final Resource eResource;
		if (context != null) {
			eResource = context.eResource();
		} else {
			eResource = null;
		}
		final ResourceSet resourceSet;
		if (eResource != null) {
			resourceSet = eResource.getResourceSet();
		} else {
			resourceSet = null;
		}
		if (resourceSet != null) {
			final GeneratorConfigAdapter adapter = GeneratorConfigAdapter.findInEmfObject(resourceSet);
			if (adapter != null && adapter.getLanguage2GeneratorConfig().containsKey(this.languageId)) {
				return adapter.getLanguage2GeneratorConfig().get(this.languageId);
			}
		}
		final GeneratorConfig2 config = new GeneratorConfig2();
		return config;
	}

	/** Install the given configuration in the resource set.
	 *
	 * @param resourceSet the resource set.
	 * @param config the ne configuration.
	 * @return the configuration.
	 */
	public GeneratorConfig2 install(final ResourceSet resourceSet, GeneratorConfig2 config) {
		GeneratorConfigAdapter adapter = GeneratorConfigAdapter.findInEmfObject(resourceSet);
		if (adapter == null) {
			adapter = new GeneratorConfigAdapter();
		}
		adapter.attachToEmfObject(resourceSet);
		return adapter.getLanguage2GeneratorConfig().put(this.languageId, config);
	}

	/** Adapter for providing the SARL generator configuration.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@EmfAdaptable
	public static class GeneratorConfigAdapter {

		private final Map<String, GeneratorConfig2> language2GeneratorConfig = new HashMap<>();

		/** Find the adapter in the EMF object.
		 *
		 * @param emfObject the EMF object.
		 * @return the adapter or {@code null} if no object is available.
		 */
		public static GeneratorConfigAdapter findInEmfObject(final Notifier emfObject) {
			if (emfObject != null) {
				for (final Adapter adapter : emfObject.eAdapters()) {
					if (adapter instanceof GeneratorConfigAdapter.GeneratorConfigAdapterAdapter) {
						return ((GeneratorConfigAdapter.GeneratorConfigAdapterAdapter) adapter).get();
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
		public static GeneratorConfigAdapter removeFromEmfObject(final Notifier emfObject) {
			final List<Adapter> adapters = emfObject.eAdapters();
			final Iterator<Adapter> iterator = adapters.iterator();
			while (iterator.hasNext()) {
				final Adapter adapter = iterator.next();
				if (adapter instanceof GeneratorConfigAdapter.GeneratorConfigAdapterAdapter) {
					iterator.remove();
					return ((GeneratorConfigAdapter.GeneratorConfigAdapterAdapter) adapter).get();
				}
			}
			return null;
		}

		/** Attach this adapter to the EMF object.
		 *
		 * @param emfObject the EMF object.
		 */
		public void attachToEmfObject(Notifier emfObject) {
			final GeneratorConfigAdapter result = findInEmfObject(emfObject);
			if (result != null) {
				throw new IllegalStateException(Messages.GeneratorConfigProvider2_0);
			}
			final GeneratorConfigAdapter.GeneratorConfigAdapterAdapter adapter = new GeneratorConfigAdapter.GeneratorConfigAdapterAdapter(this);
			emfObject.eAdapters().add(adapter);
		}

		/** Replies the configuration map.
		 *
		 * @return the map.
		 */
		@Pure
		public Map<String, GeneratorConfig2> getLanguage2GeneratorConfig() {
			return this.language2GeneratorConfig;
		}

		/** EMF Adapter.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		public static class GeneratorConfigAdapterAdapter extends AdapterImpl {

			private final GeneratorConfigAdapter element;

			/** Constructor.
			 *
			 * @param element the linked adapter.
			 */
			public GeneratorConfigAdapterAdapter(GeneratorConfigAdapter element) {
				this.element = element;
			}

			/** Replies the linked adapter.
			 *
			 * @return the linked adapter.
			 */
			public GeneratorConfigAdapter get() {
				return this.element;
			}

			@Override
			public boolean isAdapterForType(Object object) {
				return object == GeneratorConfigAdapter.class;
			}

		}

	}

}
