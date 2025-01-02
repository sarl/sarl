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

package io.sarl.lang.jvmmodel.fragments;

import java.util.Collection;
import java.util.Collections;
import java.util.ServiceLoader;

import org.eclipse.emf.ecore.EObject;

import com.google.common.collect.LinkedListMultimap;
import com.google.common.collect.Multimap;
import com.google.inject.Inject;
import com.google.inject.Injector;
import com.google.inject.Singleton;

/** Tool for obtaining all the fragment contributions to the JVM model inferrer.
 *
 * <p>The contributions are obtained from the definitions of Java plugins.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@Singleton
@SuppressWarnings("rawtypes")
public class JavaPluginInferrerFragmentContributions implements IInferrerFragmentContributions {

	@Inject
	private Injector injector;

	private ServiceLoader<ISingleStageInferrerFragment> serviceLoader0;

	private Multimap<Class<?>, ISingleStageInferrerFragment> contributions0;

	private ServiceLoader<IBiStageInferrerFragment> serviceLoader1;

	private Multimap<Class<?>, IBiStageInferrerFragment> contributions1;

	/** Replies the accessor to the Java plugins that constribute to the single-stage process without
	 * inherited transformer from Xbase or Xtend.
	 *
	 * @return the service loader.
	 */
	protected ServiceLoader<ISingleStageInferrerFragment> getServiceLoaderForSingleStage() {
		if (this.serviceLoader0 == null) {
			this.serviceLoader0 = ServiceLoader.load(ISingleStageInferrerFragment.class, getClass().getClassLoader());
		}
		return this.serviceLoader0;
	}
	
	@Override
	public Collection<ISingleStageInferrerFragment> getSingleStageFragmentContributions(Class<? extends EObject> type) {
		final var loader = getServiceLoaderForSingleStage();
		if (this.contributions0 == null) {
			this.contributions0 = LinkedListMultimap.create();
			for (final var contrib : loader) {
				this.injector.injectMembers(contrib);
				Class<?> type0 = contrib.getSupportedType();
				while (type0 != null && !EObject.class.equals(type0)) {
					this.contributions0.put(type0, contrib);
					type0 = type0.getSuperclass();
				}
			}
		}
		Class<?> type0 = type;
		while (type0 != null && !EObject.class.equals(type0)) {
			final var list = this.contributions0.get(type0);
			if (list != null) {
				return list;
			}
			type0 = type.getSuperclass();
		}
		return Collections.emptyList();
	}

	/** Replies the accessor to the Java plugins that constribute to the bi-stage process.
	 *
	 * @return the service loader.
	 */
	protected ServiceLoader<IBiStageInferrerFragment> getServiceLoaderForBiStage() {
		if (this.serviceLoader1 == null) {
			this.serviceLoader1 = ServiceLoader.load(IBiStageInferrerFragment.class, getClass().getClassLoader());
		}
		return this.serviceLoader1;
	}

	@Override
	public Collection<IBiStageInferrerFragment> getBiStageFragmentContributions(Class<? extends EObject> type) {
		final var loader = getServiceLoaderForBiStage();
		if (this.contributions1 == null) {
			this.contributions1 = LinkedListMultimap.create();
			for (final var contrib : loader) {
				this.injector.injectMembers(contrib);
				Class<?> type1 = contrib.getSupportedType();
				while (type1 != null && !EObject.class.equals(type1)) {
					this.contributions1.put(type1, contrib);
					type1 = type1.getSuperclass();
				}
			}
		}
		Class<?> type1 = type;
		while (type1 != null && !EObject.class.equals(type1)) {
			final var list = this.contributions1.get(type1);
			if (list != null) {
				return list;
			}
			type1 = type.getSuperclass();
		}
		return Collections.emptyList();
	}

}
