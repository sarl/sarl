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

package io.sarl.sarldoc.modules.internal;

import javax.inject.Singleton;

import com.google.inject.AbstractModule;
import com.google.inject.Injector;
import com.google.inject.Provides;
import org.arakhne.afc.vmutil.DynamicURLClassLoader;

/** Module for injecting the dynamic class loader.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarldocDynamicClassLoaderModule extends AbstractModule {

	@Override
	protected void configure() {
		//
	}

	/** Create the dynamic class loader.
	 *
	 * @param injector the current injector.
	 * @return the class loader;
	 */
	@SuppressWarnings("static-method")
	@Provides
	@Singleton
	@SarldocDynamicClassLoader
	public DynamicURLClassLoader providesDynamicURLClassLoader(Injector injector) {
		return DynamicURLClassLoader.newInstance(injector.getClass().getClassLoader());
	}

}
