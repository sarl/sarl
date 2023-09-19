/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.explorer;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.google.inject.Binder;
import com.google.inject.Injector;
import com.google.inject.Module;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerContentProvider;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerLabelProvider;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;

/**
 * Provides the labels for the Package Explorer.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.13.0 20230919-093100
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.6
 */
@Singleton
public class DefaultPackageExplorerLabelProviderBuilder implements IPackageExplorerLabelProviderBuilder {

	private Injector injector;

	/** Change the injector.
	 *
	 * @param injector the injector.
	 */
	@Inject
	public void setInjector(Injector injector) {
		this.injector = injector.createChildInjector(new PrivateModule());
	}

	@Override
	public PackageExplorerLabelProvider newInstance(PackageExplorerContentProvider contentProvider) {
		final SARLPackageExplorerLabelProvider provider = new SARLPackageExplorerLabelProvider(contentProvider);
		this.injector.injectMembers(provider);
		return provider;
	}

	/** Private injection module.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version io.sarl.eclipse 0.13.0 20230919-093100
	 * @mavengroupid io.sarl.eclipse
	 * @mavenartifactid io.sarl.eclipse
	 */
	private static class PrivateModule implements Module {

		PrivateModule() {
			//
		}

		@Override
		public void configure(Binder binder) {
			binder.bind(JavaElementImageProvider.class).to(SARLElementImageProvider.class);
		}

	}

}
