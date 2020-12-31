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

package io.sarl.eclipse.explorer;

import javax.inject.Inject;

import org.eclipse.jdt.internal.ui.packageview.PackageExplorerContentProvider;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerLabelProvider;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;

/**
 * Provides the labels for the Package Explorer.
 *
 * <p>This provider inherits its behavior from the JDT package explorer label provider,
 * and add the following changes:
 * <ul>
 * <li>the icon of the package fragments are computed in order to consider the SARL scripts inside.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SARLPackageExplorerLabelProvider extends PackageExplorerLabelProvider {

	/**
	 * Constuctor.
	 *
	 * @param contentProvider the provider of content.
	 */
	public SARLPackageExplorerLabelProvider(PackageExplorerContentProvider contentProvider) {
		super(contentProvider);
	}

	/** Change the image provider.
	 *
	 * @param provider the new image provider.
	 */
	@Inject
	public void setImageLabelProvider(JavaElementImageProvider provider) {
		if (provider != null) {
			this.fImageLabelProvider = provider;
		}
	}

}
