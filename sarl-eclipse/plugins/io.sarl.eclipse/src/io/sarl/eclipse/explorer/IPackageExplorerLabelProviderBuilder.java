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

package io.sarl.eclipse.explorer;

import com.google.inject.ImplementedBy;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerContentProvider;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerLabelProvider;

/**
 * Provides the labels for the Package Explorer.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.6
 */
@SuppressWarnings("restriction")
@ImplementedBy(DefaultPackageExplorerLabelProviderBuilder.class)
public interface IPackageExplorerLabelProviderBuilder {

	/** Create a label provider for the package explorer.
	 *
	 * @param contentProvider the provider of content.
	 * @return the provider
	 */
	PackageExplorerLabelProvider newInstance(PackageExplorerContentProvider contentProvider);

}
