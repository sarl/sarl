/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import org.eclipse.jdt.internal.ui.filters.EmptyPackageFilter;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerContentProvider;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerLabelProvider;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerPart;
import org.eclipse.jdt.internal.ui.viewsupport.DecoratingJavaLabelProvider;
import org.eclipse.jdt.internal.ui.viewsupport.ProblemTreeViewer;
import org.eclipse.jdt.ui.IPackagesViewPart;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;

/**
 * The ViewPart for the Package Explorer into the SARL environment.
 *
 * <p>It listens to part activation events. When selection
 * linking with the editor is enabled the view selection tracks the active editor page.
 * Similarly when a resource is selected in the packages view the corresponding editor is activated.
 *
 * <p>This explorer inherits its behavior from the JDT package explorer, and add the following changes:
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
public class SARLPackageExplorerPart extends PackageExplorerPart {

	/**
	 * The view part id of the Packages view
	 * (value <code>"io.sarl.eclipse.explorer.PackageExplorer"</code>).
	 * <p>
	 * When this id is used to access
	 * a view part with <code>IWorkbenchPage.findView</code> or
	 * <code>showView</code>, the returned <code>IViewPart</code>
	 * can be safely cast to an <code>IPackagesViewPart</code>.
	 * </p>
	 *
	 * @see IPackagesViewPart
	 * @see org.eclipse.ui.IWorkbenchPage#findView(java.lang.String)
	 * @see org.eclipse.ui.IWorkbenchPage#showView(java.lang.String)
	 */
	public static final String ID_PACKAGES = "io.sarl.eclipse.explorer.PackageExplorer"; //$NON-NLS-1$

	private final ReflectExtensions reflect;

	private final IPackageExplorerLabelProviderBuilder packageExplorerLabelProviderProvider;

	/**
	 * Constructor.
	 *
	 * @param reflect the reflection utilities.
	 * @param builder the builder.
	 */
	@Inject
	public SARLPackageExplorerPart(ReflectExtensions reflect, IPackageExplorerLabelProviderBuilder builder) {
		this.reflect = reflect;
		this.packageExplorerLabelProviderProvider = builder;
	}

	@Override
	public void createPartControl(Composite parent) {
		// Overriden for setting the label provider, which is set in a private function in the super type.
		super.createPartControl(parent);
		internalResetLabelProvider();
		restoreFilterAndSorter();
	}

	/** Restore the filters and sorters.
	 */
	protected void restoreFilterAndSorter() {
		final ProblemTreeViewer viewer = getViewer();
		viewer.addFilter(new EmptyPackageFilter());
		viewer.addFilter(new HiddenFileFilter());
	}

	@Override
	public void rootModeChanged(int newMode) {
		// Overriden for setting the label provider, which is set in a private function in the super type.
		super.rootModeChanged(newMode);
		internalResetLabelProvider();
	}

	/** Reset the label provider for using the SARL one.
	 */
	protected void internalResetLabelProvider() {
		try {
			final PackageExplorerLabelProvider provider = createLabelProvider();
			this.reflect.set(this, "fLabelProvider", provider); //$NON-NLS-1$
			provider.setIsFlatLayout(isFlatLayout());
			final DecoratingJavaLabelProvider decoratingProvider = new DecoratingJavaLabelProvider(
					provider, false, isFlatLayout());
			this.reflect.set(this, "fDecoratingLabelProvider", decoratingProvider); //$NON-NLS-1$
			getViewer().setLabelProvider(decoratingProvider);

		} catch (SecurityException | NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
			throw new Error(e);
		}
	}

	/** Replies the label provider.
	 *
	 * @return the label provider.
	 */
	protected PackageExplorerLabelProvider getLabelProvider() {
		try {
			return (PackageExplorerLabelProvider) this.reflect.get(this, "fLabelProvider"); //$NON-NLS-1$
		} catch (SecurityException | NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
			throw new Error(e);
		}
	}

	/** Replies the viewer.
	 *
	 * @return the viewer.
	 */
	protected ProblemTreeViewer getViewer() {
		try {
			return (ProblemTreeViewer) this.reflect.get(this, "fViewer"); //$NON-NLS-1$
		} catch (SecurityException | NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
			throw new Error(e);
		}
	}

	/** Replies the label provider.
	 *
	 * @return the label provider.
	 */
	protected PackageExplorerContentProvider getContentProvider() {
		try {
			return (PackageExplorerContentProvider) this.reflect.get(this, "fContentProvider"); //$NON-NLS-1$
		} catch (SecurityException | NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
			throw new Error(e);
		}
	}

	/** Create a label provider.
	 *
	 * <p>A similatr function exists in the super type. But it has private access.
	 *
	 * @return the label provider.
	 */
	protected PackageExplorerLabelProvider createLabelProvider() {
		return this.packageExplorerLabelProviderProvider.newInstance(getContentProvider());
	}

}
