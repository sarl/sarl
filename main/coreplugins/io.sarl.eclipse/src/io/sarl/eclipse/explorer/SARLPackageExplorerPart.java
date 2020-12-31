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

import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.filters.EmptyInnerPackageFilter;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerContentProvider;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerLabelProvider;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerPart;
import org.eclipse.jdt.internal.ui.viewsupport.DecoratingJavaLabelProvider;
import org.eclipse.jdt.internal.ui.viewsupport.ProblemTreeViewer;
import org.eclipse.jdt.ui.IPackagesViewPart;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.contexts.IContextService;
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

	private static final String CONTEXT_ID = ID_PACKAGES;

	private static final String TAG_LAYOUT = "layout"; //$NON-NLS-1$

	private static final String TAG_GROUP_LIBRARIES = "group_libraries"; //$NON-NLS-1$

	private static final int FLAT_LAYOUT = 0x2;

	private static final boolean DEFAULT_SHOW_LIBRARIES = false;

	private static final boolean DEFAULT_SHOW_FLAT = false;

	private final ReflectExtensions reflect;

	private final IPackageExplorerLabelProviderBuilder packageExplorerLabelProviderProvider;

	/**
	 * Constructor.
	 *
	 * @param reflect the reflection utilities.
	 * @param builder the builder.
	 * @throws Error a runtime exception.
	 */
	@Inject
	public SARLPackageExplorerPart(ReflectExtensions reflect, IPackageExplorerLabelProviderBuilder builder) {
		this.reflect = reflect;
		this.packageExplorerLabelProviderProvider = builder;
		//
		// Trick for overriding the dialog settings with nicer default values.
		//
		final IDialogSettings dialogSettings = getDialogSettings();
		try {
			this.reflect.set(this, "fShowLibrariesNode", //$NON-NLS-1$
					dialogSettings.get(TAG_GROUP_LIBRARIES) == null ? DEFAULT_SHOW_LIBRARIES
					: dialogSettings.getBoolean(TAG_GROUP_LIBRARIES));

			try {
				this.reflect.set(this, "fIsCurrentLayoutFlat", //$NON-NLS-1$
						dialogSettings.getInt(TAG_LAYOUT) == FLAT_LAYOUT);
			} catch (NumberFormatException e) {
				this.reflect.set(this, "fIsCurrentLayoutFlat", DEFAULT_SHOW_FLAT); //$NON-NLS-1$
			}
		} catch (SecurityException | NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
			throw new Error(e);
		}
	}

	/** Replies the dialog settings for this view.
	 *
	 * @return the settings.
	 * @throws Error a runtime exception.
	 */
	protected IDialogSettings getDialogSettings() {
		try {
			return (IDialogSettings) this.reflect.get(this, "fDialogSettings"); //$NON-NLS-1$
		} catch (SecurityException | NoSuchFieldException | IllegalArgumentException | IllegalAccessException e) {
			throw new Error(e);
		}
	}

	/**
	 * Returns the package explorer part of the active perspective. If
	 * there isn't any package explorer part {@code null} is returned.
	 *
	 * @return the package explorer from the active perspective
	 */
	public static SARLPackageExplorerPart getFromActivePerspective() {
		final IWorkbenchPage activePage = JavaPlugin.getActivePage();
		if (activePage == null) {
			return null;
		}
		final IViewPart view = activePage.findView(ID_PACKAGES);
		if (view instanceof PackageExplorerPart) {
			return (SARLPackageExplorerPart) view;
		}
		return null;
	}

	/**
	 * Makes the package explorer part visible in the active perspective. If there
	 * isn't a package explorer part registered {@code null} is returned.
	 * Otherwise the opened view part is returned.
	 *
	 * @return the opened package explorer
	 */
	public static SARLPackageExplorerPart openInActivePerspective() {
		try {
			return (SARLPackageExplorerPart) JavaPlugin.getActivePage().showView(ID_PACKAGES);
		} catch (PartInitException exception) {
			return null;
		}
	}

	@Override
	public void createPartControl(Composite parent) {
		// Open the context
		final IContextService contextService = getSite().getService(IContextService.class);
		contextService.activateContext(CONTEXT_ID);
		// Overridden for setting the label provider, which is set in a private function in the super type.
		super.createPartControl(parent);
		internalResetLabelProvider();
		restoreFilterAndSorter();
	}

	/** Restore the filters and sorters.
	 */
	protected void restoreFilterAndSorter() {
		final ProblemTreeViewer viewer = getViewer();
		viewer.addFilter(new EmptyInnerPackageFilter());
		viewer.addFilter(new HiddenFileFilter());
	}

	@Override
	public void rootModeChanged(int newMode) {
		// Overridden for setting the label provider, which is set in a private function in the super type.
		super.rootModeChanged(newMode);
		internalResetLabelProvider();
	}

	/** Reset the label provider for using the SARL one.
	 * @throws Error a runtime exception.
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
	 * @throws Error a runtime exception.
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
	 * @throws Error a runtime exception.
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
	 * @throws Error a runtime exception.
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
