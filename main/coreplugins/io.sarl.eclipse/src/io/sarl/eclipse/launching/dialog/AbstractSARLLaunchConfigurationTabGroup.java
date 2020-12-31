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

package io.sarl.eclipse.launching.dialog;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.inject.Inject;

import com.google.inject.Injector;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchConfigurationTabGroupViewer;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchConfigurationsDialog;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.EnvironmentTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.debug.ui.sourcelookup.SourceLookupTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaClasspathTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaDependenciesTab;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.xtext.xbase.lib.Functions.Function1;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * Abstract tab group for run configurations of a SARL element.
 *
 * <p>This class provides the tools for helping to build a tab group for running SARL agents and SARL applications.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public abstract class AbstractSARLLaunchConfigurationTabGroup extends AbstractLaunchConfigurationTabGroup {

	@Inject
	private Injector injector;

	/** Constructor.
	 */
	public AbstractSARLLaunchConfigurationTabGroup() {
		//
	}

	/** {@inheritDoc}
	 *
	 * <p>This function is overridden in order to inject the members of the tabs.
	 *
	 * @param tabs the tabs to add into the panel.
	 */
	@Override
	protected void setTabs(ILaunchConfigurationTab... tabs) {
		//Override the function for automatic injection within the tabs.
		if (this.injector != null) {
			for (final ILaunchConfigurationTab tab : tabs) {
				this.injector.injectMembers(tab);
			}
		}
		//
		super.setTabs(tabs);
	}

	/** Build the list of the tabs to be inserted into the launch configuration panel.
	 *
	 * <p>This function adds the panels that are provided by the given {@code builder}, and
	 * the panels that are provided by the plugin contributions.
	 *
	 * <p>The {@code builder} is invoked with an empty list of panels.
	 * The panels that are given by extension points are added after.
	 * If the boolean value replied by {@code builder} is evaluated to {@code true}, the following standard panels
	 * are automatically added to the launch configuration after the ones provided by the extension points:<ul>
	 * <li>the class-path tab,</li>
	 * <li>the source look-up tab,</li>
	 * <li>the environment variable tab,</li>
	 * <li>the common tab.</li>
	 * </ul>
	 *
	 * @param dialog is the reference to the launch configuration dialog box.
	 * @param mode the running mode.
	 * @param builder the builder that is able to fill out the list of panels. The parameter of the procedure
	 *     is the list to fill with the panels.
	 * @return the list of tabs to be added into the panel, before they are injected.
	 * @since 0.12
	 * @see #setTabs(ILaunchConfigurationTab...)
	 */
	protected ILaunchConfigurationTab[] buildTabList(
			ILaunchConfigurationDialog dialog,
			String mode,
			Function1<List<ILaunchConfigurationTab>, Boolean> builder) {
		assert builder != null;
		final List<ILaunchConfigurationTab> list = new ArrayList<>();

		final Boolean addStandardPanels = builder.apply(list);

		// Find the SARL run-time environment tab
		ISarlRuntimeEnvironmentTab runtimeTab = null;
		for (final ILaunchConfigurationTab tab : list) {
			if (tab instanceof ISarlRuntimeEnvironmentTab) {
				runtimeTab = (ISarlRuntimeEnvironmentTab) tab;
				break;
			}
		}

		final List<ISarlLaunchConfigurationPanelFactory> factories = getFactoriesFromExtension();
		for (final ISarlLaunchConfigurationPanelFactory factory : factories) {
			if (factory.canCreatePanel(dialog, mode, list, runtimeTab)) {
				final ILaunchConfigurationTab panel = factory.newLaunchConfigurationPanel(dialog, mode, list, runtimeTab);
				if (panel != null) {
					list.add(panel);
					if (panel instanceof ISreChangeListener) {
						runtimeTab.addSreChangeListener((ISreChangeListener) panel);
					}
				}
			}
		}

		if (addStandardPanels == null || addStandardPanels.booleanValue()) {
			list.add(getClasspathTab(dialog));
			list.add(new SourceLookupTab());
			list.add(new EnvironmentTab());
			list.add(new CommonTab());
		}

		final ILaunchConfigurationTab[] array = new ILaunchConfigurationTab[list.size()];
		list.toArray(array);
		return array;
	}

	private static List<ISarlLaunchConfigurationPanelFactory> getFactoriesFromExtension() {
		final IExtensionPoint extensionPoint = Platform.getExtensionRegistry().getExtensionPoint(
				SARLEclipsePlugin.PLUGIN_ID,
				SARLEclipseConfig.EXTENSION_POINT_LAUNCH_CONFIGURATION_PANEL_FACTORY);
		if (extensionPoint != null) {
			final List<ISarlLaunchConfigurationPanelFactory> factories = new ArrayList<>();
			for (final IConfigurationElement element : extensionPoint.getConfigurationElements()) {
				try {
					final Object obj = element.createExecutableExtension("class"); //$NON-NLS-1$
					if (obj instanceof ISarlLaunchConfigurationPanelFactory) {
						factories.add((ISarlLaunchConfigurationPanelFactory) obj);
					} else {
						SARLEclipsePlugin.getDefault().logErrorMessage(
								"Cannot instance extension point: " + element.getName()); //$NON-NLS-1$
					}
				} catch (CoreException e) {
					SARLEclipsePlugin.getDefault().log(e);
				}
			}
			return factories;
		}
		return Collections.emptyList();
	}

	/** Register the given SRE listener on any change of SRE selection into the run-time environment tab.
	 * This function does nothing if there is no tab for the run-time environment into the given list.
	 *
	 * @param listOfTabs is the list of the registered tabs.
	 * @param listeners is the list of the objects to register as listener on the SRE selection changes.
	 * @since 0.12
	 */
	protected static void addSreChangeListeners(List<ILaunchConfigurationTab> listOfTabs, ISreChangeListener... listeners) {
		if (listOfTabs != null && listeners != null) {
			for (final ILaunchConfigurationTab tab : listOfTabs) {
				if (tab instanceof ISarlRuntimeEnvironmentTab) {
					final ISarlRuntimeEnvironmentTab runtimeTab = (ISarlRuntimeEnvironmentTab) tab;
					for (final ISreChangeListener listener : listeners) {
						runtimeTab.addSreChangeListener(listener);
					}
				}
			}
		}
	}

	/** Replies the preferred tab for configuring the classpath.
	 * The tab may be the standard {@link JavaClasspathTab} or its specialization
	 * {@link JavaDependenciesTab}.
	 *
	 * @param dialog the dialog that will contains the tab.
	 * @return the tab instance, never {@code null}.
	 */
	protected static JavaClasspathTab getClasspathTab(ILaunchConfigurationDialog dialog) {
		JavaClasspathTab tab = null;
		if (dialog instanceof LaunchConfigurationsDialog) {
			final LaunchConfigurationTabGroupViewer tabViewer = ((LaunchConfigurationsDialog) dialog).getTabViewer();
			if (tabViewer != null) {
				final Object input = tabViewer.getInput();
				if (input instanceof ILaunchConfiguration) {
					final ILaunchConfiguration configuration = (ILaunchConfiguration) input;
					if (JavaRuntime.isModularConfiguration(configuration)) {
						tab = new JavaDependenciesTab();
					}
				}
			}
		}
		if (tab == null) {
			tab = new JavaClasspathTab();
		}
		return tab;
	}

}
