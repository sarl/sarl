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

package io.sarl.eclipse.launching.dialog;

import java.util.ArrayList;
import java.util.List;

import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.debug.core.ILaunchConfiguration;
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

import io.sarl.apputils.eclipseextensions.launching.ISarlRuntimeEnvironmentTab;
import io.sarl.apputils.eclipseextensions.launching.ISreChangeListener;
import io.sarl.apputils.eclipseextensions.launching.SarlLaunchConfigurationPanels;

/**
 * Abstract tab group for run configurations of a SARL element.
 *
 * <p>This class provides the tools for helping to build a tab group for running SARL agents and SARL applications.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
@SuppressWarnings("restriction")
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
			for (final var tab : tabs) {
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
	@SuppressWarnings("static-method")
	protected ILaunchConfigurationTab[] buildTabList(
			ILaunchConfigurationDialog dialog,
			String mode,
			Function1<List<ILaunchConfigurationTab>, Boolean> builder) {
		assert builder != null;
		final var list = new ArrayList<ILaunchConfigurationTab>();

		final var addStandardPanels = builder.apply(list);

		// Find the SARL run-time environment tab
		ISarlRuntimeEnvironmentTab runtimeTab = null;
		for (final var tab : list) {
			if (tab instanceof ISarlRuntimeEnvironmentTab cvalue) {
				runtimeTab = cvalue;
				break;
			}
		}

		final var factories = SarlLaunchConfigurationPanels.getPanelFactoriesFromExtension();
		for (final var factory : factories) {
			if (factory.canCreatePanel(dialog, mode, list, runtimeTab)) {
				final var panel = factory.newLaunchConfigurationPanel(dialog, mode, list, runtimeTab);
				if (panel != null) {
					list.add(panel);
					if (panel instanceof ISreChangeListener cvalue && runtimeTab != null) {
						runtimeTab.addSreChangeListener(cvalue);
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

		final var array = new ILaunchConfigurationTab[list.size()];
		list.toArray(array);
		return array;
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
			for (final var tab : listOfTabs) {
				if (tab instanceof ISarlRuntimeEnvironmentTab cvalue) {
					final var runtimeTab = cvalue;
					for (final var listener : listeners) {
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
		if (dialog instanceof LaunchConfigurationsDialog cvalue) {
			final var tabViewer = cvalue.getTabViewer();
			if (tabViewer != null) {
				final var input = tabViewer.getInput();
				if (input instanceof ILaunchConfiguration configuration) {
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
