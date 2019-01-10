/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import javax.inject.Inject;

import com.google.inject.Injector;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchConfigurationTabGroupViewer;
import org.eclipse.debug.internal.ui.launchConfigurations.LaunchConfigurationsDialog;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaClasspathTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaDependenciesTab;
import org.eclipse.jdt.launching.JavaRuntime;

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

	@Override
	protected void setTabs(ILaunchConfigurationTab[] tabs) {
		//Override the function for automatic injection within the tabs.
		if (this.injector != null) {
			for (final ILaunchConfigurationTab tab : tabs) {
				this.injector.injectMembers(tab);
			}
		}
		//
		super.setTabs(tabs);
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
