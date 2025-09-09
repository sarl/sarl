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

package io.sarl.eclipse.sre.janus.network;

import java.util.List;

import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

import io.sarl.apputils.eclipseextensions.launching.ISarlLaunchConfigurationPanelFactory;
import io.sarl.apputils.eclipseextensions.launching.ISarlRuntimeEnvironmentTab;

/**
 * Provider of the launch configuration panel for the Janus network.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse.sre.janus 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.sre.janus
 * @since 0.12
 */
public class JanusLaunchNetworkTabFactory implements ISarlLaunchConfigurationPanelFactory {

	@Override
	public ILaunchConfigurationTab newLaunchConfigurationPanel(ILaunchConfigurationDialog dialog, String mode,
			List<ILaunchConfigurationTab> list, ISarlRuntimeEnvironmentTab runtimeTab) {
		final var tab = new JanusLaunchNetworkTab();
		/*if (runtimeTab != null) {
			runtimeTab.addSreChangeListener(tab);
		}*/
		return tab;
	}

}
