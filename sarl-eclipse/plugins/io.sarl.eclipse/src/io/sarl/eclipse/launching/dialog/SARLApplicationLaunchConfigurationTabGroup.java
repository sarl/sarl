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

package io.sarl.eclipse.launching.dialog;

import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaArgumentsTab;

/**
 * Tab group object for configuring the run of a Java application embedding SARL.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse 0.13.0 20230919-093100
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.7
 */
public class SARLApplicationLaunchConfigurationTabGroup extends AbstractSARLLaunchConfigurationTabGroup {

	@Override
	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		final ILaunchConfigurationTab[] tabs = buildTabList(dialog, mode, list -> {
			// Add before the dynamically provided panels
			final SARLApplicationMainLaunchConfigurationTab mainTab = new SARLApplicationMainLaunchConfigurationTab();
			list.add(0, mainTab);
			list.add(1, new JavaArgumentsTab());
			list.add(2, new SARLRuntimeEnvironmentTab(false));

			addSreChangeListeners(list, mainTab);

			return true;
		});
		setTabs(tabs);
	}

}
