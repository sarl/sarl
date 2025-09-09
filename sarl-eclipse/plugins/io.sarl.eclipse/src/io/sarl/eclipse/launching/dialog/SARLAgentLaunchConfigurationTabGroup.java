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

import org.eclipse.debug.ui.ILaunchConfigurationDialog;

/**
 * Tab group object for configuration the run of an agent.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
public class SARLAgentLaunchConfigurationTabGroup extends AbstractSARLLaunchConfigurationTabGroup {

	@Override
	public void createTabs(ILaunchConfigurationDialog dialog, String mode) {
		final var tabs = buildTabList(dialog, mode, list -> {
			// Add before the dynamically provided panels
			final var mainTab = new SARLAgentMainLaunchConfigurationTab();
			list.add(0, mainTab);
			list.add(1, new SARLArgumentsTab());
			list.add(2, new SARLRuntimeEnvironmentTab(true));

			addSreChangeListeners(list, mainTab);

			return Boolean.TRUE;
		});
		setTabs(tabs);
	}

}
