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

import java.util.List;

import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

/**
 * Interface that is implemented a factory of launch configurations panels dedicated to SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public interface ISarlLaunchConfigurationPanelFactory {

	/** Create the instance of the launch configuration panel.
	 *
	 * @param dialog the launch configuration panel.
	 * @param mode the running mode.
	 * @param list the list of panels that were already created.
	 * @param runtimeTab the reference to the runtime tab if it is added to the dialog box. It may
	 *     be {@code null} if no run-time panel was added into the list.
	 * @return the instance.
	 */
	ILaunchConfigurationTab newLaunchConfigurationPanel(ILaunchConfigurationDialog dialog, String mode,
			List<ILaunchConfigurationTab> list, ISarlRuntimeEnvironmentTab runtimeTab);

	/** Determine if the panel should be created with {@link #newLaunchConfigurationPanel()}.
	 *
	 * @param dialog the launch configuration panel.
	 * @param mode the running mode.
	 * @param list the list of panels that were already created.
	 * @param runtimeTab the reference to the runtime tab if it is added to the dialog box.
	 * @return {@code true} if the launch configuration panel should be created.
	 */
	default boolean canCreatePanel(ILaunchConfigurationDialog dialog, String mode, List<ILaunchConfigurationTab> list,
			ISarlRuntimeEnvironmentTab runtimeTab) {
		return true;
	}

}
