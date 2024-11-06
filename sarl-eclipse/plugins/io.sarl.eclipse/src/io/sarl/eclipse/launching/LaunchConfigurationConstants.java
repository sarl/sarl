/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.launching;

import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * Constants for a SARL launch configurations.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @since 0.8
 */
public final class LaunchConfigurationConstants {

	/**
	 * Identifier for a "agent launch configuration".
	 */
	public static final String ID_AGENT_LAUNCH_CONFIGURATION = SARLEclipsePlugin.PLUGIN_ID + ".debug.AgentLaunchConfigType"; //$NON-NLS-1$

	/**
	 * Identifier for an "application launch configuration".
	 */
	public static final String ID_APPLICATION_LAUNCH_CONFIGURATION = SARLEclipsePlugin.PLUGIN_ID + ".debug.ApplicationLaunchConfigType"; //$NON-NLS-1$

	private LaunchConfigurationConstants() {
		//
	}

}
