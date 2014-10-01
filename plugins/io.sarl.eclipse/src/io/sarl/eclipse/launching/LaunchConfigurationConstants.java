/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.eclipse.util.PluginUtil;

/**
 * Constant definitions for SARL launch configurations.
 * <p>
 * Constant definitions only.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @noimplement This interface is not intended to be implemented by clients.
 * @noextend This interface is not intended to be extended by clients.
 */
public final class LaunchConfigurationConstants {

	/**
	 * The value is the identifier of the class path container for
	 * the SARL runtime environment.
	 */
	public static final String RUNTIME_ENVIRONMENT_ID = PluginUtil.PLUGIN_ID + ".launching.SARL_RUNTIME"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is a fully qualified name
	 * of the agent to launch.
	 */
	public static final String ATTR_AGENT_NAME = PluginUtil.PLUGIN_ID + ".AGENT_NAME"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is the identifier of the SRE;
	 */
	public static final String ATTR_SARL_RUNTIME_ENVIRONMENT = PluginUtil.PLUGIN_ID + ".SARL_RUNTIME_ENVIRONMENT"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is a boolean that indicates if the system-wide SRE should be used.
	 */
	public static final String ATTR_USE_SARL_RUNTIME_ENVIRONMENT = PluginUtil.PLUGIN_ID
			+ ".USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT"; //$NON-NLS-1$

	 /**
	 * Status code indicating a launch configuration does not
	 * specify an agent name to launch.
	 */
	public static final int ERR_UNSPECIFIED_AGENT_NAME = 501;

	/** Minimal version of the JRE supported by the SARL launch application.
	 */
	public static final String MINIMAL_JRE_VERSION = "1.7"; //$NON-NLS-1$

	private LaunchConfigurationConstants() {
		//
	}

}
