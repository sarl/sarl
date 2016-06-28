/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.eclipse;


/**
 * Provides the constants for the SARL projects.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see io.sarl.lang.SARLConfig
 */
public final class SARLEclipseConfig {

	/**
	 * ID of the project nature defined by XText.
	 */
	public static final String XTEXT_NATURE_ID = "org.eclipse.xtext.ui.shared.xtextNature"; //$NON-NLS-1$

	/**
	 * ID of the project nature defined by Maven.
	 */
	public static final String MAVEN_NATURE_ID = "org.eclipse.m2e.core.maven2Nature"; //$NON-NLS-1$

	/**
	 * ID of this nature.
	 */
	public static final String NATURE_ID = "io.sarl.eclipse.SARLProjectNature"; //$NON-NLS-1$

	/**
	 * The value is the identifier of the class path container for
	 * the SARL runtime environment.
	 */
	public static final String RUNTIME_ENVIRONMENT_ID = SARLEclipsePlugin.PLUGIN_ID + ".launching.SARL_RUNTIME"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is a fully qualified name
	 * of the agent to launch.
	 */
	public static final String ATTR_AGENT_NAME = SARLEclipsePlugin.PLUGIN_ID + ".AGENT_NAME"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates the type of the root context id.
	 */
	public static final String ATTR_ROOT_CONTEXT_ID_TYPE = SARLEclipsePlugin.PLUGIN_ID + ".ROOT_CONTEXT_ID_TYPE"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates if the SRE logo
	 * should be displayed at start-up.
	 */
	public static final String ATTR_SHOW_LOGO_OPTION = SARLEclipsePlugin.PLUGIN_ID + ".SHOW_LOGO_OPTION"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates if the logged information messages
	 * should be displayed or not.
	 */
	public static final String ATTR_SHOW_LOG_INFO = SARLEclipsePlugin.PLUGIN_ID + ".SHOW_LOG_INFO"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value indicates if the SRE should be
	 * launched offline.
	 */
	public static final String ATTR_SRE_OFFLINE = SARLEclipsePlugin.PLUGIN_ID + ".SRE_OFFLINE"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is the identifier of the SRE;
	 */
	public static final String ATTR_SARL_RUNTIME_ENVIRONMENT = SARLEclipsePlugin.PLUGIN_ID
			+ ".SARL_RUNTIME_ENVIRONMENT"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is a boolean that indicates if the system-wide SRE should be used.
	 */
	public static final String ATTR_USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT = SARLEclipsePlugin.PLUGIN_ID
			+ ".USE_SYSTEM_SARL_RUNTIME_ENVIRONMENT"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is a boolean that indicates if the project SRE should be used.
	 */
	public static final String ATTR_USE_PROJECT_SARL_RUNTIME_ENVIRONMENT = SARLEclipsePlugin.PLUGIN_ID
			+ ".USE_PROJECT_SARL_RUNTIME_ENVIRONMENT"; //$NON-NLS-1$

	/**
	 * Launch configuration attribute key. The value is the arguments for the SRE;
	 */
	public static final String ATTR_SARL_RUNTIME_ENVIRONMENT_ARGUMENTS = SARLEclipsePlugin.PLUGIN_ID
			+ ".SARL_RUNTIME_ENVIRONMENT_ARGUMENTS"; //$NON-NLS-1$

	/**
	 * Status code indicating a launch configuration does not
	 * specify an agent name to launch.
	 */
	public static final int ERR_UNSPECIFIED_AGENT_NAME = 501;

	/** Minimal version of the JRE supported by the SARL launch application.
	 */
	public static final String MINIMAL_JRE_VERSION = "1.8"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when creating new SARL project.
	 */
	public static final String NEW_PROJECT_WIZARD_DIALOG_IMAGE = "icons/sarl_new_project_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when creating new SARL file.
	 */
	public static final String NEW_FILE_WIZARD_DIALOG_IMAGE = "icons/sarl_new_file_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when creating new SARL agent.
	 */
	public static final String NEW_AGENT_WIZARD_DIALOG_IMAGE = "icons/sarl_new_agent_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when creating new SARL behavior.
	 */
	public static final String NEW_BEHAVIOR_WIZARD_DIALOG_IMAGE = "icons/sarl_new_behavior_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when creating new SARL capacity.
	 */
	public static final String NEW_CAPACITY_WIZARD_DIALOG_IMAGE = "icons/sarl_new_capacity_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when creating new SARL event.
	 */
	public static final String NEW_EVENT_WIZARD_DIALOG_IMAGE = "icons/sarl_new_event_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when creating new SARL skill.
	 */
	public static final String NEW_SKILL_WIZARD_DIALOG_IMAGE = "icons/sarl_new_skill_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used for representing a SARL project.
	 */
	public static final String SARL_PROJECT_IMAGE = "icons/sarl_project_16.png"; //$NON-NLS-1$

	/** Filename of the image that is the SARL logo with an icon size (16x16).
	 */
	public static final String SARL_LOGO_IMAGE = "icons/sarl_16.png"; //$NON-NLS-1$

	/**
	 * Extension point identifier for contributions of a wizard page that for a ISREInstall
	 * (value <code>"sreInstallPage"</code>).
	 */
	public static final String EXTENSION_POINT_SRE_INSTALL_PAGES = "sreInstallPages"; //$NON-NLS-1$

	/**
	 * Name of the extension points for SRE installation
	 * (value <code>"sreInstallations"</code>).
	 */
	public static final String EXTENSION_POINT_SARL_RUNTIME_ENVIRONMENT = "sreInstallations"; //$NON-NLS-1$

	/**
	 * Name of the extension points for the factory of project SRE provider.
	 * (value <code>"projectSREProviderFactory"</code>).
	 */
	public static final String EXTENSION_POINT_PROJECT_SRE_PROVIDER_FACTORY = "projectSREProviderFactory"; //$NON-NLS-1$

	private SARLEclipseConfig() {
		//
	}

}
