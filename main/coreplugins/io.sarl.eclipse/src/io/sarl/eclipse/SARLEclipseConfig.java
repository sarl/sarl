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
	 * Default SARL file extension (with the dot).
	 *
	 * <p>TODO Find a way to get it from the io.sarl.lang code itself (which is generated).
	 */
	public static final String SARL_FILE_EXTENSION = ".sarl"; //$NON-NLS-1$

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
	 * ID of the SARL perspective.
	 */
	public static final String ID_SARL_PERSPECTIVE = "io.sarl.eclipse.perspective.devel"; //$NON-NLS-1$

	/**
	 * ID of the SARL perspective.
	 */
	public static final String ID_SARL_DEBUG_PERSPECTIVE = "io.sarl.eclipse.perspective.debug"; //$NON-NLS-1$

	/**
	 * The value is the identifier of the class path container for
	 * the SARL runtime environment.
	 */
	public static final String RUNTIME_ENVIRONMENT_ID = SARLEclipsePlugin.PLUGIN_ID + ".launching.SARL_RUNTIME"; //$NON-NLS-1$

	/**
	 * Status code indicating a launch configuration does not
	 * specify an agent name to launch.
	 */
	public static final int ERR_UNSPECIFIED_AGENT_NAME = 501;

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

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when importing a SARL project.
	 */
	public static final String IMPORT_PROJECT_WIZARD_DIALOG_IMAGE = "icons/sarl_import_project_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used for representing a SARL project.
	 */
	public static final String SARL_PROJECT_IMAGE = "icons/sarl_project_16.png"; //$NON-NLS-1$

	/** Filename of the image that is the SARL logo with an icon size (16x16).
	 */
	public static final String SARL_LOGO_IMAGE = "icons/sarl_16.png"; //$NON-NLS-1$

	/** Filename of the image that is the SARL logo with an icon size (16x16).
	 *
	 * @since 0.7
	 */
	public static final String SARL_APPLICATION_IMAGE = "icons/sarl_application_16.png"; //$NON-NLS-1$

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

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when submiting an issue to the SARL tracker.
	 */
	public static final String SUBMIT_ISSUE_WIZARD_DIALOG_IMAGE = "icons/sarl_submit_issue_dialog.png"; //$NON-NLS-1$

	/**
	 * Name of the extension points for the factory of launch configuration panels.
	 * (value <code>"sarlLaunchConfigurationPanels"</code>).
	 */
	public static final String EXTENSION_POINT_LAUNCH_CONFIGURATION_PANEL_FACTORY = "sarlLaunchConfigurationPanels"; //$NON-NLS-1$

	private SARLEclipseConfig() {
		//
	}

}
