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
package io.sarl.eclipse;


/**
 * Provides the constants for the SARL projects.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SARLConfig {

	/** Path of the Java source files.
	 */
	public static final String FOLDER_SOURCE_JAVA = "src/main/java"; //$NON-NLS-1$

	/** Path of the SARL source files.
	 */
	public static final String FOLDER_SOURCE_SARL = "src/main/sarl"; //$NON-NLS-1$

	/** Path of the generated source files.
	 */
	public static final String FOLDER_SOURCE_GENERATED = "src/main/generated-sources/xtend"; //$NON-NLS-1$

	/** Path of the Java source files.
	 */
	public static final String FOLDER_TEST_SOURCE_SARL = "src/test/sarl"; //$NON-NLS-1$

	/** Path of the SARL source files.
	 */
	public static final String FOLDER_TEST_SOURCE_GENERATED = "src/test/generated-sources/xtend"; //$NON-NLS-1$

	/** Path of the generated source files that should be no more used when creating
	 * new projects. This value is the default generation folder form Xtext.
	 */
	public static final String FOLDER_SOURCE_GENERATED_XTEXT = "src-gen"; //$NON-NLS-1$

	/** Path of the resource files.
	 */
	public static final String FOLDER_RESOURCES = "src/main/resources"; //$NON-NLS-1$

	/** Path of the binary files.
	 */
	public static final String FOLDER_BIN = "target/classes"; //$NON-NLS-1$

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
	 * Status code indicating a launch configuration does not
	 * specify an agent name to launch.
	 */
	public static final int ERR_UNSPECIFIED_AGENT_NAME = 501;

	/** Minimal version of the JRE supported by the SARL launch application.
	 */
	public static final String MINIMAL_JRE_VERSION = "1.7"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wiazrd dialog when creating new SARL project.
	 */
	public static final String NEW_PROJECT_WIZARD_DIALOG_IMAGE = "icons/sarl_new_project_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wiazrd dialog when creating new SARL file.
	 */
	public static final String NEW_FILE_WIZARD_DIALOG_IMAGE = "icons/sarl_new_file_dialog.png"; //$NON-NLS-1$

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

	private SARLConfig() {
		//
	}

}
