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

package io.sarl.eclipse.runtime;

/**
 * Constants that are representing elements in the SRE's manifest file.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SREConstants {

	/** Field name in the manifest that provides the main class.
	 */
	public static final String MANIFEST_MAIN_CLASS = "Main-Class"; //$NON-NLS-1$

	/** Field name for the class path in the manifest file.
	 */
	public static final String MANIFEST_CLASS_PATH = "Class-Path"; //$NON-NLS-1$

	/** Section of the manifest file that is detected to SRE.
	 */
	public static final String MANIFEST_SECTION_SRE = "SARL-Runtime-Environment"; //$NON-NLS-1$

	/** Field name for the SARL specification version in the SRE section of the manifest file.
	 */
	public static final String MANIFEST_SARL_SPEC_VERSION = "SARL-Spec-Version"; //$NON-NLS-1$

	/** Field name for the SRE name in the SRE section of the manifest file.
	 */
	public static final String MANIFEST_SRE_NAME = "SRE-Name"; //$NON-NLS-1$

	/** Field name for the VM arguments in the SRE section of the manifest file.
	 */
	public static final String MANIFEST_VM_ARGUMENTS = "VM-Arguments"; //$NON-NLS-1$

	/** Field name for the program arguments in the SRE section of the manifest file.
	 */
	public static final String MANIFEST_PROGRAM_ARGUMENTS = "Program-Arguments"; //$NON-NLS-1$

	/** Field name for CLI option for showing the SRE logo.
	 */
	public static final String MANIFEST_CLI_SHOW_LOGO = "CLI-Show-Logo"; //$NON-NLS-1$

	/** Field name for CLI option for hiding the SRE logo.
	 */
	public static final String MANIFEST_CLI_HIDE_LOGO = "CLI-Hide-Logo"; //$NON-NLS-1$

	/** Field name for CLI option for showing the SRE logo.
	 */
	public static final String MANIFEST_CLI_SHOW_INFO = "CLI-Show-Info"; //$NON-NLS-1$

	/** Field name for CLI option for hiding the SRE logo.
	 */
	public static final String MANIFEST_CLI_HIDE_INFO = "CLI-Hide-Info"; //$NON-NLS-1$

	/** Field name for CLI option for using the default identifier of the root context.
	 */
	public static final String MANIFEST_CLI_DEFAULT_CONTEXT_ID = "CLI-Default-Context-ID"; //$NON-NLS-1$

	/** Field name for CLI option for using the random identifier of the root context.
	 */
	public static final String MANIFEST_CLI_RANDOM_CONTEXT_ID = "CLI-Random-Context-ID"; //$NON-NLS-1$

	/** Field name for CLI option for using the boot-agent-based identifier of the root context.
	 */
	public static final String MANIFEST_CLI_BOOT_AGENT_CONTEXT_ID = "CLI-BootAgent-Context-ID"; //$NON-NLS-1$

	/** Field name for CLI option for launching the SRE offline.
	 */
	public static final String MANIFEST_CLI_SRE_OFFLINE = "CLI-Offline"; //$NON-NLS-1$

	/** Field name for CLI option that disabling options.
	 */
	public static final String MANIFEST_CLI_NO_MORE_OPTION = "CLI-No-More-Option"; //$NON-NLS-1$

	/** Field name for CLI option for marking the SRE as embedded in another application..
	 */
	public static final String MANIFEST_CLI_EMBEDDED = "CLI-Embedded"; //$NON-NLS-1$

	/** Attribute name of the SRE's library path (the jar file).
	 */
	public static final String XML_LIBRARY_PATH = "libraryPath"; //$NON-NLS-1$

	/** Attribute name of the SRE's name.
	 */
	public static final String XML_SRE_NAME = "name"; //$NON-NLS-1$

	/** Attribute name of the SRE's main class.
	 */
	public static final String XML_MAIN_CLASS = "mainClass"; //$NON-NLS-1$

	/** Attribute name of the SRE's bootstrap.
	 */
	public static final String XML_BOOTSTRAP = "bootstrap"; //$NON-NLS-1$

	/** Node name of a library used by the SRE.
	 */
	public static final String XML_LIBRARY_LOCATION = "libraryLocation"; //$NON-NLS-1$

	/** Attribute name of system path of the SRE's library.
	 */
	public static final String XML_SYSTEM_LIBRARY_PATH = "systemLibraryPath"; //$NON-NLS-1$

	/** Attribute name of package root path of the SRE's library.
	 */
	public static final String XML_PACKAGE_ROOT_PATH = "packageRootPath"; //$NON-NLS-1$

	/** Attribute name of source path of the SRE's library.
	 */
	public static final String XML_SOURCE_PATH = "sourcePath"; //$NON-NLS-1$

	/** Attribute name of javadoc path of the SRE's library.
	 */
	public static final String XML_JAVADOC_PATH = "javadoc"; //$NON-NLS-1$

	private static final String SERVICE_PATH = "META-INF/services/"; //$NON-NLS-1$

	/** Path of the file that contains the SRE bootstrap definition.
	 */
	public static final String SERVICE_SRE_BOOTSTRAP = SERVICE_PATH + "io.sarl.bootstrap.SREBootstrap"; //$NON-NLS-1$

	private SREConstants() {
		//
	}

}
