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

package io.sarl.eclipse.runtime;

/**
 * Constants that are representing elements in the manifest for the SRE preferences.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SREManifestPreferenceConstants {

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

	/** Field name for the VM arguments.
	 */
	public static final String MANIFEST_VM_ARGUMENTS = "VM-Arguments"; //$NON-NLS-1$

	/** Field name for the specific program arguments.
	 */
	public static final String MANIFEST_PROGRAM_ARGUMENTS = "Program-Arguments"; //$NON-NLS-1$

	private static final String SERVICE_PATH = "META-INF/services/"; //$NON-NLS-1$

	/** Path of the file that contains the SRE bootstrap definition.
	 */
	public static final String SERVICE_SRE_BOOTSTRAP = SERVICE_PATH + "io.sarl.bootstrap.SREBootstrap"; //$NON-NLS-1$

	private SREManifestPreferenceConstants() {
		//
	}

}
