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

package io.sarl.bspl.eclipse;

/**
 * Provides the constants for the BSPL files.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 * @see io.sarl.lang.SARLConfig
 */
public final class BSPLEclipseConfig {

	/**
	 * Default BSPL file extension (without the dot).
	 */
	public static final String BSPL_WO_DOT_FILE_EXTENSION = "bspl"; //$NON-NLS-1$

	/**
	 * Default BSPL file extension (with the dot).
	 */
	public static final String BSPL_FILE_EXTENSION = "." + BSPL_WO_DOT_FILE_EXTENSION; //$NON-NLS-1$

	/**
	 * ID of the project nature defined by XText.
	 */
	public static final String XTEXT_NATURE_ID = "org.eclipse.xtext.ui.shared.xtextNature"; //$NON-NLS-1$

	/**
	 * ID of this nature.
	 */
	public static final String NATURE_ID = BSPLEclipsePlugin.PLUGIN_ID + ".BSPLProjectNature"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wizard dialog when creating new SARL file.
	 */
	public static final String NEW_FILE_WIZARD_DIALOG_IMAGE = "icons/bspl_new_file_dialog.png"; //$NON-NLS-1$

	private BSPLEclipseConfig() {
		//
	}

}
