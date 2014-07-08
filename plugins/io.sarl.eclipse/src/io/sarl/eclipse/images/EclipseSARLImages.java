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
package io.sarl.eclipse.images;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * Provider of images for the Eclipse plugin dedicated to SARL.
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class EclipseSARLImages {

	/** Filename of the image that may be used at
	 * the top of the wiazrd dialog when creating new SARL project.
	 */
	public static final String NEW_PROJECT_WIZARD_DIALOG_IMAGE = "icons/sarl_new_project_dialog.png"; //$NON-NLS-1$

	/** Filename of the image that may be used at
	 * the top of the wiazrd dialog when creating new SARL file.
	 */
	public static final String NEW_FILE_WIZARD_DIALOG_IMAGE = "icons/sarl_new_file_dialog.png"; //$NON-NLS-1$

	private static final String PLUGIN_ID = "io.sarl.eclipse"; //$NON-NLS-1$

	private EclipseSARLImages() {
		//
	}

	/** Replies the image stored in the current Eclipse plugin.
	 *
	 * @param imagePath - path of the image.
	 * @return the image.
	 */
    public static Image getImage(String imagePath) {
        return getImageDescriptor(imagePath).createImage();
    }

	/** Replies the descriptor of the image stored in the current Eclipse plugin.
	 *
	 * @param imagePath - path of the image.
	 * @return the image descriptor.
	 */
    public static ImageDescriptor getImageDescriptor(String imagePath) {
        return AbstractUIPlugin.imageDescriptorFromPlugin(EclipseSARLImages.PLUGIN_ID, imagePath);
    }

}
