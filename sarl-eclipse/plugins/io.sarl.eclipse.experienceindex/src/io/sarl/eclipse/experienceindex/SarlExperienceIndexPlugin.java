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

package io.sarl.eclipse.experienceindex;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ResourceLocator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * Plugin.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse.experienceindex 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.experienceindex
 * @since 0.6
 */
public class SarlExperienceIndexPlugin extends AbstractUIPlugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.eclipse.experienceindex"; //$NON-NLS-1$

	private static SarlExperienceIndexPlugin instance;

	/** Construct an Eclipse plugin.
	 */
	public SarlExperienceIndexPlugin() {
		setDefault(this);
	}

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance the default plugin instance.
	 */
	public static void setDefault(SarlExperienceIndexPlugin defaultInstance) {
		instance = defaultInstance;
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static SarlExperienceIndexPlugin getDefault() {
		return instance;
	}

	/** Replies the image descriptor for the given image path.
	 *
	 * @param imagePath path of the image.
	 * @return the image descriptor.
	 */
	public ImageDescriptor getImageDescriptor(String imagePath) {
		var descriptor = getImageRegistry().getDescriptor(imagePath);
		if (descriptor == null) {
			var odescriptor = ResourceLocator.imageDescriptorFromBundle(PLUGIN_ID, imagePath);
			if (odescriptor.isPresent()) {
				descriptor = odescriptor.get();
				getImageRegistry().put(imagePath, descriptor);
			}
		}
		return descriptor;
	}

	/** Replies the image for the given image path.
	 *
	 * @param imagePath path of the image.
	 * @return the image.
	 */
	public Image getImage(String imagePath) {
		final var descriptor = getImageDescriptor(imagePath);
		return descriptor != null ? descriptor.createImage() : null;
	}

}
