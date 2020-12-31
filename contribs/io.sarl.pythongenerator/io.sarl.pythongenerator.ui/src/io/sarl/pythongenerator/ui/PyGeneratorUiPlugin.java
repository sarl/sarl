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

package io.sarl.pythongenerator.ui;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ResourceLocator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * Utility functions for the plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class PyGeneratorUiPlugin extends AbstractUIPlugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.pythongenerator.ui"; //$NON-NLS-1$

	private static PyGeneratorUiPlugin instance;

	/** Construct an Eclipse plugin for SARL.
	 */
	public PyGeneratorUiPlugin() {
		setDefault(this);
	}

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance the default plugin instance.
	 */
	public static void setDefault(PyGeneratorUiPlugin defaultInstance) {
		instance = defaultInstance;
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static PyGeneratorUiPlugin getDefault() {
		return instance;
	}

	/**
	 * Returns a section in the SARL Eclipse plugin's dialog settings.
	 * If the section doesn't exist yet, it is created.
	 *
	 * @param name the name of the section
	 * @return the section of the given name
	 */
	public IDialogSettings getDialogSettingsSection(String name) {
		final IDialogSettings dialogSettings = getDialogSettings();
		IDialogSettings section = dialogSettings.getSection(name);
		if (section == null) {
			section = dialogSettings.addNewSection(name);
		}
		return section;
	}

	/** Replies the image stored in the current Eclipse plugin.
	 *
	 * @param imagePath path of the image.
	 * @return the image.
	 */
	public Image getImage(String imagePath) {
		final ImageDescriptor descriptor = getImageDescriptor(imagePath);
		if (descriptor == null) {
			return null;
		}
		return descriptor.createImage();
	}

	/** Replies the image descriptor for the given image path.
	 *
	 * @param imagePath path of the image.
	 * @return the image descriptor.
	 */
	public ImageDescriptor getImageDescriptor(String imagePath) {
		ImageDescriptor descriptor = getImageRegistry().getDescriptor(imagePath);
		if (descriptor == null) {
			descriptor = ResourceLocator.imageDescriptorFromBundle(PLUGIN_ID, imagePath).orElse(null);
			if (descriptor != null) {
				getImageRegistry().put(imagePath, descriptor);
			}
		}
		return descriptor;
	}

	/** Create a status.
	 *
	 * @param severity the severity level, see {@link IStatus}.
	 * @param cause the cause of the problem.
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createStatus(int severity, Throwable cause) {
		String message = cause.getLocalizedMessage();
		if (Strings.isNullOrEmpty(message)) {
			message = cause.getMessage();
		}
		if (Strings.isNullOrEmpty(message)) {
			message = cause.getClass().getSimpleName();
		}
		return new Status(severity, PLUGIN_ID, message, cause);
	}

}
