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

import java.util.Arrays;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.osgi.internal.debug.Debug;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.service.prefs.BackingStoreException;


/**
 * Utility functions for the plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLEclipsePlugin extends AbstractUIPlugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.eclipse"; //$NON-NLS-1$

	private static SARLEclipsePlugin instance;

	/** Construct an Eclipse plugin for SARL.
	 */
	public SARLEclipsePlugin() {
		setDefault(this);
	}

	/** Replies the logger.
	 *
	 * <p>Thus function is a non-final version of {@link #getLog()}.
	 *
	 * @return the logger.
	 */
	public ILog getILog() {
		return getLog();
	}

	/**
	 * Returns a section in the SARL Eclipse plugin's dialog settings.
	 * If the section doesn't exist yet, it is created.
	 *
	 * @param name - the name of the section
	 * @return the section of the given name
	 */
	public IDialogSettings getDialogSettingsSection(String name) {
		IDialogSettings dialogSettings = getDialogSettings();
		IDialogSettings section = dialogSettings.getSection(name);
		if (section == null) {
			section = dialogSettings.addNewSection(name);
		}
		return section;
	}

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance - the default plugin instance.
	 */
	public static void setDefault(SARLEclipsePlugin defaultInstance) {
		instance = defaultInstance;
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static SARLEclipsePlugin getDefault() {
		return instance;
	}

	/** Replies the image stored in the current Eclipse plugin.
	 *
	 * @param imagePath - path of the image.
	 * @return the image.
	 */
	public Image getImage(String imagePath) {
		ImageDescriptor descriptor = getImageDescriptor(imagePath);
		if (descriptor == null) {
			return null;
		}
		return descriptor.createImage();
	}

	/** Replies the image descriptor for the given image path.
	 *
	 * @param imagePath - path of the image.
	 * @return the image descriptor.
	 */
	public ImageDescriptor getImageDescriptor(String imagePath) {
		ImageDescriptor descriptor = getImageRegistry().getDescriptor(imagePath);
		if (descriptor == null) {
			descriptor = AbstractUIPlugin.imageDescriptorFromPlugin(SARLEclipsePlugin.PLUGIN_ID, imagePath);
		}
		return descriptor;
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param message - the message associated to the status.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createStatus(int severity, String message, Throwable cause) {
		return new Status(severity, PLUGIN_ID, message, cause);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param code - the code of the error.
	 * @param message - the message associated to the status.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createStatus(int severity, int code, String message, Throwable cause) {
		return new Status(severity, PLUGIN_ID, code, message, cause);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	public IStatus createStatus(int severity, Throwable cause) {
		String message = cause.getLocalizedMessage();
		if (Strings.isNullOrEmpty(message)) {
			message = cause.getMessage();
		}
		if (Strings.isNullOrEmpty(message)) {
			message = cause.getClass().getSimpleName();
		}
		return createStatus(severity, message, cause);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param code - the code of the error.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	public IStatus createStatus(int severity, int code, Throwable cause) {
		String message = cause.getLocalizedMessage();
		if (Strings.isNullOrEmpty(message)) {
			message = cause.getMessage();
		}
		if (Strings.isNullOrEmpty(message)) {
			message = cause.getClass().getSimpleName();
		}
		return createStatus(severity, code, message, cause);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param message - the message associated to the status.
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createStatus(int severity, String message) {
		return new Status(severity, PLUGIN_ID, message);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param code - the code of the error.
	 * @param message - the message associated to the status.
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createStatus(int severity, int code, String message) {
		return new Status(severity, PLUGIN_ID, code, message, null);
	}

	/** Create a ok status.
	 *
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createOkStatus() {
		return Status.OK_STATUS;
	}

	/** Create a multistatus.
	 *
	 * @param status - the status to put in the same status instance.
	 * @return the status.
	 */
	public IStatus createMultiStatus(IStatus... status) {
		return createMultiStatus(Arrays.asList(status));
	}

	/** Create a multistatus.
	 *
	 * @param status - the status to put in the same status instance.
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createMultiStatus(Iterable<? extends IStatus> status) {
		IStatus max = findMax(status);
		MultiStatus multiStatus;
		if (max == null) {
			multiStatus = new MultiStatus(PLUGIN_ID, 0, null, null);
		} else {
			multiStatus = new MultiStatus(PLUGIN_ID, 0, max.getMessage(), max.getException());
		}
		for (IStatus s : status) {
			multiStatus.add(s);
		}
		return multiStatus;
	}

	private static IStatus findMax(Iterable<? extends IStatus> status) {
		IStatus max = null;
		for (IStatus s : status) {
			if (max == null || max.getSeverity() > s.getSeverity()) {
				max = s;
			}
		}
		return max;
	}

	/**
	 * Logs an internal error with the specified message.
	 *
	 * @param message the error message to log
	 */
	public void logErrorMessage(String message) {
		getILog().log(new Status(IStatus.ERROR, PLUGIN_ID, message, null));
	}

	/**
	 * Logs an internal debug message with the specified message.
	 *
	 * @param message the debug message to log
	 */
	@SuppressWarnings("static-method")
	public void logDebugMessage(String message) {
		Debug.println(message);
	}

	/**
	 * Logs an internal debug message with the specified message.
	 *
	 * @param cause - the cause of the message log.
	 */
	@SuppressWarnings({"static-method", "checkstyle:regexp"})
	public void logDebugMessage(Throwable cause) {
		Debug.printStackTrace(cause);
	}

	/**
	 * Logs an internal debug message with the specified message.
	 *
	 * @param message - the debug message to log
	 * @param cause - the cause of the message log.
	 */
	@SuppressWarnings({"static-method", "checkstyle:regexp"})
	public void logDebugMessage(String message, Throwable cause) {
		Debug.println(message);
		if (cause != null) {
			Debug.printStackTrace(cause);
		}
	}

	/**
	 * Logs an internal error with the specified throwable.
	 *
	 * @param exception the exception to be logged
	 */
	public void log(Throwable exception) {
		if (exception instanceof CoreException) {
			getILog().log(new Status(IStatus.ERROR, PLUGIN_ID,
					exception.getMessage(),
					exception.getCause()));
		} else if (exception != null) {
			getILog().log(new Status(IStatus.ERROR, PLUGIN_ID,
					exception.getMessage(), exception));
		} else {
			getILog().log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Internal Error", exception));   //$NON-NLS-1$
		}
	}

	/** Replies the Eclipse preferences for this plugin.
	 *
	 * @return the Eclipse preferences, never <code>null</code>.
	 */
	@SuppressWarnings("static-method")
	public IEclipsePreferences getPreferences() {
		return InstanceScope.INSTANCE.getNode(PLUGIN_ID);
	}

	/**
	 * Saves the preferences for the plug-in.
	 */
	public void savePreferences() {
		IEclipsePreferences prefs = getPreferences();
		try {
			prefs.flush();
		} catch (BackingStoreException e) {
			getILog().log(createStatus(IStatus.ERROR, e));
		}
	}

}
