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
package io.sarl.eclipse.util;

import org.eclipse.core.internal.runtime.InternalPlatform;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.osgi.internal.debug.Debug;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;
import org.osgi.service.prefs.BackingStoreException;


/**
 * Utility functions for the plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class PluginUtil {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.eclipse"; //$NON-NLS-1$

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
	public static final String SARL_LOGO_IMAGE = "icons/sarl_logo_16.png"; //$NON-NLS-1$

	private PluginUtil() {
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
		return AbstractUIPlugin.imageDescriptorFromPlugin(PluginUtil.PLUGIN_ID, imagePath);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param message - the message associated to the status.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	public static IStatus createStatus(int severity, String message, Throwable cause) {
		return new Status(severity, PLUGIN_ID, message, cause);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	public static IStatus createStatus(int severity, Throwable cause) {
		return createStatus(severity, cause.getLocalizedMessage(), cause);
	}

	/** Create a ok status.
	 *
	 * @return the status.
	 */
	public static IStatus createOkStatus() {
		return Status.OK_STATUS;
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param message - the message associated to the status.
	 * @return the status.
	 */
	public static IStatus createStatus(int severity, String message) {
		return new Status(severity, PLUGIN_ID, message);
	}

	/** Replies the bundle of the plugin.
	 *
	 * @return the bundle.
	 */
	public static Bundle getBundle() {
		InternalPlatform platform = InternalPlatform.getDefault();
		return platform.getBundle(PLUGIN_ID);
	}

	/** Returns the read/write location in which the given bundle can manage private state.
	 *
	 * @return the state location.
	 */
	public static IPath getStateLocation() {
		InternalPlatform platform = InternalPlatform.getDefault();
		return platform.getStateLocation(getBundle());
	}

	/** Replies the logger of the plugin.
	 *
	 * @return the logger.
	 */
	public static ILog getLog() {
		InternalPlatform platform = InternalPlatform.getDefault();
		return platform.getLog(getBundle());
	}

	/**
	 * Logs the specified status with this plug-in's log.
	 *
	 * @param status status to log
	 */
	public static void log(IStatus status) {
		getLog().log(status);
	}

	/**
	 * Logs an internal error with the specified message.
	 *
	 * @param message the error message to log
	 */
	public static void logErrorMessage(String message) {
		log(new Status(IStatus.ERROR, PLUGIN_ID, message, null));
	}

	/**
	 * Logs an internal debug message with the specified message.
	 *
	 * @param message the debug message to log
	 */
	public static void logDebugMessage(String message) {
		Debug.println(message);
	}

	/**
	 * Logs an internal debug message with the specified message.
	 *
	 * @param message - the debug message to log
	 * @param cause - the cause of the message log.
	 */
	public static void logDebugMessage(String message, Throwable cause) {
		Debug.println(message);
		if (cause != null) {
			//CHECKSTYLE:OFF
			Debug.printStackTrace(cause);
			//CHECKSTYLE:ON
		}
	}

	/**
	 * Logs an internal debug message with the specified message.
	 *
	 * @param cause - the cause of the message log.
	 */
	public static void logDebugMessage(Throwable cause) {
		//CHECKSTYLE:OFF
		Debug.printStackTrace(cause);
		//CHECKSTYLE:ON
	}

	/**
	 * Logs an internal error with the specified throwable.
	 *
	 * @param e the exception to be logged
	 */
	public static void log(Throwable e) {
		if (e instanceof CoreException) {
			log(new Status(IStatus.ERROR, PLUGIN_ID,
					e.getMessage(), e.getCause()));
		} else {
			log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Internal Error", e));   //$NON-NLS-1$
		}
	}

	/** Replies the Eclipse preferences for this plugin.
	 *
	 * @return the Eclipse preferences, never <code>null</code>.
	 */
	public static IEclipsePreferences getPreferences() {
		return InstanceScope.INSTANCE.getNode(PLUGIN_ID);
	}

	/**
	 * Saves the preferences for the plug-in.
	 */
	public static void savePreferences() {
		IEclipsePreferences prefs = getPreferences();
		try {
			prefs.flush();
		} catch (BackingStoreException e) {
			log(createStatus(IStatus.ERROR, e));
		}
	}

	/** Null-safe equality test.
	 *
	 * @param a - the first object.
	 * @param b - the second object.
	 * @return <code>true</code> if a is equal to b.
	 */
	public static boolean equals(Object a, Object b) {
		return (a == b)
			|| (a == null && b == null)
			|| (a != null && a.equals(b));
	}

	/** Null-safe equality test between strings.
	 * Empty string is assumed to be equal to <code>null</code> value.
	 *
	 * @param a - the first object.
	 * @param b - the second object.
	 * @return <code>true</code> if a is equal to b.
	 */
	public static boolean equalsString(String a, String b) {
		if (a == b) {
			return true;
		}
		if (a == null) {
			return (b == null || b.isEmpty());
		}
		if (b == null) {
			return a.isEmpty();
		}
		return a.equals(b);
	}

	/** Null-safe comparison.
	 *
	 * @param <T> - type of the comparable element.
	 * @param a - the first object.
	 * @param b - the second object.
	 * @return Negative number if a lower than b.
	 * Positive number if a greater than b.
	 * <code>0</code> if a is equal to b.
	 */
	public static <T> int compareTo(Comparable<T> a, T b) {
		if (a == b) {
			return 0;
		}
		if (a == null) {
			return Integer.MIN_VALUE;
		}
		if (b == null) {
			return Integer.MAX_VALUE;
		}
		assert (a != null && b != null);
		return a.compareTo(b);
	}

	/** Null-safe version parser.
	 *
	 * @param version - the version string.
	 * @return the version.
	 */
	public static Version parseVersion(String version) {
		if (version != null && !version.isEmpty()) {
			try {
				return Version.parseVersion(version);
			} catch (Throwable _) {
				//
			}
		}
		return null;
	}

	/** Null-safe compare a version number to a range of version numbers.
	 *
	 * @param version - the version to compare to the range.
	 * @param minVersion - the minimal version in the range (inclusive).
	 * @param maxVersion - the maximal version in the range (exclusive).
	 * @return A negative number if the version in lower than the minVersion.
	 * A positive number if the version is greater than or equal to the maxVersion.
	 * <code>0</code> if the version is between minVersion and maxVersion.
	 */
	public static int compareVersionToRange(Version version, Version minVersion, Version maxVersion) {
		if (version == null) {
			return Integer.MIN_VALUE;
		}
		if (minVersion != null && version.compareTo(minVersion) < 0) {
			return -1;
		}
		if (maxVersion != null && version.compareTo(maxVersion) >= 0) {
			return 1;
		}
		return 0;
	}

}
