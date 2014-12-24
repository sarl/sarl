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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.osgi.internal.debug.Debug;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Version;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.base.Strings;


/**
 * Utility functions for the plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLEclipsePlugin extends Plugin {

	/** Empty string.
	 */
	public static final String EMPTY_STRING = ""; //$NON-NLS-1$

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.eclipse"; //$NON-NLS-1$

	private static SARLEclipsePlugin instance;

	/**
	 */
	public SARLEclipsePlugin() {
		setDefault(this);
	}
	
	/** Replies the logger.
	 *
	 * Thus function is a non-final version of {@link #getLog()}.
	 * 
	 * @return the logger.
	 */
	public ILog getILog() {
		return getLog();
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
	public static Image getImage(String imagePath) {
		return getImageDescriptor(imagePath).createImage();
	}

	/** Replies the descriptor of the image stored in the current Eclipse plugin.
	 *
	 * @param imagePath - path of the image.
	 * @return the image descriptor.
	 */
	public static ImageDescriptor getImageDescriptor(String imagePath) {
		return AbstractUIPlugin.imageDescriptorFromPlugin(SARLEclipsePlugin.PLUGIN_ID, imagePath);
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
	 * @param code - the code of the error.
	 * @param message - the message associated to the status.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	public static IStatus createStatus(int severity, int code, String message, Throwable cause) {
		return new Status(severity, PLUGIN_ID, code, message, cause);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	public static IStatus createStatus(int severity, Throwable cause) {
		String m = cause.getLocalizedMessage();
		if (Strings.isNullOrEmpty(m)) {
			m = cause.getMessage();
		}
		if (Strings.isNullOrEmpty(m)) {
			m = cause.getClass().getSimpleName();
		}
		return createStatus(severity, m, cause);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param code - the code of the error.
	 * @param cause - the cause of the problem.
	 * @return the status.
	 */
	public static IStatus createStatus(int severity, int code, Throwable cause) {
		String m = cause.getLocalizedMessage();
		if (Strings.isNullOrEmpty(m)) {
			m = cause.getMessage();
		}
		if (Strings.isNullOrEmpty(m)) {
			m = cause.getClass().getSimpleName();
		}
		return createStatus(severity, code, m, cause);
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

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param code - the code of the error.
	 * @param message - the message associated to the status.
	 * @return the status.
	 */
	public static IStatus createStatus(int severity, int code, String message) {
		return new Status(severity, PLUGIN_ID, code, message, null);
	}

	/**
	 * Logs the specified status with this plug-in's log.
	 *
	 * @param status status to log
	 */
	public static void log(IStatus status) {
		getDefault().getILog().log(status);
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
		} else if (e != null) {
			log(new Status(IStatus.ERROR, PLUGIN_ID,
					e.getMessage(), e));
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
		if (!Strings.isNullOrEmpty(version)) {
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
	 * The minVersion must be strictly lower to the maxVersion. Otherwise
	 * the behavior is not predictible.
	 *
	 * @param version - the version to compare to the range; must not be <code>null</code>.
	 * @param minVersion - the minimal version in the range (inclusive); could be <code>null</code>.
	 * @param maxVersion - the maximal version in the range (exclusive); could be <code>null</code>.
	 * @return a negative number if the version in lower than the minVersion.
	 * A positive number if the version is greater than or equal to the maxVersion.
	 * <code>0</code> if the version is between minVersion and maxVersion.
	 */
	public static int compareVersionToRange(Version version, Version minVersion, Version maxVersion) {
		assert (minVersion == null || maxVersion == null || minVersion.compareTo(maxVersion) < 0);
		if (version == null) {
			return Integer.MIN_VALUE;
		}
		if (minVersion != null && compareVersionsNoQualifier(version, minVersion) < 0) {
			return -1;
		}
		if (maxVersion != null && compareVersionsNoQualifier(version, maxVersion) >= 0) {
			return 1;
		}
		return 0;
	}

	private static int compareVersionsNoQualifier(Version a, Version b) {
		if (a == b) {
			return 0;
		}

		int result = a.getMajor() - b.getMajor();
		if (result != 0) {
			return result;
		}

		result = a.getMinor() - b.getMinor();
		if (result != 0) {
			return result;
		}

		return a.getMicro() - b.getMicro();
	}

}
