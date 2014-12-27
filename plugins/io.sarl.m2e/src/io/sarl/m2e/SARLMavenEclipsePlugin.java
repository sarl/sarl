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
package io.sarl.m2e;

import java.text.MessageFormat;

import org.apache.maven.artifact.ArtifactUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.Version;

import com.google.common.base.Strings;


/**
 * Plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLMavenEclipsePlugin extends Plugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.m2e"; //$NON-NLS-1$

	private static SARLMavenEclipsePlugin instance;

	/**
	 */
	public SARLMavenEclipsePlugin() {
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

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static SARLMavenEclipsePlugin getDefault() {
		return instance;
	}

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance - the default plugin instance.
	 */
	public static void setDefault(SARLMavenEclipsePlugin defaultInstance) {
		instance = defaultInstance;
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
		return new Status(severity, PLUGIN_ID, m, cause);
	}

	/** Create a status.
	 *
	 * @param severity - the severity level, see {@link IStatus}.
	 * @param message - the status message.
	 * @return the status.
	 */
	public static IStatus createStatus(int severity, String message) {
		return new Status(severity, PLUGIN_ID, message);
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
					MessageFormat.format(Messages.SARLMavenEclipsePlugin_0, e.getMessage()), e));
		}
	}

	/**
	 * Logs the specified status with this plug-in's log.
	 *
	 * @param status status to log
	 */
	public static void log(IStatus status) {
		getDefault().getILog().log(status);
	}

	/** Maven version parser.
	 *
	 * @param version - the version string.
	 * @return the version.
	 */
	public static Version parseMavenVersion(String version) {
		boolean isSnapshot = ArtifactUtils.isSnapshot(version);
		String[] parts = version.split("[.]"); //$NON-NLS-1$
		int minor = 0;
		int micro = 0;
		int major = Integer.parseInt(parts[0]);
		if (parts.length > 1) {
			minor = Integer.parseInt(parts[1]);
			if (parts.length > 1) {
				if (isSnapshot) {
					parts[2] = parts[2].replaceFirst("\\-.+$", ""); //$NON-NLS-1$//$NON-NLS-2$
				}
				micro = Integer.parseInt(parts[2]);
			}
		}
		if (isSnapshot) {
			return new Version(major, minor, micro, "qualifier"); //$NON-NLS-1$
		}
		return new Version(major, minor, micro);
	}

}
