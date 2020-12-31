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

package io.sarl.m2e;

import java.text.MessageFormat;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * Plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLMavenEclipsePlugin extends AbstractUIPlugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.m2e"; //$NON-NLS-1$

	private static SARLMavenEclipsePlugin instance;

	/** Construct the plugin.
	 */
	public SARLMavenEclipsePlugin() {
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

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static SARLMavenEclipsePlugin getDefault() {
		return instance;
	}

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance the default plugin instance.
	 */
	public static void setDefault(SARLMavenEclipsePlugin defaultInstance) {
		instance = defaultInstance;
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

	/** Create a status.
	 *
	 * @param severity the severity level, see {@link IStatus}.
	 * @param message the status message.
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createStatus(int severity, String message) {
		return new Status(severity, PLUGIN_ID, message);
	}

	/**
	 * Logs an internal error with the specified throwable.
	 *
	 * @param exception the exception to be logged
	 */
	public void log(Throwable exception) {
		if (exception instanceof CoreException) {
			getILog().log(new Status(IStatus.ERROR, PLUGIN_ID,
					exception.getMessage(), exception.getCause()));
		} else if (exception != null) {
			getILog().log(new Status(IStatus.ERROR, PLUGIN_ID,
					MessageFormat.format(Messages.SARLMavenEclipsePlugin_0, exception.getMessage()), exception));
		} else {
			getILog().log(new Status(IStatus.ERROR, PLUGIN_ID,
					"Internal Error", exception));   //$NON-NLS-1$
		}
	}

}
