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

package io.sarl.eclipsedsl4sarl;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * Utility functions for the plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DSL4SARLEclipsePlugin extends AbstractUIPlugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.eclipse-dsl4sarl"; //$NON-NLS-1$

	private static DSL4SARLEclipsePlugin instance;

	/** Construct an Eclipse plugin for SARL.
	 */
	public DSL4SARLEclipsePlugin() {
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

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance the default plugin instance.
	 */
	public static void setDefault(DSL4SARLEclipsePlugin defaultInstance) {
		instance = defaultInstance;
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static DSL4SARLEclipsePlugin getDefault() {
		return instance;
	}

	/**
	 * Logs an internal error with the specified throwable.
	 *
	 * @param exception the exception to be logged
	 * @see #openError(Shell, String, String, Throwable)
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

}
