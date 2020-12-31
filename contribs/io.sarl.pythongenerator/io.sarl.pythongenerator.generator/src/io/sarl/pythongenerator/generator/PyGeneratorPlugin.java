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

package io.sarl.pythongenerator.generator;

import com.google.common.base.Strings;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;

/**
 * Utility functions for the plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class PyGeneratorPlugin extends Plugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.pythongenerator.generator"; //$NON-NLS-1$

	/** Identifier of the preference container.
	 */
	public static final String PREFERENCE_ID = PLUGIN_ID;

	private static PyGeneratorPlugin instance;

	/** Construct an Eclipse plugin for SARL.
	 */
	public PyGeneratorPlugin() {
		setDefault(this);
	}

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance the default plugin instance.
	 */
	public static void setDefault(PyGeneratorPlugin defaultInstance) {
		instance = defaultInstance;
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static PyGeneratorPlugin getDefault() {
		return instance;
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
