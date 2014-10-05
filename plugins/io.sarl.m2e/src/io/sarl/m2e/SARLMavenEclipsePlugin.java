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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;

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
		//
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static SARLMavenEclipsePlugin getDefault() {
		return instance;
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

}
