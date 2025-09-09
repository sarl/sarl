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

package io.sarl.apputils.uiextensions;

import java.util.ArrayList;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;

/**
 * UI plugin.
 *
 * @author $Author: sgalland$
 * @version uiextensions 0.15.0 20250909-115749
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid uiextensions
 * @since 0.15
 */
public class UiExtensionsPlugin extends Plugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.apputils.uiextensions"; //$NON-NLS-1$

	private static UiExtensionsPlugin instance;

	/** Construct an Eclipse plugin.
	 */
	public UiExtensionsPlugin() {
		setDefault(this);
	}

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance the default plugin instance.
	 */
	public static void setDefault(UiExtensionsPlugin defaultInstance) {
		instance = defaultInstance;
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static UiExtensionsPlugin getDefault() {
		return instance;
	}

	/** Create a status.
	 *
	 * @param severity the severity level, see {@link IStatus}.
	 * @param code the code of the error.
	 * @param message the message associated to the status.
	 * @param cause the cause of the problem.
	 * @return the status.
	 */
	public IStatus createStatus(int severity, int code, String message, Throwable cause) {
		var msg = message;
		if (msg == null || msg.length() == 0) {
			msg = cause.getLocalizedMessage();
			if (msg == null || msg.length() == 0) {
				msg = cause.getMessage();
			}
			if (msg == null || msg.length() == 0) {
				msg = cause.getClass().getSimpleName();
			}
		}
		if (cause != null) {
			final var childStatuses = new ArrayList<IStatus>();
			final var stackTraces = cause.getStackTrace();
			for (final var stackTrace: stackTraces) {
				final var status = createStatus(severity, stackTrace.toString());
				childStatuses.add(status);
			}

			final var children = new IStatus[childStatuses.size()];
			if (!childStatuses.isEmpty()) {
				childStatuses.toArray(children);
			}
			return new MultiStatus(PLUGIN_ID,
					code,
					children,
					msg, cause);
		}
		return new Status(severity, PLUGIN_ID, code, msg, cause);
	}

	/** Create a status.
	 *
	 * @param severity the severity level, see {@link IStatus}.
	 * @param cause the cause of the problem.
	 * @return the status.
	 */
	public IStatus createStatus(int severity, Throwable cause) {
		return createStatus(severity, 0, null, cause);
	}

	/** Create a status.
	 *
	 * @param severity the severity level, see {@link IStatus}.
	 * @param code the code of the error.
	 * @param cause the cause of the problem.
	 * @return the status.
	 */
	public IStatus createStatus(int severity, int code, Throwable cause) {
		return createStatus(severity, code, null, cause);
	}

	/** Create a status.
	 *
	 * @param severity the severity level, see {@link IStatus}.
	 * @param message the message associated to the status.
	 * @return the status.
	 */
	public IStatus createStatus(int severity, String message) {
		return createStatus(severity, 0, message);
	}

	/** Create a status.
	 *
	 * @param severity the severity level, see {@link IStatus}.
	 * @param code the code of the error.
	 * @param message the message associated to the status.
	 * @return the status.
	 */
	public IStatus createStatus(int severity, int code, String message) {
		return createStatus(severity, code, message, null);
	}

	/** Create a ok status.
	 *
	 * @return the status.
	 */
	@SuppressWarnings("static-method")
	public IStatus createOkStatus() {
		return Status.OK_STATUS;
	}

}
