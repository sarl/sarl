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

package io.sarl.eclipse.examples;

import java.util.ArrayList;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * Utility functions for the plugin.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.examples 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.examples
 * @since 0.10
 */
public class SARLExamplePlugin extends AbstractUIPlugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.eclipse.examples"; //$NON-NLS-1$

	private static SARLExamplePlugin instance;

	/** Construct an Eclipse plugin for SARL.
	 */
	public SARLExamplePlugin() {
		setDefault(this);
	}

	/** Set the default instance of the plugin.
	 *
	 * @param defaultInstance the default plugin instance.
	 */
	public static void setDefault(SARLExamplePlugin defaultInstance) {
		instance = defaultInstance;
	}

	/** Replies the instance of the plugin.
	 *
	 * @return the default plugin instance.
	 */
	public static SARLExamplePlugin getDefault() {
		return instance;
	}

	/**
	 * Logs an internal error with the specified throwable.
	 *
	 * @param exception the exception to be logged
	 * @return the status.
	 * @see #openError(Shell, String, String, Throwable)
	 */
	public IStatus log(Throwable exception) {
		final var st = createStatus(exception);
		getLog().log(st);
		return st;
	}

	/**
	 * Display an error dialog and log the error.
	 *
	 * @param shell the parent container.
	 * @param title the title of the dialog box.
	 * @param message the message to display into the dialog box.
	 * @param reason the reason to display into the dialog box.
	 * @param exception the exception to be logged.
	 * @since 0.12
	 * @see #log(Throwable)
	 */
	public void openError(Shell shell, String title, String message, String reason, Throwable exception) {
		final var ex = (exception != null) ? Throwables.getRootCause(exception) : null;
		if (ex != null) {
			log(ex);
			final var rmessage = Strings.isNullOrEmpty(reason) ? message : reason;
			final var status = createStatus(IStatus.ERROR, 0, rmessage, ex);
			ErrorDialog.openError(shell, title, message, status);
		} else {
			MessageDialog.openError(shell, title, message);
		}
	}

	/**
	 * Create a status for the given exception.
	 *
	 * @param exception the exception to be logged
	 * @return the status.
	 */
	public static IStatus createStatus(Throwable exception) {
		if (exception instanceof CoreException) {
			return new Status(IStatus.ERROR, PLUGIN_ID,
					exception.getMessage(),
					exception.getCause());
		}
		if (exception != null) {
			return new Status(IStatus.ERROR, PLUGIN_ID,
					exception.getMessage(), exception);
		}
		return new Status(IStatus.ERROR, PLUGIN_ID,
				"Internal Error", exception); //$NON-NLS-1$
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
		if (Strings.isNullOrEmpty(msg)) {
			msg = cause.getLocalizedMessage();
			if (Strings.isNullOrEmpty(msg)) {
				msg = cause.getMessage();
			}
			if (Strings.isNullOrEmpty(msg)) {
				msg = cause.getClass().getSimpleName();
			}
		}
		if (cause != null) {
			final var childStatuses = new ArrayList<IStatus>();
			final var stackTraces = cause.getStackTrace();
			for (final var stackTrace: stackTraces) {
				final var status = createStatus(severity, code, stackTrace.toString(), null);
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

}
