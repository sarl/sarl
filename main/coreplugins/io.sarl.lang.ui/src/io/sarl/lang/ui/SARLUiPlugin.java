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

package io.sarl.lang.ui;

import com.google.common.base.Throwables;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

import io.sarl.lang.ui.internal.LangActivator;

/**
 * Utility functions for the plugin. The plugin activator is an instance of {@link LangActivator}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 * @see LangActivator
 */
public final class SARLUiPlugin {

	private SARLUiPlugin() {
		//
	}

	/**
	 * Logs an internal error with the specified throwable.
	 *
	 * @param exception the exception to be logged
	 * @see #openError(Shell, String, String, Throwable)
	 */
	public static void log(Throwable exception) {
		final LangActivator activator = LangActivator.getInstance();
		if (exception instanceof CoreException) {
			activator.getLog().log(new Status(IStatus.ERROR, LangActivator.PLUGIN_ID,
					exception.getMessage(),
					exception.getCause()));
		} else if (exception != null) {
			activator.getLog().log(new Status(IStatus.ERROR, LangActivator.PLUGIN_ID,
					exception.getMessage(), exception));
		} else {
			activator.getLog().log(new Status(IStatus.ERROR, LangActivator.PLUGIN_ID,
					"Internal Error", exception));   //$NON-NLS-1$
		}
	}

	/**
	 * Display an error dialog and log the error.
	 *
	 * @param shell the parent container.
	 * @param title the title of the dialog box.
	 * @param message the message to display into the dialog box.
	 * @param reasonMessage the message that explain the reason to display into the dialog box.
	 * @param exception the cause of the error.
	 * @see #log(Throwable)
	 */
	public static void openError(Shell shell, String title, String message, String reasonMessage, Throwable exception) {
		final Throwable ex = (exception != null) ? Throwables.getRootCause(exception) : null;
		if (ex != null) {
			log(ex);
			final IStatus status = new Status(IStatus.ERROR, LangActivator.PLUGIN_ID, reasonMessage, ex);
			ErrorDialog.openError(shell, title, message, status);
		} else {
			MessageDialog.openError(shell, title, message);
		}
	}

}
