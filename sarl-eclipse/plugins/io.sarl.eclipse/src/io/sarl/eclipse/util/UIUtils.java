/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;

import io.sarl.eclipse.SARLEclipsePlugin;

/** Utilities for UI.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public final class UIUtils {

	private UIUtils() {
		//
	}

	/** Replies the current display.
	 *
	 * @return the display, never {@code null}.
	 */
	public static Display getDisplay() {
		var display = Display.getCurrent();
		if (display == null) {
			try {
				display = PlatformUI.getWorkbench().getDisplay();
			} catch (Throwable ignore) {
				//
			}
		}
		if (display == null) {
			display = Display.getDefault();
		}
		if (display == null) {
			display = new Display();
		}
		return display;
	}

	/** Run the given {@code task} asynchronously on the UI threads.
	 *
	 * @param task the task to be run.
	 */
	public static void asyncExec(Runnable task) {
		assert task != null;
		final var display = getDisplay();
		if (display != null) {
			asyncExec(display, task);
		}
	}

	/** Run the given {@code task} asynchronously on the UI thread attached to the given UI component.
	 *
	 * @param control the UI component.
	 * @param task the task to be run.
	 */
	public static void asyncExec(Control control, Runnable task) {
		assert control != null;
		assert task != null;
		try {
			if (control.isDisposed()) {
				return;
			}
			control.getDisplay().asyncExec(() -> {
				if (!control.isDisposed()) {
					task.run();
				}
			});
		} catch (SWTException ex) {
			if (ex.code != SWT.ERROR_WIDGET_DISPOSED) {
				throw ex;
			}
		}
	}

	/** Run the given {@code task} asynchronously on the given UI thread.
	 *
	 * @param display the UI display.
	 * @param task the task to be run.
	 */
	public static void asyncExec(Display display, Runnable task) {
		assert display != null;
		assert task != null;
		try {
			if (display.isDisposed()) {
				return;
			}
			display.asyncExec(() ->  {
				if (display.isDisposed()) {
					return;
				}
				try {
					task.run();
				} catch (SWTException ex) {
					if (ex.code != SWT.ERROR_WIDGET_DISPOSED) {
						throw ex;
					}
				}
			});
		} catch (SWTException ex) {
			if (ex.code != SWT.ERROR_WIDGET_DISPOSED) {
				throw ex;
			}
		}
	}

	/** Run the given {@code task} synchronously on the current UI thread.
	 *
	 * @param task the task to be run.
	 */
	public static void syncExec(Runnable task) {
		assert task != null;
		final var display = getDisplay();
		if (Display.getCurrent() == display || display == null) {
			task.run();
		} else {
			syncExec(display, task);
		}
	}

	/** Run the given {@code task} asynchronously on the given UI thread.
	 *
	 * @param control the UI component.
	 * @param task the task to be run.
	 */
	public static void syncExec(Control control, Runnable task) {
		assert control != null;
		assert task != null;
		try {
			if (control.isDisposed()) {
				return;
			}
			control.getDisplay().syncExec(() ->  {
				if (!control.isDisposed()) {
					task.run();
				}
			});
		} catch (SWTException ex) {
			if (ex.code != SWT.ERROR_WIDGET_DISPOSED) {
				throw ex;
			}
		}
	}

	/** Run the given {@code task} asynchronously on the given UI thread.
	 *
	 * @param display the UI display.
	 * @param task the task to be run.
	 */
	public static void syncExec(Display display, Runnable task) {
		assert display != null;
		assert task != null;
		try {
			if (display.isDisposed()) {
				return;
			}
			display.syncExec(() -> {
				if (display.isDisposed()) {
					return;
				}
				try {
					task.run();
				} catch (SWTException ex) {
					if (ex.code != SWT.ERROR_WIDGET_DISPOSED) {
						throw ex;
					}
				}
			});
		} catch (SWTException ex) {
			if (ex.code != SWT.ERROR_WIDGET_DISPOSED) {
				throw ex;
			}
		}
	}

	/** Start the command or handler with the given identifier.
	 *
	 * @param handlerId the identifier of the handler to start.
	 */
	public static void startCommand(String handlerId) {
		asyncExec(() -> {
	        final var commandService = PlatformUI.getWorkbench().getService(ICommandService.class);
	        final var handlerService = PlatformUI.getWorkbench().getService(IHandlerService.class);
	        final var command = commandService.getCommand(handlerId);
	        if (command != null) {
	          if (command.isEnabled()) {
	            try {
	              command.executeWithChecks(handlerService.createExecutionEvent(command, null));
	            } catch (Exception ex) {
	              SARLEclipsePlugin.getDefault().logWarningMessage(ex);
	            }
	          }
	        }
		});
	}

}
