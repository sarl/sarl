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

package io.sarl.eclipse;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IStartup;

import io.sarl.eclipse.notifications.SARLNotificationHandler;
import io.sarl.eclipse.notifications.sarlupdate.SARLUpdateChecker;

/** Handler for the start-up of the SARL IDE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public class StandardEarlyStartup implements IStartup {

	@Override
	public void earlyStartup() {
		final Display display = Display.getDefault();
		display.asyncExec(() -> doStartup());
	}

	/** Do the startup actions.
	 */
	@SuppressWarnings("static-method")
	protected void doStartup() {
		SARLNotificationHandler.register();
		SARLUpdateChecker.asynchronousCheck();
	}

}
