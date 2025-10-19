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

package io.sarl.eclipse.notifications;

import java.util.Collections;
import java.util.List;

import org.eclipse.core.databinding.observable.list.IListChangeListener;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.runtime.preferences.InstanceScope;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.UIUtils;

/**
 * A container of SARL IDE notifications that serves as intermediary
 * between the notification generators and the notifications listeners.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public class Notifications {

	private static IObservableList<Notification> notifications = new WritableList<>();

	private Notifications() {
		//
	}

	/** Add listener on the changes in the notification list.
	 *
	 * @param listener the listener to add.
	 */
	public static void addChangeListener(IListChangeListener<Notification> listener) {
		notifications.addListChangeListener(listener);
	}

	/** Remove listener on the changes in the notification list.
	 *
	 * @param listener the listener to remove.
	 */
	public static void removeChangeListener(IListChangeListener<Notification> listener) {
		notifications.removeListChangeListener(listener);
	}

	/** Replies the notifications and keep the notifications in this container.
	 * After calling this function, this container still contains all the notifications.
	 *
	 * @return the unmodifiable list of notifications.
	 * @see #consumeNotifications()
	 */
	public static List<Notification> getNotifications() {
		return Collections.unmodifiableList(notifications);
	}

	/** Add a notification.
	 *
	 * @param notification the notification to be added. never, {@code null}.
	 */
	public static void addNotification(Notification notification) {
		assert notification != null;
		UIUtils.syncExec(() -> notifications.add(notification));
	}

	/** Replies if the notification with the given identifier must be ignored.
	 *
	 * @param id the identifier to test.
	 * @return {@code true} if the notification must be ignored.
	 */
	public static boolean isIgnoredNotification(String id) {
		try {
			final var prefs = InstanceScope.INSTANCE.getNode(Notifications.class.getName());
			if (prefs != null) {
				return prefs.getBoolean(id + ".IGNORE", false); //$NON-NLS-1$
			}
		} catch (Throwable ex) {
			SARLEclipsePlugin.getDefault().logWarningMessage(ex);
		}
		return false;
	}

	/** Replies if the notification with the given identifier must be ignored.
	 *
	 * @param id the identifier to test.
	 * @param ignore {@code true} if the notification must be ignored; otherwise {@false} if the notification should be show up.
	 */
	public static void setIgnoredNotification(String id, boolean ignore) {
		try {
			final var prefs = InstanceScope.INSTANCE.getNode(Notifications.class.getName());
			if (prefs != null) {
				prefs.putBoolean(id + ".IGNORE", ignore); //$NON-NLS-1$
				prefs.flush();
				prefs.sync();
			}
		} catch (Throwable ex) {
			SARLEclipsePlugin.getDefault().logWarningMessage(ex);
		}
	}

}
