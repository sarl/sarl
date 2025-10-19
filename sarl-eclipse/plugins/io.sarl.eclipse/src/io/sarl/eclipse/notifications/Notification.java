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

import org.eclipse.xtext.xbase.lib.Procedures.Procedure0;

/** A notification in the SARL IDE that is related to some SARL event.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public interface Notification {

	/**
	 * Returns the identifier of the notification. This identifier may be used in the Eclipse preferences.
	 *
	 * @return the identifier, never {@code null} or empty string.
	 */
	String getID();

	/**
	 * Returns the name of the source of the notification.
	 *
	 * @return a name, never {@code null}.
	 */
	String getSource();

	/**
	 * Replies the type of this notification.
	 *
	 * @return the type, never {@code null}.
	 */
	default NotificationType getType() {
		return NotificationType.UNKNOWN;
	}

	/**
	 * Returns the details of the notification in the form of a dictionary that
	 * is associating names of properties to their values.
	 * The key is of type {@link java.lang.String},
	 * and the value is of type {@link java.lang.Object}
	 *
	 * @return the map of the notification's details.
	 */
	NotificationDetailMap getDetails();

	/** Replies if this notification my be used for the standard notification view, or not.
	 * If this notification is not suitable for the default notification view,
	 *
	 * @return {@code true} if the properties of this notification are suitable for
	 *      the standard notification view.
	 */
	default boolean isStandardViewSupport() {
		return true;
	}

	/** Replies a procedure that enables to show up the notification to the user.
	 * This function must reply {@code null} if {@link #isStandardViewSupport()}
	 * replies {@code true}.
	 *
	 * @return the procedure to show the notification up to the user.
	 */
	default Procedure0 showNotificationUI() {
		return null;
	}

	/** Mark this notification used by the notification system.
	 *
	 * @return the previous value of this flag.
	 */
	boolean markAsUsed();

	/** Replies if the notification was already used by the notification system.
	 *
	 * @return {@code true} if the notification was marked as used.
	 */
	boolean isMarkedAsUsed();

}
