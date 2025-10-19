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

package io.sarl.eclipse.notifications.sarlupdate;

import java.util.Objects;

import io.sarl.eclipse.notifications.AbstractNotification;
import io.sarl.eclipse.notifications.NotificationDetailMap;

/**
 * Notification for an available update of the SARL environment. 
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public class SARLUpdateNotification extends AbstractNotification {

	/** The identifier of the notification.
	 */
	public static final String ID = SARLUpdateNotification.class.getName();
	
	private final NotificationDetailMap details;

	/** Constructor.
	 *
	 * @param source the source of the notification.
	 * @param details the details for the available update.
	 */
	public SARLUpdateNotification(String source, NotificationDetailMap details) {
		super(source);
		this.details = details;
	}

	@Override
	public String getID() {
		return ID;
	}
	
	@Override
	public NotificationDetailMap getDetails() {
		return this.details;
	}

	@Override
	public String toString() {
		return getSource() + " -> " + this.details; //$NON-NLS-1$
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		return ((obj instanceof SARLUpdateNotification notif)
				&& super.equals(obj)
				&& ((this.details == null && notif.details == null)
					|| (this.details.equals(notif.details))));
	}

	@Override
	public int hashCode() {
		return Objects.hash(getSource(), this.details);
	}

}
