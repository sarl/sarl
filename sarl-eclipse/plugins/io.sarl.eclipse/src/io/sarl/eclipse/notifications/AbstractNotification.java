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

import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

import org.eclipse.xtext.util.Strings;

/**
 * Abstract implementation of a notification. 
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 1.0
 */
public abstract class AbstractNotification implements Notification {

	private final AtomicBoolean used = new AtomicBoolean();

	private final String source;

	/** Constructor.
	 *
	 * @param source the source of the notification.
	 * @param details the details for the available update.
	 */
	public AbstractNotification(String source) {
		this.source = source;
	}
	
	@Override
	public String getSource() {
		return this.source;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		return ((obj instanceof AbstractNotification notif)
				&& Strings.equal(this.source, notif.source));
	}

	@Override
	public int hashCode() {
		return Objects.hash(this.source);
	}

	@Override
	public boolean markAsUsed() {
		return this.used.getAndSet(true);
	}

	@Override
	public boolean isMarkedAsUsed() {
		return this.used.get();
	}

}
