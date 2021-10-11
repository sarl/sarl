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

package io.sarl.lang.core;

import io.sarl.lang.annotation.SarlElementType;
import io.sarl.lang.annotation.SarlSpecification;

/**
 * Wraps an event that was posted, but which had no subscribers and thus could not be delivered.
 *
 * <p>
 * Registering a DeadEvent {@code BehaviorGuardEvaluator} is useful for debugging or logging, as it can detect
 * misconfigurations in a system's event distribution.
 *
 * Directly copy from com.google.common.eventbus.DeadEvent
 * </p>
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @deprecated no more used since 0.5.
 */
@Deprecated
@SuppressWarnings("all")
@SarlSpecification("0.4")
@SarlElementType(15)
public final class DeadEvent extends Event {

	private static final long serialVersionUID = 1117818766135181831L;

	private final Object event;

	/**
	 * Creates a new DeadEvent.
	 *
	 * @param event the event that could not be delivered.
	 */
	public DeadEvent(Event event) {
		super(event.getSource());
		assert event != null;
		this.event = event;
	}

	/**
	 * Returns the wrapped, 'dead' event, which the system was unable to deliver to any registered {@code BehaviorGuardEvaluator}.
	 *
	 * @return the 'dead' event that could not be delivered.
	 */
	public Object getEvent() {
		return this.event;
	}

}
