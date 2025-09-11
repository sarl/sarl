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

package io.sarl.lang.core;

import java.util.Collection;
import java.util.Set;

/**
 * Describes an object that has the ability to receive SARL events, and consequently to evaluate the
 * guard associated to a given event and returns the list of behaviors' runnable that must be
 * executed according to the result of the guard evaluation.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.12
 */
public interface IBehaviorGuardEvaluatorReceiver {

	/** Evaluate the behavior unit's guards.
	 *
	 * @param eventType the type of event to be considered for evaluating the {@code event}.
	 * @param event the event to evaluate.
	 * @param callbacks the collection of callbacks to run.
	 * @since 0.14
	 */
	default void $evaluateBehaviorGuards(Class<?> eventType, Object event, Collection<Runnable> callbacks) {
		//
	}

	/** Evaluate the behavior unit's guards.
	 *
	 * @param event the event to evaluate.
	 * @param callbacks the collection of callbacks to run.
	 * @see #$evaluateBehaviorGuards(Class, Object, Collection)
	 * @deprecated See {@link #$evaluateBehaviorGuards(Class, Object, Collection)} for replacement.
	 */
	@Deprecated(since = "0.14", forRemoval = true)
	default void $evaluateBehaviorGuards(Object event, Collection<Runnable> callbacks) {
		$evaluateBehaviorGuards(event.getClass(), event, callbacks);
	}

	/** Replies the list of the supported events by the receiver.
	 *
	 * @param toBeFilled the set to fill with the supported events.
	 */
	default void $getSupportedEvents(Set<Class<? extends Event>> toBeFilled) {
		//
	}

	/** Replies if the given type is supported.
	 *
	 * @param event is the type to be tested.
	 * @return {@code true} is the event type is supported.
	 */
	default boolean $isSupportedEvent(Class<? extends Event> event) {
		return false;
	}

}
