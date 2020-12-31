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

import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Event driven Interaction {@link Space} for agents.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface EventSpace extends Space {

	/**
	 * Returns the address of the agent identified by <code>id</code>.
	 *
	 * @param id the agent's id.
	 * @return the agent's address in this space.
	 */
	@Pure
	Address getAddress(UUID id);

	/**
	 * Emits the event inside this space with the given scope. Only agents
	 * matching the scope will receive the event.
	 *
	 * <p>This function does not change the source of the event if it was set.
	 *
	 * <p>If the given event has no specified source, the emit function uses the
	 * {@code eventSource} parameter to set the source's address.
	 *
	 * @param eventSource the sender of the event.
	 * @param event the event to emit in the space.
	 * @param scope the definition of the list of receivers of the event.
	 * @since 0.6
	 */
	void emit(UUID eventSource, Event event, Scope<Address> scope);

	/**
	 * Emits the event inside this space. All registered agents will receive the event.
	 *
	 * <p>This function does not change the source of the event if it was set.
	 *
	 * <p>If the given event has no specified source, the emit function uses the
	 * {@code eventSource} parameter to set the source's address.
	 *
	 * @param eventSource the sender of the event.
	 * @param event the event to emit in the space.
	 * @since 0.6
	 */
	@Inline(value = "emit($1, $2, null)")
	default void emit(UUID eventSource, Event event) {
		emit(eventSource, event, null);
	}

}
