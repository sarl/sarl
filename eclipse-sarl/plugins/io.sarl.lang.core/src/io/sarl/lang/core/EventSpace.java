/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Event driven Interaction {@link Space} for agents.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface EventSpace extends Space {

	/**
	 * Returns the address of the agent identified by <code>id</code>.
	 *
	 * @param id - the agent's id.
	 * @return the agent's address in this space.
	 */
	@Pure
	Address getAddress(UUID id);

	/**
	 * Emits the event inside this space with the given scope. Only agents
	 * matching the scope will receive the event.
	 *
	 * @param event - the event to emit in the space.
	 * @param scope - the definition of the list of receiviers of the event.
	 */
	void emit(Event event, Scope<Address> scope);

	/**
	 * Emits the event inside this space. All registered agents will receive the event.
	 *
	 * @param event - the event to emit in the space.
	 */
	void emit(Event event);

}
