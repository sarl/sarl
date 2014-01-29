/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.util;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.EventSpace;



/**
 * Event driven interaction space where agent are free register and unregister themselves.
 * Agents should only register once in this type of space.
 * 
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface OpenEventSpace extends EventSpace {

	/**
	 * Registers the entity inside this space.
	 * After registering a new agent, the Space should emit a ParticipantRegistered
	 * event where the source is the address of the newly registered agent.
	 * 
	 * If the agent is already registered the address is return, but the listener is not replaced.
	 * 
	 * @param entity
	 * @return the entity's address in this space
	 * @fires ParticipantRegistered
	 */
	public Address register(EventListener entity);

	/**
	 * Unregisters the entity inside this space.
	 * Before unregistering an agent, the Space should emit a ParticipantUnregistered
	 * event where the source is the address of the unregistered agent.
	 * 
	 * @param entity
	 * @return the former entity's address
	 * @fires ParticipantUnregistered
	 */
	public Address unregister(EventListener entity);
}
