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

import org.eclipse.xtext.xbase.lib.Pure;

/**
 * Thrown when an {@link Agent} tries to access a skill associated to a Capacity
 * and no implementation has been mapped. To define the skill use
 * {@link Agent#setSkill}.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class UnimplementedCapacityException extends RuntimeException {

	private static final long serialVersionUID = -6286153666879425064L;

	private final Class<? extends Capacity> unimplementedCapacity;

	private final UUID callingAgent;

	/**
	 * Creates a new instance of the exception.
	 *
	 * @param unimplementedCapacity
	 *            the capacitiy that the agent was trying to access.
	 * @param agent the agent accessing the capacity
	 */
	public UnimplementedCapacityException(Class<? extends Capacity> unimplementedCapacity, UUID agent) {
		this(unimplementedCapacity, agent, null);
	}

	/**
	 * Creates a new instance of the exception.
	 *
	 * @param unimplementedCapacity
	 *            the capacitiy that the agent was trying to access.
	 * @param agent the agent accessing the capacity
	 * @param cause the cause of the error.
	 * @since 16.0
	 */
	public UnimplementedCapacityException(Class<? extends Capacity> unimplementedCapacity, UUID agent, Throwable cause) {
		super(unimplementedCapacity.getName(), cause);
		this.unimplementedCapacity = unimplementedCapacity;
		this.callingAgent = agent;
	}

	/**
	 * The ID of the agent trying to access the capacity.
	 *
	 * @return the calling agent.
	 */
	@Pure
	public UUID getCallingAgent() {
		return this.callingAgent;
	}

	/**
	 * The capacity that the agent was trying to access.
	 *
	 * @return the unimplemented capacity.
	 */
	@Pure
	public Class<? extends Capacity> getUnimplementedCapacity() {
		return this.unimplementedCapacity;
	}

}
