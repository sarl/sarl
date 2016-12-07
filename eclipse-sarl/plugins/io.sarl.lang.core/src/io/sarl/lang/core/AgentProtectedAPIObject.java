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

/** This class defines the protected API that is provided to the agents and the agent traits.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.4
 */
abstract class AgentProtectedAPIObject {

	/**
	 * Returns a String representation of the Event E1 attributes only.
	 *
	 * @return the string representation of the attributes of this Event.
	 */
	@Pure
	protected abstract String attributesToString();

	/** Replies the skill corresponding to the given capacity.
	 *
	 * <p>The return may never be <code>null</code>. If not capacity
	 * was set, the exception {@link UnimplementedCapacityException}
	 * is thrown.
	 *
	 * @param <S> - type of the capacity.
	 * @param capacity - the capacity to search for the implementation.
	 * @return the skill, never <code>null</code>
	 * @throws UnimplementedCapacityException - if no skill is owned by the agent for the given capacity.
	 */
	@Pure
	protected abstract <S extends Capacity> S getSkill(Class<S> capacity);

	/** Replies the skill corresponding to the given capacity.
	 *
	 * <p>The return may never be <code>null</code>. If not capacity
	 * was set, the exception {@link UnimplementedCapacityException}
	 * is thrown.
	 *
	 * @param capacity - the capacity to search for the implementation.
	 * @return the reference to the skill, never <code>null</code>
	 * @throws UnimplementedCapacityException - if no skill is owned by the agent for the given capacity.
	 */
	@Pure
	protected abstract ClearableReference<Skill> $getSkill(Class<? extends Capacity> capacity);

	/** Defines the implementation of the "capacity maps-to skill" operator.
	 *
	 * @param <S> - type of skill to be mapped to.
	 * @param capacity - the implemented capacity.
	 * @param skill - the skill to associate to the capacity.
	 */
	protected abstract <S extends Skill> void operator_mappedTo(Class<? extends Capacity> capacity, S skill);

	/**
	 * Set the skill for the {@link Capacity} <code>capacity</code>.
	 *
	 * @param <S> - type of the skill.
	 * @param capacities the capacity or the capacities to set.
	 * @param skill implementaion of <code>capacity</code>.
	 * @return the skill that was set.
	 */
	@SuppressWarnings("unchecked")
	protected abstract <S extends Skill> S setSkill(S skill, Class<? extends Capacity>... capacities);

	/**
	 * Clears the Skill associated with the capacity.
	 *
	 * @param <S> - the type of the capacity.
	 * @param capacity - the capacity for which the skill must be cleared.
	 * @return the skill that was removed
	 */
	protected abstract <S extends Capacity> S clearSkill(Class<S> capacity);

	/**
	 * Checks if this agent has a Skill that implements the {@link Capacity}
	 * <code>capacity</code>.
	 *
	 * @param capacity - capacity to check
	 * @return <code>true</code> if it has a skill associate to this capacity,
	 *     <code>false</code> otherwise
	 */
	@Pure
	protected abstract boolean hasSkill(Class<? extends Capacity> capacity);

	/** Replies if the given address is one of the addresses of this agent.
	 * The test is done on the identifier replied by {@link Address#getUUID()}.
	 *
	 * @param address - the address to test.
	 * @return <code>true</code> if the given address is one of this agent,
	 *     otherwise <code>false</code>.
	 */
	@Pure
	protected abstract boolean isMe(Address address);

	/** Replies if the given identifier corresponds to the identifier
	 * of this agent.
	 *
	 * <p>This function is equivalent to:<pre><code>
	 * id.equals( agent.getID() )
	 * </code></pre>
	 *
	 * @param uID - the identifier to test.
	 * @return <code>true</code> if the given identifier is the one of this agent,
	 *     otherwise <code>false</code>.
	 */
	@Pure
	protected abstract boolean isMe(UUID uID);

	/** Replies if the given event was emitted by this agent.
	 *
	 * @param event - the event to test.
	 * @return <code>true</code> if the given event was emitted by
	 *     this agent; otherwise <code>false</code>.
	 */
	@Pure
	protected abstract boolean isFromMe(Event event);

}
