/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import java.security.InvalidParameterException;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import javax.inject.Inject;

/**
 * The definition of the notion of Agent in SARL.
 * An agent is an autonomous entity having some intrinsic skills to realize
 * the capacities it exhibits. An agent defines a context.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class Agent implements Identifiable {

	private final UUID id = UUID.randomUUID();

	private Map<Class<? extends Capacity>, Skill> capacities = new ConcurrentHashMap<>();

	private final UUID parentID;

	/**
	 * Creates a new agent by parent <code>parentID</code>.
	 *
	 * @param parentID - the agent's spawner.
	 */
	public Agent(UUID parentID) {
		this.parentID = parentID;
	}

	/**
	 * Returns a String representation of the Event E1 attributes only.
	 *
	 * @return the string representation of the attributes of this Event.
	 */
	protected String attributesToString() {
		StringBuilder builder = new StringBuilder();
		builder.append("id = "); //$NON-NLS-1$
		builder.append(this.id);
		builder.append(", parentID="); //$NON-NLS-1$
		builder.append(this.parentID);
		return builder.toString();
	}

	@Override
	public String toString() {
		return getClass().getSimpleName()
				+ " [" + attributesToString() //$NON-NLS-1$
				+ "]"; //$NON-NLS-1$
	}

	/**
	 * Replies the agent's spawner's ID.
	 *
	 * @return the identifier of the agent's spawner.
	 */
	public UUID getParentID() {
		return this.parentID;
	}

	@Override
	public UUID getID() {
		return this.id;
	}

	/**
	 * Set the skill for the {@link Capacity} <code>capacity</code>.
	 *
	 * @param <S> - type of the skill.
	 * @param capacity capacity to set.
	 * @param skill implementaion of <code>capacity</code>.
	 * @return the skill that was set.
	 */
	protected <S extends Skill> S setSkill(Class<? extends Capacity> capacity, S skill) {
		assert capacity != null : "the capacity parameter must not be null"; //$NON-NLS-1$
		assert capacity.isInterface() : "the capacity parameter must be an interface"; //$NON-NLS-1$
		assert skill != null : "the skill parameter must not be null"; //$NON-NLS-1$
		if (!capacity.isInstance(skill)) {
			throw new InvalidParameterException(
					"the skill must implement the given capacity " //$NON-NLS-1$
					+ capacity.getName());
		}
		skill.setOwner(this);
		Skill oldS = this.capacities.put(capacity, skill);
		if (oldS != null) {
			oldS.uninstall();
		}
		skill.install();
		return skill;
	}

	/** Implementation of the operator "capacity maps-to skill".
	 *
	 * @param <S> - type of the skill.
	 * @param capacity - the capacity to map.
	 * @param skill - the skill to be mapped to.
	 */
	protected <S extends Skill> void operator_mappedTo(Class<? extends Capacity> capacity, S skill) {
		setSkill(capacity, skill);
	}

	/**
	 * Clears the Skill associated with the capacity.
	 *
	 * @param <S> - the type of the skill.
	 * @param capacity - the capacity for which the skill must be cleared.
	 * @return the skill that was removed
	 */
	@SuppressWarnings("unchecked")
	protected <S extends Skill & Capacity> S clearSkill(Class<? extends Capacity> capacity) {
		assert capacity != null;
		Skill s = this.capacities.remove(capacity);
		if (s != null) {
			s.uninstall();
		}
		return (S) s;
	}

	/**
	 * Replies with the skill associated to the {@link Capacity}
	 * <code>capacity</code>. The return may be <code>null</code> if not capacity
	 * was set.
	 *
	 * @param <S> - the type of the capacity.
	 * @param capacity - the capacity to retreive.
	 * @return the skill.
	 */
	protected <S extends Capacity> S getSkill(Class<S> capacity) {
		assert capacity != null;
		S skill = capacity.cast(this.capacities.get(capacity));
		if (skill == null) {
			throw new UnimplementedCapacityException(capacity, this.getID());
		}
		return skill;
	}

	/**
	 * Checks if this agent has a Skill that implements the {@link Capacity}
	 * <code>capacity</code>.
	 *
	 * @param capacity - capacity to check
	 * @return <code>true</code> if it has a skill associate to this capacity,
	 * <code>false</code> otherwise
	 */
	protected boolean hasSkill(Class<? extends Capacity> capacity) {
		assert capacity != null;
		return this.capacities.containsKey(capacity);
	}

	/** Set the provider of the built-in capacities.
	 *
	 * @param provider - the provider of built-in capacities for this agent.
	 */
	@Inject
	void setBuiltinCapacitiesProvider(BuiltinCapacitiesProvider provider) {
		this.capacities.putAll(provider.getBuiltinCapacities(this));
	}

	/** Replies if the given address is one of the addresses of this agent.
	 * The test is done on the identifier replied by {@link Address#getUUID()}.
	 *
	 * @param address - the address to test.
	 * @return <code>true</code> if the given address is one of this agent,
	 * otherwise <code>false</code>.
	 */
	protected boolean isMe(Address address) {
		return (address != null) && (this.id.equals(address.getUUID()));
	}

	/** Replies if the given identifier corresponds to the identifier
	 * of this agent.
	 * <p>
	 * This function is equivalent to:<pre><code>
	 * id.equals( agent.getID() )
	 * </code></pre>
	 *
	 * @param id - the identifier to test.
	 * @return <code>true</code> if the given identifier is the one of this agent,
	 * otherwise <code>false</code>.
	 */
	protected boolean isMe(UUID id) {
		return (id != null) && (this.id.equals(id));
	}

	/** Replies if the given event was emitted by this agent.
	 *
	 * @param event - the event to test.
	 * @return <code>true</code> if the given event was emitted by
	 * this agent; otherwise <code>false</code>.
	 */
	protected boolean isFromMe(Event event) {
		return (event != null) && isMe(event.getSource());
	}

}
