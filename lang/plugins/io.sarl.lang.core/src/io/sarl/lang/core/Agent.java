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
package io.sarl.lang.core;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

import javax.inject.Inject;

/**
 * The definition of the notion of Agent in SARL
 * An agent is an autonomous entity having some intrinsic skills to realize the capacities it exhibits. 
 * An agent defines a context.
 *  
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class Agent implements Identifiable {

	private final UUID id = UUID.randomUUID();

	@Inject
	private Logger log;

	private Map<Class<? extends Capacity>, Capacity> capacities = new ConcurrentHashMap<>();

	/**
	 * {@inheritDoc}
	 */
	@Override
	public UUID getId() {
		return this.id;
	}

	/**
	 * Set the skill for the {@link Capacity} <code>capacity</code>
	 * 
	 * @param capacity
	 *            capacity to set
	 * @param skill
	 *            implementaion of <code>capacity</code>
	 * @return the skill that was set
	 */
	protected <C extends Capacity> C setSkill(Class<? extends Capacity> capacity, C skill) {
		assert capacity != null;
		assert skill != null;
		return (C) this.capacities.put(capacity, skill);
	}

	/**
	 * Clears the Skill associated with the capacity
	 * 
	 * @param capacity
	 * @return the skill that was removed
	 */
	protected <C extends Capacity> C clearSkill(Class<C> capacity) {
		assert capacity != null;
		return (C) this.capacities.remove(capacity);
	}

	/**
	 * Replies with the skill associated to the {@link Capacity}
	 * <code>capacity</code> The return may be <code>null</code> if not capacity
	 * was set
	 * 
	 * @param capacity
	 * @return the skill
	 */
	protected <C extends Capacity> C getSkill(Class<C> capacity) {
		assert capacity != null;
		C skill = (C) this.capacities.get(capacity);
		if(skill == null){
			throw new UnimplementedCapacityException(capacity, this.getId());
		}
		return skill;
	}

	/**
	 * Checks if this agent has a Skill that implements the {@link Capacity}
	 * <code>capacity</code>
	 * 
	 * @param capacity
	 *            capacity to check
	 * @return true if it has a skill associate to this capacity, false
	 *         otherwise
	 */
	protected boolean hasSkill(Class<? extends Capacity> capacity) {
		assert capacity != null;
		return this.capacities.containsKey(capacity);
	}

	protected <C extends Capacity> void operator_mappedTo(Class<C> capacity, C skill) {
		setSkill(capacity, skill);
	}

	@Inject
	void setPlatform(BuiltinCapacitiesProvider platform) {
		log.finer("Setting platform");
		for (Class<? extends Capacity> capCls : platform.getBuiltinCapacities()) {
			log.finer("Adding Builtin Capacity : " + capCls);
			capacities.put(capCls, platform.getBuiltinCapacity(capCls, this));
		}
	}

}
