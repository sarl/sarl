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

import java.lang.ref.WeakReference;

/**
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
abstract class AgentTrait {

	private WeakReference<Agent> agentRef;

	/**
	 * @param agent
	 */
	public AgentTrait(Agent agent) {
		this.agentRef = new WeakReference<>(agent);
	}

	/** Replies the agent that has this trait.
	 * 
	 * @return the owner.
	 */
	protected Agent getOwner() {
		return this.agentRef.get();
	}

	/** Replies the skill corresponding to the given capacity.
	 * 
	 * @param capacity
	 * @return the skill.
	 */
	protected <S extends Capacity> S getSkill(Class<S> capacity) {
		return getOwner().getSkill(capacity);
	}
	
	/** Defines the implementation of the "capacity maps-to skill" operator. 
	 * 
	 * @param capacity
	 * @param skill
	 */
	protected <S extends Skill & Capacity> void operator_mappedTo(Class<? extends Capacity> capacity, S skill) {
		getOwner().setSkill(capacity, skill);
	}

}
