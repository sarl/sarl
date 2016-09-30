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

import java.lang.ref.WeakReference;
import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

/** This class represents a part of trait of an agent.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
abstract class AgentTrait extends AgentProtectedAPIObject {

	private WeakReference<Agent> agentRef;

	/** Construct a trait to the given agent.
	 *
	 * @param agent - the owner of this trait.
	 */
	AgentTrait(Agent agent) {
		this.agentRef = new WeakReference<>(agent);
	}

	/** Construct a trait.
	 */
	AgentTrait() {
		this.agentRef = new WeakReference<>(null);
	}

	@Override
	@Pure
	protected String attributesToString() {
		final StringBuilder result = new StringBuilder();
		result.append("owner = "); //$NON-NLS-1$
		result.append(getOwner());
		return result.toString();
	}

	@Override
	@Pure
	public String toString() {
		return getClass().getSimpleName()
				+ " [" + attributesToString() //$NON-NLS-1$
				+ "]"; //$NON-NLS-1$
	}

	/** Set the agent that has this trait.
	 *
	 * @param agent - the owner of this trait.
	 */
	void setOwner(Agent agent) {
		this.agentRef = new WeakReference<>(agent);
	}

	/** Replies the agent that has this trait.
	 *
	 * @return the owner.
	 */
	@Pure
	protected Agent getOwner() {
		return this.agentRef.get();
	}

	@Override
	@Pure
	protected <S extends Capacity> S getSkill(Class<S> capacity) {
		final Agent owner = getOwner();
		if (owner == null) {
			return null;
		}
		return owner.getSkill(capacity);
	}

	@Override
	@Inline("$setSkill($2, $1)")
	protected <S extends Skill> void operator_mappedTo(Class<? extends Capacity> capacity, S skill) {
		setSkill(skill, capacity);
	}

	@Override
	@SafeVarargs
	@Inline("$setSkill($1, $2)")
	protected final <S extends Skill> S setSkill(S skill, Class<? extends Capacity>... capacities) {
		return $setSkill(skill, capacities);
	}

	@Override
	@SuppressWarnings("unchecked")
    protected <S extends Skill> S $setSkill(S skill, Class<? extends Capacity>... capacities) {
		final Agent owner = getOwner();
		if (owner == null) {
			return skill;
		}
		return owner.$setSkill(skill, capacities);
	}

	@Override
	protected <S extends Capacity> S clearSkill(Class<S> capacity) {
		final Agent owner = getOwner();
		if (owner == null) {
			return null;
		}
		return owner.clearSkill(capacity);
	}

	@Override
	@Pure
	protected boolean hasSkill(Class<? extends Capacity> capacity) {
		final Agent owner = getOwner();
		if (owner == null) {
			return false;
		}
		return owner.hasSkill(capacity);
	}

	@Override
	@Pure
	protected boolean isMe(Address address) {
		final Agent owner = getOwner();
		if (owner == null) {
			return false;
		}
		return owner.isMe(address);
	}

	@Override
	@Pure
	protected boolean isMe(UUID uID) {
		final Agent owner = getOwner();
		if (owner == null) {
			return false;
		}
		return owner.isMe(uID);
	}

	@Override
	@Pure
	protected boolean isFromMe(Event event) {
		final Agent owner = getOwner();
		if (owner == null) {
			return false;
		}
		return owner.isFromMe(event);
	}

}
