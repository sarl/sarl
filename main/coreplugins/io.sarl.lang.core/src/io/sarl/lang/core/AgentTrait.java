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

import java.lang.ref.WeakReference;
import java.util.UUID;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

/** This class represents a part of trait of an agent.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AgentTrait extends AgentProtectedAPIObject {

	private WeakReference<Agent> agentRef;

	/** Construct a trait to the given agent.
	 *
	 * @param agent the owner of this trait.
	 */
	protected AgentTrait(Agent agent) {
		this.agentRef = new WeakReference<>(agent);
	}

	/** Construct a trait.
	 */
	protected AgentTrait() {
		this.agentRef = new WeakReference<>(null);
	}

	@Override
	protected void toString(ToStringBuilder builder) {
		builder.add("type", getClass().getSimpleName()); //$NON-NLS-1$
		builder.add("owner", getOwner()); //$NON-NLS-1$
	}

	/** Set the agent that has this trait.
	 *
	 * @param agent the owner of this trait.
	 */
	void setOwner(Agent agent) {
		this.agentRef = new WeakReference<>(agent);
	}

	/** Replies the agent that has this trait.
	 *
	 * @return the owner.
	 */
	@Pure
	public Agent getOwner() {
		return this.agentRef.get();
	}

	/** Replies the identifier of the agent that has this trait.
	 *
	 * @return the UUID of the owner.
	 * @since 0.6
	 */
	@Pure
	@Inline("getOwner().getID()")
	public UUID getID() {
		return getOwner().getID();
	}

	@Override
	@Pure
	protected final <S extends Capacity> S getSkill(Class<S> capacity) {
		assert capacity != null;
		final AtomicSkillReference skill = $getSkill(capacity);
		assert skill != null;
		return $castSkill(capacity, skill);
	}

	/** Cast the skill reference to the given capacity type.
	 *
	 * @param <S> the expected capacity type.
	 * @param capacity the expected capacity type.
	 * @param skillReference the skill reference.
	 * @return the skill casted to the given capacity.
	 */
	@Pure
	protected <S extends Capacity> S $castSkill(Class<S> capacity, AtomicSkillReference skillReference) {
		if (skillReference != null) {
			final S skill = capacity.cast(skillReference.get());
			if (skill != null) {
				return skill;
			}
		}
		throw new UnimplementedCapacityException(capacity, getOwner().getID());
	}

	@Override
	protected AtomicSkillReference $getSkill(Class<? extends Capacity> capacity) {
		final Agent owner = getOwner();
		if (owner == null) {
			throw new OwnerNotFoundException(this);
		}
		return owner.$getSkill(capacity);
	}

	@Override
	@Inline("setSkill($2, $1)")
	protected <S extends Skill> void operator_mappedTo(Class<? extends Capacity> capacity, S skill) {
		setSkill(skill, capacity);
	}

	@Override
	@SafeVarargs
	protected final <S extends Skill> S setSkill(S skill, Class<? extends Capacity>... capacities) {
		final Agent owner = getOwner();
		if (owner == null) {
			return skill;
		}
		return owner.setSkill(skill, capacities);
	}

	@Override
	@SafeVarargs
	protected final void setSkillIfAbsent(Skill skill, Class<? extends Capacity>... capacities) {
		final Agent owner = getOwner();
		if (owner != null) {
			owner.setSkillIfAbsent(skill, capacities);
		}
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
	public boolean isMe(Address address) {
		final Agent owner = getOwner();
		if (owner == null) {
			return false;
		}
		return owner.isMe(address);
	}

	@Override
	@Pure
	public boolean isMe(UUID uID) {
		final Agent owner = getOwner();
		if (owner == null) {
			return false;
		}
		return owner.isMe(uID);
	}

	@Override
	@Pure
	public boolean isFromMe(Event event) {
		final Agent owner = getOwner();
		if (owner == null) {
			return false;
		}
		return owner.isFromMe(event);
	}

}
