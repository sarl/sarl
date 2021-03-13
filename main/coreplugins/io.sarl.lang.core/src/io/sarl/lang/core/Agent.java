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
import javax.inject.Inject;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.annotation.SarlSpecification;


/**
 * The definition of the notion of Agent in SARL.
 * An agent is an autonomous entity having some intrinsic skills to realize
 * the capacities it exhibits. An agent defines a context.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SarlSpecification(SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING)
public class Agent extends AbstractSkillContainer implements IBehaviorGuardEvaluatorReceiver {

	private final UUID id;

	private final UUID parentID;

	private volatile Procedure2<Agent, Skill> skillCallback;

	/**
	 * Creates a new agent with a parent <code>parentID</code> without initializing the built-in capacities.
	 *
	 * @param parentID the agent's spawner.
	 * @param agentID the identifier of the agent, or
	 *                  {@code null} for computing it randomly.
	 * @since 0.5
	 */
	public Agent(
			UUID parentID,
			UUID agentID) {
		this(parentID, agentID, null);
	}

	/**
	 * Creates a new agent with a parent <code>parentID</code> without initializing the built-in capacities.
	 *
	 * @param parentID the agent's spawner.
	 * @param agentID the identifier of the agent, or
	 *                  {@code null} for computing it randomly.
	 * @param skillProvider provides the skills dynamically on demand.
	 * @since 0.6
	 */
	@Inject
	public Agent(
			UUID parentID,
			UUID agentID,
			DynamicSkillProvider skillProvider) {
		super(skillProvider);
		this.parentID = parentID;
		this.id = (agentID == null) ? UUID.randomUUID() : agentID;
	}

	@Override
	@Pure
	protected void toString(ToStringBuilder builder) {
		builder.add("type", getClass().getSimpleName()); //$NON-NLS-1$
		builder.add("id", this.id); //$NON-NLS-1$
		builder.add("parentID", this.parentID); //$NON-NLS-1$
	}

	@Override
	protected final void $attachOwner(Skill skill) {
		skill.setOwner(this);
		final Procedure2<Agent, Skill> cb = this.skillCallback;
		if (cb != null) {
			cb.apply(this, skill);
		}
	}

	/** Change the callback for the skill.
	 * This callback is invoked each time a skill is attached to the agent.
	 *
	 * @param callback the callback.
	 * @since 0.12
	 */
	void setSkillCallback(Procedure2<Agent, Skill> callback) {
		this.skillCallback = callback;
	}

	/**
	 * Replies the agent's parent's ID.
	 *
	 * @return the identifier of the agent's parent.
	 */
	@Pure
	public UUID getParentID() {
		return this.parentID;
	}

	@Override
	@Pure
	public UUID getID() {
		return this.id;
	}

	@Override
	@Pure
	@Inline(value = "($1 != null && $0getID().equals($1.getID()))", constantExpression = true)
	protected boolean isMe(Address address) {
		return address != null && isMe(address.getID());
	}

	@Override
	@Pure
	@Inline(value = "getID().equals($1)")
	protected boolean isMe(UUID uID) {
		return uID != null && getID().equals(uID);
	}

	@Override
	@Pure
	@Inline(value = "($1 != null && $0getID().equals($1.getSource().getID()))", constantExpression = true)
	protected boolean isFromMe(Event event) {
		return event != null && isMe(event.getSource());
	}

}
