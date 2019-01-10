/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.janusproject.kernel.bic;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.Set;
import java.util.UUID;

import com.google.common.collect.Sets;
import com.google.inject.Inject;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

import io.janusproject.services.contextspace.ContextSpaceService;

import io.sarl.core.Behaviors;
import io.sarl.core.ContextJoined;
import io.sarl.core.ContextLeft;
import io.sarl.core.ExternalContextAccess;
import io.sarl.core.MemberJoined;
import io.sarl.core.MemberLeft;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.ClearableReference;
import io.sarl.lang.util.SynchronizedCollection;
import io.sarl.util.Collections3;
import io.sarl.util.OpenEventSpace;

/**
 * Skill that permits to access to the context in which the agent is located.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ExternalContextAccessSkill extends BuiltinSkill implements ExternalContextAccess {

	private static int installationOrder = -1;

	private final Set<UUID> contexts = Sets.newConcurrentHashSet();

	@Inject
	private ContextSpaceService contextRepository;

	private ClearableReference<Skill> skillBufferInternalEventBusCapacity;

	private ClearableReference<Skill> skillBufferBehaviors;

	/** Constructor.
	 * @param agent owner of the skill.
	 */
	ExternalContextAccessSkill(Agent agent) {
		super(agent);
	}

	/** Replies the InternalEventBusCapacity skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final InternalEventBusCapacity getInternalEventBusCapacitySkill() {
		if (this.skillBufferInternalEventBusCapacity == null || this.skillBufferInternalEventBusCapacity.get() == null) {
			this.skillBufferInternalEventBusCapacity = $getSkill(InternalEventBusCapacity.class);
		}
		return $castSkill(InternalEventBusCapacity.class, this.skillBufferInternalEventBusCapacity);
	}

	/** Replies the Behaviors skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final Behaviors getBehaviorsSkill() {
		if (this.skillBufferBehaviors == null || this.skillBufferBehaviors.get() == null) {
			this.skillBufferBehaviors = $getSkill(Behaviors.class);
		}
		return $castSkill(Behaviors.class, this.skillBufferBehaviors);
	}

	@Override
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	@Pure
	public void toString(ToStringBuilder builder) {
		super.toString(builder);
		builder.add("contexts", this.contextRepository.toString()); //$NON-NLS-1$
	}

	@Override
	protected void install() {
		final AgentContext ac = this.contextRepository.getContext(getOwner().getParentID());
		join(ac.getID(), ac.getDefaultSpace().getSpaceID().getID());
	}

	@Override
	protected void uninstall(UninstallationStage stage) {
		if (stage == UninstallationStage.POST_DESTROY_EVENT) {
			// Leave all contexts including the default one.
			for (final UUID contextID : this.contexts) {
				leave(contextID);
			}
		}
	}

	@Override
	public SynchronizedCollection<AgentContext> getAllContexts() {
		return Collections3.synchronizedCollection(
				Collections.unmodifiableCollection(this.contextRepository.getContexts(this.contexts)),
				this.contextRepository.mutex());
	}

	@Override
	public AgentContext getContext(UUID contextID) {
		assert contextID != null;
		if (!this.contexts.contains(contextID)) {
			throw new IllegalArgumentException(MessageFormat.format(Messages.ExternalContextAccessSkill_0, contextID));
		}
		return this.contextRepository.getContext(contextID);
	}

	@Override
	public AgentContext getUniverseContext() {
		return this.contextRepository.getUniverseContext();
	}

	@Override
	public boolean join(UUID futureContext, UUID futureContextDefaultSpaceID) {
		assert futureContext != null;
		assert futureContextDefaultSpaceID != null;

		if (this.contexts.contains(futureContext)) {
			return false;
		}

		final AgentContext ac = this.contextRepository.getContext(futureContext);
		assert ac != null : "Unknown Context"; //$NON-NLS-1$

		if (!futureContextDefaultSpaceID.equals(ac.getDefaultSpace().getSpaceID().getID())) {
			return false;
		}

		this.contexts.add(futureContext);

		((OpenEventSpace) ac.getDefaultSpace()).register(getInternalEventBusCapacitySkill().asEventListener());

		fireContextJoined(futureContext, futureContextDefaultSpaceID);
		fireMemberJoined(ac);
		return true;
	}

	/**
	 * Fires an {@link ContextJoined} event into the Inner Context default space of the owner agent to notify behaviors/members
	 * that a new context has been joined.
	 *
	 * @param futureContext ID of the newly joined context
	 * @param futureContextDefaultSpaceID ID of the default space of the newly joined context
	 */
	protected final void fireContextJoined(UUID futureContext, UUID futureContextDefaultSpaceID) {
		getBehaviorsSkill().wake(new ContextJoined(futureContext, futureContextDefaultSpaceID));
	}

	/**
	 * Fires an {@link MemberJoined} event into the newly joined parent Context default space to notify other context's members
	 * that a new agent joined this context.
	 *
	 * @param newJoinedContext the newly joined context to notify its members
	 */
	protected final void fireMemberJoined(AgentContext newJoinedContext) {
		final EventSpace defSpace = newJoinedContext.getDefaultSpace();
		defSpace.emit(
				// No need to give an event source because the event's source is explicitly set below.
				null,
				new MemberJoined(defSpace.getAddress(getOwner().getID()), newJoinedContext.getID(), getOwner().getID(),
				getOwner().getClass().getName()));
	}

	@Override
	public boolean leave(UUID contextID) {
		assert contextID != null;

		final AgentContext ac = this.contextRepository.getContext(contextID);

		assert ac != null : "Unknown Context"; //$NON-NLS-1$

		if (!this.contexts.contains(contextID)) {
			return false;
		}

		// To send this event the agent must still be inside the context and its default space
		fireContextLeft(contextID);
		fireMemberLeft(ac);

		((OpenEventSpace) ac.getDefaultSpace()).unregister(getInternalEventBusCapacitySkill().asEventListener());

		return this.contexts.remove(contextID);
	}

	/**
	 * Fires an {@link ContextLeft} event into the Inner Context Default space of the owner agent to notify behaviors/members that
	 * the specified context has been left.
	 *
	 * @param contextID the ID of context that will be left
	 */
	protected final void fireContextLeft(UUID contextID) {
		getBehaviorsSkill().wake(new ContextLeft(contextID));
	}

	/**
	 * Fires an {@link MemberLeft} event into the default space of the Context that will be left to notify other context's members
	 * that an agent has left this context.
	 *
	 * @param leftContext the context that will be left
	 */
	protected final void fireMemberLeft(AgentContext leftContext) {
		final EventSpace defSpace = leftContext.getDefaultSpace();
		defSpace.emit(
				// No need to give an event source because the event's source is explicitly set below.
				null,
				new MemberLeft(defSpace.getAddress(getOwner().getID()), getOwner().getID(), getOwner().getClass().getName()));
	}

	@Override
	public boolean isInSpace(Event event, Space space) {
		return isInSpace(event, space.getSpaceID());
	}

	@Override
	public boolean isInSpace(Event event, SpaceID spaceID) {
		return spaceID.equals(event.getSource().getSpaceID());
	}

	@Override
	public boolean isInSpace(Event event, UUID spaceID) {
		return spaceID.equals(event.getSource().getSpaceID().getID());
	}

	@Override
	public void emit(EventSpace space, Event event, Scope<Address> scope) {
		if (space != null) {
			space.emit(getOwner().getID(), event, scope);
		}
	}

}
