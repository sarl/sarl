/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.google.inject.Inject;
import com.google.inject.Injector;

import io.janusproject.kernel.Kernel;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.spawn.SpawnService;

import io.sarl.core.Behaviors;
import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.ExternalContextAccess;
import io.sarl.core.InnerContextAccess;
import io.sarl.core.Lifecycle;
import io.sarl.core.Logging;
import io.sarl.core.Schedules;
import io.sarl.core.Time;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.BuiltinCapacitiesProvider;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Skill;
import io.sarl.lang.core.SpaceID;
import io.sarl.util.OpenEventSpaceSpecification;

/**
 * Provider of the built-in capacities of the Janus platform.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardBuiltinCapacitiesProvider implements BuiltinCapacitiesProvider {

	/** Order of installation of the BIC skills.
	 *
	 * <p>The skills that are not present in the table are assumed to be installed after all the others.
	 */
	@SuppressWarnings("unchecked")
	static final Class<? extends BuiltinSkill>[] SKILL_INSTALLATION_ORDER = new Class[] {
		//
		// The order depends on the dependencies of the skill to the other capacities:
		// a skill using a capacity should be launched after the skill implemented this latter capacity.
		//
		// MicroKernelSkill ->
		MicroKernelSkill.class,
		// LoggingSkill ->
		LoggingSkill.class,
		// TimeSkill ->
		TimeSkill.class,
		// SchedulesSkill -> Logging
		SchedulesSkill.class,
		// InternalEventBusSkill -> Logging
		InternalEventBusSkill.class,
		// LifecycleSkill -> InternalEventBusCapacity
		LifecycleSkill.class,
		// InnerContextSkill -> InternalEventBusCapacity
		InnerContextSkill.class,
		// DefaultContextInteractionsSkill -> Lifecycle
		DefaultContextInteractionsSkill.class,
		// BehaviorsSkill -> InternalEventBusCapacity, InnerContextAccess, Schedules
		BehaviorsSkill.class,
		// ExternalContextAccessSkill -> InternalEventBusCapacity, Behaviors
		ExternalContextAccessSkill.class,
	};

	@Inject
	private Injector injector;

	@Inject
	private SpawnService spawnService;

	@Inject
	private ContextSpaceService contextRepository;

	@Override
	public Map<Class<? extends Capacity>, Skill> getBuiltinCapacities(Agent agent) {
		final UUID innerContextID = agent.getID();
		final SpaceID innerSpaceID = new SpaceID(innerContextID, UUID.randomUUID(), OpenEventSpaceSpecification.class);
		final Address agentAddressInInnerSpace = new Address(innerSpaceID, agent.getID());
		final Kernel k = this.injector.getInstance(Kernel.class);

		final InternalEventBusSkill eventBusSkill = new InternalEventBusSkill(agent, agentAddressInInnerSpace);
		final InnerContextSkill innerContextSkill = new InnerContextSkill(agent, agentAddressInInnerSpace);
		final BehaviorsSkill behaviorSkill = new BehaviorsSkill(agent, agentAddressInInnerSpace);
		final LifecycleSkill lifecycleSkill = new LifecycleSkill(agent);
		final ExternalContextAccessSkill externalContextSkill = new ExternalContextAccessSkill(agent);
		final DefaultContextInteractionsSkill interactionSkill = new DefaultContextInteractionsSkill(agent,
				this.contextRepository.getContext(agent.getParentID()));
		final SchedulesSkill scheduleSkill = new SchedulesSkill(agent);
		final LoggingSkill loggingSkill = new LoggingSkill(agent);
		final TimeSkill timeSkill = new TimeSkill(agent);

		this.injector.injectMembers(eventBusSkill);
		this.injector.injectMembers(innerContextSkill);
		this.injector.injectMembers(behaviorSkill);
		this.injector.injectMembers(lifecycleSkill);
		this.injector.injectMembers(externalContextSkill);
		this.injector.injectMembers(interactionSkill);
		this.injector.injectMembers(scheduleSkill);
		this.injector.injectMembers(loggingSkill);
		this.injector.injectMembers(timeSkill);

		final MicroKernelSkill microKernelSkill = new MicroKernelSkill(agent, k);

		// no need to be synchronized
		final Map<Class<? extends Capacity>, Skill> result = new HashMap<>();
		result.put(MicroKernelCapacity.class, microKernelSkill);
		result.put(InternalEventBusCapacity.class, eventBusSkill);
		result.put(InnerContextAccess.class, innerContextSkill);
		result.put(Behaviors.class, behaviorSkill);
		result.put(Lifecycle.class, lifecycleSkill);
		result.put(ExternalContextAccess.class, externalContextSkill);
		result.put(DefaultContextInteractions.class, interactionSkill);
		result.put(Schedules.class, scheduleSkill);
		result.put(Logging.class, loggingSkill);
		result.put(Time.class, timeSkill);

		this.spawnService.addSpawnServiceListener(agent.getID(),
				new AgentLifeCycleSupport(agent.getID(), this.spawnService, eventBusSkill));

		// Test if all the BICs are installed.
		assert result.get(Behaviors.class) != null;
		assert result.get(DefaultContextInteractions.class) != null;
		assert result.get(InternalEventBusCapacity.class) != null;
		assert result.get(ExternalContextAccess.class) != null;
		assert result.get(InnerContextAccess.class) != null;
		assert result.get(Lifecycle.class) != null;
		assert result.get(Schedules.class) != null;
		assert result.get(MicroKernelCapacity.class) != null;
		assert result.get(Logging.class) != null;
		assert result.get(Time.class) != null;

		return result;
	}

}
