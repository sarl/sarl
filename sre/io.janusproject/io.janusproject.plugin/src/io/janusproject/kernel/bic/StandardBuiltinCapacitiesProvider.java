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

import java.util.UUID;

import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;

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
	public void builtinCapacities(Agent agent, Procedure2<? super Class<? extends Capacity>, ? super Skill> skillMappingCallback) {
		if (skillMappingCallback != null) {
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

			skillMappingCallback.apply(MicroKernelCapacity.class, microKernelSkill);
			skillMappingCallback.apply(InternalEventBusCapacity.class, eventBusSkill);
			skillMappingCallback.apply(InnerContextAccess.class, innerContextSkill);
			skillMappingCallback.apply(Behaviors.class, behaviorSkill);
			skillMappingCallback.apply(Lifecycle.class, lifecycleSkill);
			skillMappingCallback.apply(ExternalContextAccess.class, externalContextSkill);
			skillMappingCallback.apply(DefaultContextInteractions.class, interactionSkill);
			skillMappingCallback.apply(Schedules.class, scheduleSkill);
			skillMappingCallback.apply(Logging.class, loggingSkill);
			skillMappingCallback.apply(Time.class, timeSkill);

			this.spawnService.addSpawnServiceListener(agent.getID(),
					new AgentLifeCycleSupport(agent.getID(), this.spawnService, eventBusSkill));
		}
	}

}
