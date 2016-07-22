/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.kernel.bic;

import java.lang.ref.WeakReference;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import com.google.inject.Inject;
import com.google.inject.Injector;
import io.janusproject.kernel.Kernel;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.services.spawn.SpawnServiceListener;

import io.sarl.core.Behaviors;
import io.sarl.core.DefaultContextInteractions;
import io.sarl.core.Destroy;
import io.sarl.core.ExternalContextAccess;
import io.sarl.core.Initialize;
import io.sarl.core.InnerContextAccess;
import io.sarl.core.Lifecycle;
import io.sarl.core.Logging;
import io.sarl.core.Schedules;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
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

	@Inject
	private Injector injector;

	@Inject
	private SpawnService spawnService;

	@Inject
	private ContextSpaceService contextRepository;

	@Override
	public Map<Class<? extends Capacity>, Skill> getBuiltinCapacities(Agent agent) {
		UUID innerContextID = agent.getID();
		SpaceID innerSpaceID = new SpaceID(innerContextID, UUID.randomUUID(), OpenEventSpaceSpecification.class);
		Address agentAddressInInnerSpace = new Address(innerSpaceID, agent.getID());
		Kernel k = this.injector.getInstance(Kernel.class);

		InternalEventBusSkill eventBusSkill = new InternalEventBusSkill(agent, agentAddressInInnerSpace);
		InnerContextSkill innerContextSkill = new InnerContextSkill(agent, agentAddressInInnerSpace);
		BehaviorsSkill behaviorSkill = new BehaviorsSkill(agent, agentAddressInInnerSpace);
		LifecycleSkill lifecycleSkill = new LifecycleSkill(agent);
		ExternalContextAccessSkill externalContextSkill = new ExternalContextAccessSkill(agent);
		DefaultContextInteractionsSkill interactionSkill = new DefaultContextInteractionsSkill(agent,
				this.contextRepository.getContext(agent.getParentID()));
		SchedulesSkill scheduleSkill = new SchedulesSkill(agent);
		LoggingSkill loggingSkill = new LoggingSkill(agent);

		this.injector.injectMembers(eventBusSkill);
		this.injector.injectMembers(innerContextSkill);
		this.injector.injectMembers(behaviorSkill);
		this.injector.injectMembers(lifecycleSkill);
		this.injector.injectMembers(externalContextSkill);
		this.injector.injectMembers(interactionSkill);
		this.injector.injectMembers(scheduleSkill);
		this.injector.injectMembers(loggingSkill);

		MicroKernelSkill microKernelSkill = new MicroKernelSkill(agent, k);

		// no need to be synchronized
		Map<Class<? extends Capacity>, Skill> result = new HashMap<>();
		result.put(MicroKernelCapacity.class, microKernelSkill);
		result.put(InternalEventBusCapacity.class, eventBusSkill);
		result.put(InnerContextAccess.class, innerContextSkill);
		result.put(Behaviors.class, behaviorSkill);
		result.put(Lifecycle.class, lifecycleSkill);
		result.put(ExternalContextAccess.class, externalContextSkill);
		result.put(DefaultContextInteractions.class, interactionSkill);
		result.put(Schedules.class, scheduleSkill);
		result.put(Logging.class, loggingSkill);

		this.spawnService.addSpawnServiceListener(agent.getID(),
				new AgentLifeCycleSupport(agent.getID(), this.spawnService, eventBusSkill, microKernelSkill, innerContextSkill,
						behaviorSkill, lifecycleSkill, externalContextSkill, interactionSkill, scheduleSkill, loggingSkill));

		// Test if all the BICs are installed.
		assert (result.get(Behaviors.class) != null);
		assert (result.get(DefaultContextInteractions.class) != null);
		assert (result.get(InternalEventBusCapacity.class) != null);
		assert (result.get(ExternalContextAccess.class) != null);
		assert (result.get(InnerContextAccess.class) != null);
		assert (result.get(Lifecycle.class) != null);
		assert (result.get(Schedules.class) != null);
		assert (result.get(MicroKernelCapacity.class) != null);
		assert (result.get(Logging.class) != null);

		return result;
	}

	/**
	 * Implementation of the agent's cycle.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class AgentLifeCycleSupport implements SpawnServiceListener {

		private final UUID agentID;

		private final WeakReference<SpawnService> spawnService;

		private final InternalEventBusCapacity eventBusCapacity;

		private final Skill[] skills;

		/**
		 * @param agentId - identifier of the agent for which this class is created.
		 * @param spawnService - the agent spawning service.
		 * @param eventBusCapacity - the capacity of the agent to manage an internal bus.
		 * @param skills - the skills for the built-in capacities.
		 */
		AgentLifeCycleSupport(UUID agentId, SpawnService spawnService, InternalEventBusCapacity eventBusCapacity,
				Skill... skills) {
			this.agentID = agentId;
			this.spawnService = new WeakReference<>(spawnService);
			this.eventBusCapacity = eventBusCapacity;
			this.skills = skills;
		}

		@Override
		public void agentSpawned(AgentContext parent, Agent agent, Object[] initializationParameters) {
			try {
				// Use reflection to ignore the "protected" access right.
				Method method = Skill.class.getDeclaredMethod("install"); //$NON-NLS-1$
				boolean isAccessible = method.isAccessible();
				try {
					method.setAccessible(true);
					method.invoke(this.eventBusCapacity);
					for (Skill s : this.skills) {
						method.invoke(s);
					}
				} finally {
					method.setAccessible(isAccessible);
				}
			} catch (RuntimeException e) {
				throw e;
			} catch (Exception e) {
				throw new RuntimeException(e);
			}

			Initialize init = new Initialize();
			init.parameters = initializationParameters;
			this.eventBusCapacity.selfEvent(init);
		}

		@Override
		public void agentDestroy(Agent agent) {
			SpawnService service = this.spawnService.get();
			assert (service != null);
			service.removeSpawnServiceListener(this.agentID, this);

			Destroy destroy = new Destroy();
			this.eventBusCapacity.selfEvent(destroy);

			try {
				// Use reflection to ignore the "protected" access right.
				Method method = Skill.class.getDeclaredMethod("uninstall"); //$NON-NLS-1$
				boolean isAccessible = method.isAccessible();
				try {
					method.setAccessible(true);
					for (int i = this.skills.length - 1; i >= 0; i--) {
						method.invoke(this.skills[i]);
					}
					method.invoke(this.eventBusCapacity);
				} finally {
					method.setAccessible(isAccessible);
				}
			} catch (RuntimeException e) {
				throw e;
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

	}

}
