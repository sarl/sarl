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

package io.janusproject.kernel.bic;

import java.lang.ref.WeakReference;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.UUID;

import org.arakhne.afc.util.ListUtil;
import org.arakhne.afc.util.MultiCollection;

import io.janusproject.services.spawn.SpawnService;
import io.janusproject.services.spawn.SpawnServiceListener;

import io.sarl.core.Destroy;
import io.sarl.core.Initialize;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.ClearableReference;
import io.sarl.lang.core.Skill;

/**
 * Implementation of the agent's cycle.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class AgentLifeCycleSupport implements SpawnServiceListener {

	private static final Comparator<BuiltinSkill> ORDER_COMPARATOR = new OrderComparator();

	private static final Comparator<BuiltinSkill> REVERSE_ORDER_COMPARATOR = new ReverseOrderComparator();

	private static Method skillInstallationMethod;

	private static Method skillUninstallationMethod;

	private static Field agentSkillField;

	private final UUID agentID;

	private final WeakReference<SpawnService> spawnService;

	private final InternalEventBusCapacity eventBusCapacity;

	/**
	 * @param agentID - the identifier the agent for which this class is created.
	 * @param spawnService - the agent spawning service.
	 * @param eventBusCapacity - the capacity of the agent to manage an internal bus.
	 */
	AgentLifeCycleSupport(UUID agentID, SpawnService spawnService, InternalEventBusCapacity eventBusCapacity) {
		this.agentID = agentID;
		this.spawnService = new WeakReference<>(spawnService);
		this.eventBusCapacity = eventBusCapacity;
	}

	@Override
	public void agentSpawned(AgentContext parent, List<Agent> agents, Object[] initializationParameters) {
		// Install the skills
		installSkills(agents.get(0));
		// Notify the agent about its creation.
		final Initialize init = new Initialize();
		init.parameters = initializationParameters;
		this.eventBusCapacity.selfEvent(init);
	}

	@Override
	public void agentDestroy(Agent agent) {
		final SpawnService service = this.spawnService.get();
		assert service != null;
		service.removeSpawnServiceListener(this.agentID, this);

		// Notify the agent about its destruction
		final Destroy destroy = new Destroy();
		this.eventBusCapacity.selfEvent(destroy);

		// Uninstall the skills (BIC and user defined)
		uninstallSkills(agent);
	}

	@SuppressWarnings({"unchecked", "checkstyle:npathcomplexity"})
	private static Iterable<? extends Skill> getAllSkills(Agent agent, boolean inReverseOrder) {
		// Use reflection to ignore the "private/protected" access right.
		try {
			// Get the registered skills
			if (agentSkillField == null) {
				final Field field = Agent.class.getDeclaredField("skills"); //$NON-NLS-1$
				if (!field.isAccessible()) {
					field.setAccessible(true);
				}
				agentSkillField = field;
			}
			final Map<?, ClearableReference<Skill>> skills = (Map<?, ClearableReference<Skill>>) agentSkillField.get(agent);
			if (skills != null) {
				final List<BuiltinSkill> builtinSkills = new ArrayList<>();
				final Set<Skill> otherSkills = new TreeSet<>((first, second) -> {
					if (first == second) {
						return 0;
					}
					return Integer.compare(System.identityHashCode(first), System.identityHashCode(second));
				});
				final Comparator<BuiltinSkill> comparator = inReverseOrder ? REVERSE_ORDER_COMPARATOR : ORDER_COMPARATOR;
				for (final ClearableReference<Skill> skillReference : skills.values()) {
					final Skill skill = skillReference.get();
					if (skill instanceof BuiltinSkill) {
						ListUtil.add(builtinSkills, comparator, (BuiltinSkill) skill, true, false);
					} else if (skill != null) {
						otherSkills.add(skill);
					}
				}
				if (otherSkills.isEmpty()) {
					return builtinSkills;
				}
				final MultiCollection<Skill> allSkills = new MultiCollection<>();
				if (inReverseOrder) {
					allSkills.addCollection(otherSkills);
					allSkills.addCollection(builtinSkills);
				} else {
					allSkills.addCollection(builtinSkills);
					allSkills.addCollection(otherSkills);
				}
				return allSkills;
			}
			return Collections.emptyList();
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private static void installSkills(Agent agent) {
		// Only the BICs will be install at startup
		try {
			// Use reflection to ignore the "private/protected" access right.
			if (skillInstallationMethod == null) {
				final Method method = Skill.class.getDeclaredMethod("install"); //$NON-NLS-1$
				if (!method.isAccessible()) {
					method.setAccessible(true);
				}
				skillInstallationMethod = method;
			}
			for (final Skill s : getAllSkills(agent, false)) {
				skillInstallationMethod.invoke(s);
			}
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

	}

	private static void uninstallSkills(Agent agent) {
		try {
			// Use reflection to ignore the "private/protected" access right.
			if (skillUninstallationMethod == null) {
				final Method method = Skill.class.getDeclaredMethod("uninstall"); //$NON-NLS-1$
				if (!method.isAccessible()) {
					method.setAccessible(true);
				}
				skillUninstallationMethod = method;
			}
			for (final Skill s : getAllSkills(agent, true)) {
				skillUninstallationMethod.invoke(s);
			}
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Comparator for sorting the skills in order of installation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class OrderComparator implements Comparator<BuiltinSkill> {

		/** Constructor.
		 */
		OrderComparator() {
			//
		}

		@Override
		public int compare(BuiltinSkill outsideElement, BuiltinSkill insideElement) {
			// insideElement is always the data already in the list, according to the documentation of ListUtil#add()
			return Integer.compare(outsideElement.getInstallationOrder(), insideElement.getInstallationOrder());
		}

	}

	/**
	 * Comparator for sorting the skills in reverse order of installation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ReverseOrderComparator implements Comparator<BuiltinSkill> {

		/** Constructor.
		 */
		ReverseOrderComparator() {
			//
		}

		@Override
		public int compare(BuiltinSkill outsideElement, BuiltinSkill insideElement) {
			// insideElement is always the data already in the list, according to the documentation of ListUtil#add()
			return Integer.compare(insideElement.getInstallationOrder(), outsideElement.getInstallationOrder());
		}

	}

}
