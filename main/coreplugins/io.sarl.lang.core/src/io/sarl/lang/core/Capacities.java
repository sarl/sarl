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

import java.lang.reflect.Constructor;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.bootstrap.SREClassLoader;

/** Utility functions related to the capacities.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public final class Capacities {

	/** Local thread variable that contains the caller of a capacity function.
	 */
	static final ThreadLocal<AgentTrait> CALLER = new ThreadLocal<>();

	private static final String CAPACITY_WRAPPER_NAME = "$" + Capacity.ContextAwareCapacityWrapper.class.getSimpleName(); //$NON-NLS-1$;

	private Capacities() {
		//
	}

	/** Replies the caller of the capacity functions.
	 *
	 * <p>The replied value has a meaning inside the skills' functions that
	 * are implemented the capacities' functions.
	 *
	 * @return the caller, or {@code null} if the caller is unknown (assuming that the caller is the agent itself).
	 */
	@Pure
	public static AgentTrait getCaller() {
		return CALLER.get();
	}

	/** Create a delegator for the given skill.
	 *
	 * <p>The delegator is wrapping the original skill in order to set the value of the caller
	 * that will be replied by {@link #getCaller()}. The associated caller is given as argument.
	 *
	 * <p>The delegator is an instance of an specific inner type, sub-type of {@link Capacity.ContextAwareCapacityWrapper},
	 * which is declared in the given {@code capacity}.
	 *
	 * <p>This function fails if the delegator instance cannot be created due to inner type not found, invalid constructor signature,
	 * run-time exception when creating the instance.
	 * This functions assumes that the name of the definition type is the same as {@link Capacity.ContextAwareCapacityWrapper},
	 * and this definition extends the delegator definition of the first super type of the {@code capacity}, and implements
	 * all the super types of the {@code capacity}. The expected constructor for this inner type has the same
	 * signature as the one of {@link Capacity.ContextAwareCapacityWrapper}.
	 *
	 * <p>The function {@link #createSkillDelegatorIfPossible(Skill, Class, AgentTrait)} is a similar function than this
	 * function, except that it does not fail when the delegator instance cannot be created. In this last case,
	 * the function {@link #createSkillDelegatorIfPossible(Skill, Class, AgentTrait)} reply the original skill itself.
	 *
	 * @param <C> the type of the capacity.
	 * @param originalSkill the skill to delegate to after ensure the capacity caller is correctly set.
	 * @param capacity the capacity that contains the definition of the delegator.
	 * @param capacityCaller the caller of the capacity functions.
	 * @return the delegator.
	 * @throws Exception if the delegator cannot be created.
	 * @see #createSkillDelegatorIfPossible(Skill, Class, AgentTrait)
	 */
	@Pure
	public static <C extends Capacity> C createSkillDelegator(Skill originalSkill, Class<C> capacity, AgentTrait capacityCaller)
			throws Exception {
		final String name = capacity.getName() + CAPACITY_WRAPPER_NAME;
		final Class<?> type = SREClassLoader.loadClass(name, true, Capacities.class.getClassLoader());
		final Constructor<?> cons = type.getDeclaredConstructor(capacity, AgentTrait.class);
		return capacity.cast(cons.newInstance(originalSkill, capacityCaller));
	}

	/** Create a delegator for the given skill when it is possible.
	 *
	 * <p>The delegator is wrapping the original skill in order to set the value of the caller
	 * that will be replied by {@link #getCaller()}. The associated caller is given as argument.
	 *
	 * <p>The delegator is an instance of an specific inner type, sub-type of {@link Capacity.ContextAwareCapacityWrapper},
	 * which is declared in the given {@code capacity}.
	 *
	 * <p>This functions assumes that the name of the definition type is the same as {@link Capacity.ContextAwareCapacityWrapper},
	 * and this definition extends the delegator definition of the first super type of the {@code capacity}, and implements
	 * all the super types of the {@code capacity}. The expected constructor for this inner type has the same
	 * signature as the one of {@link Capacity.ContextAwareCapacityWrapper}.
	 * If the delegator instance cannot be created due to to inner type not found, invalid constructor signature,
	 * run-time exception when creating the instance, this function replies the original skill.
	 *
	 * <p>The function {@link #createSkillDelegator(Skill, Class, AgentTrait)} is a similar function than this
	 * function, except that it fails when the delegator instance cannot be created.
	 *
	 * @param <C> the type of the capacity.
	 * @param originalSkill the skill to delegate to after ensure the capacity caller is correctly set.
	 * @param capacity the capacity that contains the definition of the delegator.
	 * @param capacityCaller the caller of the capacity functions.
	 * @return the delegator, or the original skill.
	 * @throws ClassCastException if the skill is not implementing the capacity.
	 * @see #createSkillDelegator(Skill, Class, AgentTrait)
	 */
	@Pure
	public static <C extends Capacity> C createSkillDelegatorIfPossible(Skill originalSkill, Class<C> capacity, AgentTrait capacityCaller)
			throws ClassCastException {
		try {
			return Capacities.createSkillDelegator(originalSkill, capacity, capacityCaller);
		} catch (Exception e) {
			return capacity.cast(originalSkill);
		}
	}

}
