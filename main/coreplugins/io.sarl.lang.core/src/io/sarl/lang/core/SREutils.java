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

import java.util.Collection;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.annotation.PrivateAPI;

/** Utilities for accessing to the part of the SARL API that is dedicated to the
 * SARL run-time environments (SRE).
 *
 * <p>These utilities are provided for enabling the implementation of high-performance
 * SRE by giving storage area into the instances of SARL concepts.
 *
 * <p>The data that is set and get by the functions in this utility class are SRE-dependent.
 * The instance type of the data is known only by the SRE.
 *
 * <p>Note that any SRE-specific data stored into a SARL object has the following properties: <ul>
 * <li>it is transient, i.e. it is not serialized;</li>
 * <li>it is used in the implementation of the {@link Object#equals(Object)} function;</li>
 * <li>it is used in the implementation of the {@link Object#hashCode()} function;</li>
 * <li>it is used in the implementation of the {@link Object#toString()} function.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 * @privateapi
 */
@PrivateAPI
public final class SREutils {

	private SREutils() {
		//
	}

	/** Replies the data associated to the container by the SRE.
	 *
	 * @param <S> the type of the data.
	 * @param type the type of the data.
	 * @param container the container.
	 * @return the SRE-specific data.
	 */
	@Pure
	public static <S> S getSreSpecificData(SRESpecificDataContainer container, Class<S> type) {
		assert container != null;
		return container.$getSreSpecificData(type);
	}

	/** Change the data associated to the given container by the SRE.
	 *
	 * @param container the container.
	 * @param data the SRE-specific data.
	 */
	public static void setSreSpecificData(SRESpecificDataContainer container, Object data) {
		assert container != null;
		container.$setSreSpecificData(data);
	}

	/** Change the data associated to the given container by the SRE.
	 *
	 * @param <S> the type of the data.
	 * @param type the type of the data.
	 * @param container the container.
	 * @param data the SRE-specific data.
	 * @return the SRE-specific data that was associated to the container before associating data to it.
	 * @since 0.6
	 */
	public static <S> S setSreSpecificData(SRESpecificDataContainer container, S data, Class<S> type) {
		assert container != null;
		final S oldData = container.$getSreSpecificData(type);
		container.$setSreSpecificData(data);
		return oldData;
	}

	/** Replies the internal skill reference of a skill container.
	 *
	 * @param container the container.
	 * @param type the type of the capacity.
	 * @return the skill reference
	 * @since 0.6
	 */
	@Pure
	public static AtomicSkillReference getInternalSkillReference(AbstractSkillContainer container, Class<? extends Capacity> type) {
		return container.$getSkill(type);
	}

	/** Casts the internal skill reference of a skill container.
	 *
	 * @param <S> the type of the capacity.
	 * @param container the container.
	 * @param reference the reference to cast.
	 * @param type the type of the capacity.
	 * @return the skill reference
	 * @since 0.6
	 */
	@Pure
	public static <S extends Capacity> S castInternalSkillReference(AbstractSkillContainer container,
			AtomicSkillReference reference, Class<S> type) {
		return container.$castSkill(type, reference);
	}

	/** Set the internal skill of a skill container.
	 *
	 * @param container the container.
	 * @param skill the skill instance to attach to the container.
	 * @param capacities the list of implemented capacities. This array cannot be {@code null}.
	 * @return the reference to the skill.
	 * @since 0.10
	 */
	public static AtomicSkillReference setInternalSkill(AbstractSkillContainer container, Skill skill, Class<? extends Capacity>[] capacities) {
		assert capacities != null;
		return container.$setSkill(skill, false, capacities);
	}

	/** Set the internal skill of a skill container if the skill is not yet mapped.
	 *
	 * @param container the container.
	 * @param skill the skill instance to attach to the container.
	 * @param capacities the list of implemented capacities. This array cannot be {@code null}.
	 * @return the reference to the skill.
	 * @since 0.11
	 */
	public static AtomicSkillReference setInternalSkillIfAbsent(AbstractSkillContainer container, Skill skill, Class<? extends Capacity>[] capacities) {
		assert capacities != null;
		return container.$setSkill(skill, true, capacities);
	}

	/** Replies the internal skill of a skill container.
	 *
	 * @param <S> the type of the capacity.
	 * @param container the container.
	 * @param type the type of the capacity.
	 * @return the skill.
	 * @since 0.6
	 */
	@Pure
	public static <S extends Capacity> S getInternalSkill(AbstractSkillContainer container, Class<S> type) {
		return container.getSkill(type);
	}

	/** Replies the skill repository of the given container.
	 *
	 * <p>The replied repository is not protected against asynchronous accesses.
	 *
	 * @param container the container.
	 * @return the repository.
	 * @since 0.6
	 */
	public static ConcurrentMap<Class<? extends Capacity>, AtomicSkillReference> getSkillRepository(AbstractSkillContainer container) {
		return container.$getSkillRepository();
	}

	/** Change the dynamic skill provider of a skill container.
	 *
	 * @param container the container.
	 * @param provider the provider.
	 * @since 0.6
	 */
	public static void setDynamicSkillProvider(AbstractSkillContainer container, DynamicSkillProvider provider) {
		container.$setDynamicSkillProvider(provider);
	}

	/** Do the installation of the given skill.
	 *
	 * <p>This function invokes {@link Skill#install()} and nothing more.
	 *
	 * @param skill the skill to be installed.
	 * @since 0.6
	 * @see #doSkillUninstallationPreparation(Skill)
	 * @see #doSkillUninstallation(Skill)
	 */
	public static void doSkillInstallation(Skill skill) {
		skill.install();
	}

	/** Do the uninstallation preparation of the given skill.
	 *
	 * <p>This function invokes {@link Skill#prepareUninstallation()} and nothing more.
	 *
	 * @param skill the skill to be uninstalled.
	 * @since 0.11
	 * @see #doSkillInstallation(Skill)
	 * @see #doSkillUninstallation(Skill)
	 */
	public static void doSkillUninstallationPreparation(Skill skill) {
		skill.prepareUninstallation();
	}

	/** Do the uninstallation of the given skill.
	 *
	 * <p>This function invokes {@link Skill#uninstall()} and nothing more.
	 *
	 * @param skill the skill to be uninstalled.
	 * @since 0.11
	 * @see #doSkillInstallation(Skill)
	 * @see #doSkillUninstallationPreparation(Skill)
	 */
	public static void doSkillUninstallation(Skill skill) {
		skill.uninstall();
	}

	/** Do the guard evaluations of the behavior units.
	 *
	 * @param receiver is the object that receives the events.
	 * @param event is the event that causes the evaluation.
	 * @param behaviorsMethodsToExecute receives the behavior unit methods.
	 * @since 0.12
	 */
	public static void doEvaluateBehaviorGuards(IBehaviorGuardEvaluatorReceiver receiver, Object event, Collection<Runnable> behaviorsMethodsToExecute) {
		receiver.$evaluateBehaviorGuards(event, behaviorsMethodsToExecute);
	}

	/** Retrieve the events' types that are supported by the given receiver.
	 *
	 * @param receiver is the object that receives the events.
	 * @param events is the set to fill out with the supported events' types.
	 * @since 0.12
	 */
	public static void doGetSupportedEvents(IBehaviorGuardEvaluatorReceiver receiver, Set<Class<? extends Event>> events) {
		receiver.$getSupportedEvents(events);
	}

	/** Replies if the given event is supported by the given receiver.
	 *
	 * @param receiver is the object that receives the events.
	 * @param event is the event to test.
	 * @return {@code true} if the event is supported; {@code false} otherwise.
	 * @since 0.12
	 */
	public static boolean doIsSupportedEvent(IBehaviorGuardEvaluatorReceiver receiver, Class<? extends Event> event) {
		return receiver.$isSupportedEvent(event);
	}

	/** Do the installation of the given behavior.
	 *
	 * <p>This function invokes {@link Behavior#install()} and nothing more.
	 *
	 * @param behavior the behavior to be installed.
	 * @since 0.12
	 * @see #doBehaviorUninstallation(Behavior)
	 */
	public static void doBehaviorInstallation(Behavior behavior) {
		behavior.install();
	}

	/** Do the uninstallation of the given behavior.
	 *
	 * <p>This function invokes {@link Behavior#uninstall()} and nothing more.
	 *
	 * @param behavior the behavior to be uninstalled.
	 * @since 0.12
	 * @see #doBehaviorInstallation(Behavior)
	 */
	public static void doBehaviorUninstallation(Behavior behavior) {
		behavior.uninstall();
	}

	/** Provide an agent with a callback function for the skill installation.
	 *
	 * @param agent is the agent to equip.
	 * @param callback is the callback to install.
	 * @since 0.12
	 */
	public static void setSkillInstallationCallback(Agent agent, Procedure2<Agent, Skill> callback) {
		agent.setSkillCallback(callback);
	}

}
