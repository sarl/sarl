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

package io.sarl.lang.core;

import java.util.concurrent.ConcurrentMap;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.annotation.PrivateAPI;
import io.sarl.lang.core.Skill.UninstallationStage;
import io.sarl.lang.util.ClearableReference;

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

	/** Replies the internal skill reference of an agent.
	 *
	 * @param agent the agent.
	 * @param type the type of the capacity.
	 * @return the skill reference
	 * @throws UnimplementedCapacityException if the agent has not a skill for the given capacity.
	 * @since 0.6
	 */
	@Pure
	public static ClearableReference<Skill> getInternalSkillReference(Agent agent, Class<? extends Capacity> type) {
		return agent.$getSkill(type);
	}

	/** Casts the internal skill reference of an agent.
	 *
	 * @param <S> the type of the capacity.
	 * @param agent the agent.
	 * @param reference the reference to cast.
	 * @param type the type of the capacity.
	 * @return the skill reference
	 * @throws UnimplementedCapacityException if the agent has not a skill for the given capacity.
	 * @since 0.6
	 */
	@Pure
	public static <S extends Capacity> S castInternalSkillReference(Agent agent,
			ClearableReference<Skill> reference, Class<S> type) {
		return agent.$castSkill(type, reference);
	}

	/** Set the internal skill of an agent.
	 *
	 * @param agent the agent.
	 * @param skill the skill instance to attach to the agent.
	 * @param capacities the list of implemented capacities. This array cannot be {@code null}.
	 * @return the reference to the skill.
	 * @since 0.10
	 */
	public static ClearableReference<Skill> setInternalSkill(Agent agent, Skill skill, Class<? extends Capacity>[] capacities) {
		assert capacities != null;
		return agent.$setSkill(skill, capacities);
	}

	/** Replies the internal skill of an agent.
	 *
	 * @param <S> the type of the capacity.
	 * @param agent the agent.
	 * @param type the type of the capacity.
	 * @return the skill.
	 * @throws UnimplementedCapacityException if the agent has not a skill for the given capacity.
	 * @since 0.6
	 */
	@Pure
	public static <S extends Capacity> S getInternalSkill(Agent agent, Class<S> type) {
		return agent.getSkill(type);
	}

	/** Create the mapping between the capacity and the skill.
	 *
	 * <p>This function does not call neither {@link Skill#install()} nor {@link AgentTrait#setOwner(Agent)}.
	 *
	 * @param agent the agent.
	 * @param capacity the capacity to map.
	 * @param skill the skill to map.
	 * @return the created mapping, never {@code null}.
	 * @since 0.6
	 * @see #createSkillMappingGetOld(Agent, Class, Skill)
	 * @deprecated since 0.10, no replacement
	 */
	@Deprecated
	public static ClearableReference<Skill> createSkillMapping(Agent agent, Class<? extends Capacity> capacity, Skill skill) {
		return agent.$mapCapacityGetNew(capacity, skill);
	}

	/** Create the mapping between the capacity and the skill.
	 *
	 * <p>This function does not call neither {@link Skill#install()} nor {@link AgentTrait#setOwner(Agent)}.
	 *
	 * @param agent the agent.
	 * @param capacity the capacity to map.
	 * @param skill the skill to map.
	 * @return the mapping that is defined before the one created by the call to this function, or {@code null} if
	 *     there is no previous mapping.
	 * @since 0.6
	 * @see #createSkillMapping(Agent, Class, Skill)
	 * @deprecated since 0.10, no replacement
	 */
	@Deprecated
	public static ClearableReference<Skill> createSkillMappingGetOld(Agent agent, Class<? extends Capacity> capacity, Skill skill) {
		return agent.$mapCapacityGetOld(capacity, skill);
	}

	/** Replies the skill repository of the given agent.
	 *
	 * <p>The replied repository is not protected against asynchronous accesses.
	 *
	 * @param agent the agent.
	 * @return the repository.
	 * @since 0.6
	 */
	public static ConcurrentMap<Class<? extends Capacity>, ClearableReference<Skill>> getSkillRepository(Agent agent) {
		return agent.$getSkillRepository();
	}

	/** Change the dynamic skill provider of an agent.
	 *
	 * @param agent the agent.
	 * @param provider the provider.
	 * @since 0.6
	 */
	public static void setDynamicSkillProvider(Agent agent, DynamicSkillProvider provider) {
		agent.$setDynamicSkillProvider(provider);
	}

	/** Do the installation of the given skill.
	 *
	 * <p>This function invokes {@link Skill#install()} and nothing more.
	 *
	 * @param skill the skill to be installed.
	 * @since 0.6
	 */
	public static void doSkillInstallation(Skill skill) {
		skill.install();
	}

	/** Do the uninstallation of the given skill.
	 *
	 * <p>This function invokes {@link Skill#uninstall(UninstallationStage)} and nothing more.
	 *
	 * @param skill the skill to be uninstalled.
	 * @param stage the uninstallation stage, never {@code null}.
	 * @since 0.6
	 */
	public static void doSkillUninstallation(Skill skill, UninstallationStage stage) {
		skill.uninstall(stage);
	}

}
