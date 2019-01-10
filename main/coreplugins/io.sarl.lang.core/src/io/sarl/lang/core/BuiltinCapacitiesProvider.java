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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.xtext.xbase.lib.Procedures.Procedure2;
import org.eclipse.xtext.xbase.lib.Pure;



/** This interface represents a provider of built-in capacities.
 * The built-in capacities are assumed to be provided by
 * the runtime platform.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @deprecated Replacement is {@link DynamicSkillProvider} since 0.6
 */
@Deprecated
@FunctionalInterface
public interface BuiltinCapacitiesProvider {

	/** Replies the built-in capacities for the given agent.
	 *
	 * @param agent the agent for which the built-in capacities must be retreived.
	 * @return the built-in capacities for the given agent.
	 */
	@Pure
	default Map<Class<? extends Capacity>, Skill> getBuiltinCapacities(Agent agent) {
		final Map<Class<? extends Capacity>, Skill> result = new HashMap<>();
		builtinCapacities(agent, (capacity, skill) -> {
			result.put(capacity, skill);
		});
		return result;
	}

	/** Applies the given callback on the built-in capacities for the given agent.
	 *
	 * @param agent the agent for which the built-in capacities must be retreived.
	 * @param skillMappingCallback the callback function for mapping a capacity and a skill.
	 *     The first argument is the agent. The second
	 *     argument is the builtin capacity. The third argument is the skill instance.
	 */
	void builtinCapacities(Agent agent, Procedure2<? super Class<? extends Capacity>, ? super Skill> skillMappingCallback);

}
