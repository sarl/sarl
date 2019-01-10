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

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.util.ClearableReference;



/** This interface represents a provider of built-in capacities.
 * The built-in capacities are assumed to be provided by
 * the runtime platform.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@FunctionalInterface
public interface DynamicSkillProvider {

	/** Install the given skill into the given agent.
	 *
	 * <p>If the given type of skill depends on another builtin capacity, the corresponding skill is also installed.
	 *
	 * @param agent the agent for which the built-in capacities must be retrieved.
	 * @param capacity the type of the capacity to retrieve.
	 * @return the skill that is installed into the agent.
	 */
	@Pure
	ClearableReference<Skill> installSkill(Agent agent, Class<? extends Capacity> capacity);

}
