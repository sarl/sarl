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

import org.eclipse.xtext.xbase.lib.Pure;

/** This interface represents a provider of built-in capacities.
 * The built-in capacities are assumed to be provided by
 * the runtime platform.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public interface DynamicSkillProvider {

	/** A provider that provides nothing.
	 *
	 * @since 0.11
	 */
	DynamicSkillProvider EMPTY_PROVIDER = new DynamicSkillProvider() {

		@Override
		public Skill createSkill(Class<? extends Capacity> capacity) {
			return null;
		}

		@Override
		public boolean isSkillProviding(Class<? extends Capacity> capacity) {
			return false;
		}

	};

	/** Create the built-in skill that corresponds to the given capacity into the given container.
	 *
	 * @param capacity the type of the capacity to retrieve.
	 * @return the skill that should be installed into the sill container or {@code null} if none.
	 */
	@Pure
	Skill createSkill(Class<? extends Capacity> capacity);

	/** Replies if this provider could provide the built-in skill that corresponds to the given capacity.
	 *
	 * @param capacity the type of the capacity to retrieve.
	 * @return {@code true} if an instance of skill could be provided for the given capacity.
	 * @since 0.11
	 */
	@Pure
	boolean isSkillProviding(Class<? extends Capacity> capacity);

}
