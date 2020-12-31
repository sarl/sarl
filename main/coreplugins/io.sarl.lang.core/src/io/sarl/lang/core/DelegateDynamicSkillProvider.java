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

/** This provides an implementation of a dynamic skill provider that delegates
 * to the first known dynamic skill provider that is able to provide a skill.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class DelegateDynamicSkillProvider implements DynamicSkillProvider {

	private final DynamicSkillProvider[] delegates;

	/** Constructor.
	 *
	 * @param delegates the lsit of the providers to delegate to.
	 */
	public DelegateDynamicSkillProvider(DynamicSkillProvider[] delegates) {
		assert delegates != null;
		this.delegates = delegates;
	}

	@Pure
	@Override
	public Skill createSkill(Class<? extends Capacity> capacity) {
		for (final DynamicSkillProvider provider : this.delegates) {
			final Skill skill = provider.createSkill(capacity);
			if (skill != null) {
				return skill;
			}
		}
		return null;
	}

	@Pure
	@Override
	public boolean isSkillProviding(Class<? extends Capacity> capacity) {
		for (final DynamicSkillProvider provider : this.delegates) {
			if (provider.isSkillProviding(capacity)) {
				return true;
			}
		}
		return false;
	}

}
