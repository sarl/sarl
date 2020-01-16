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

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

/**
 * Class defined for marking a skill as builtin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class BuiltinSkill extends Skill {

	/** Constructor.
	 */
	protected BuiltinSkill() {
		super();
	}

	/** Constructor.
	 *
	 * @param owner the owner.
	 */
	protected BuiltinSkill(Agent owner) {
		super(owner);
	}

	/** Replies the installation order of the given skill.
	 *
	 * @param skill the skill.
	 * @return the installation order.
	 * @deprecated since 0.10
	 */
	@Deprecated
	protected static int installationOrder(Skill skill) {
		final int len = StandardBuiltinCapacitiesProvider.SKILL_INSTALLATION_ORDER.length;
		if (skill instanceof BuiltinSkill) {
			for (int i = 0; i < len; ++i) {
				final Class<? extends Skill> type = StandardBuiltinCapacitiesProvider.SKILL_INSTALLATION_ORDER[i];
				if (type.isInstance(skill)) {
					return i;
				}
			}
		}
		return len;
	}

	/** Replies the installation order of the builtin skill.
	 *
	 * @return the order.
	 * @deprecated since 0.10
	 */
	@Deprecated
	public abstract int getInstallationOrder();

}
