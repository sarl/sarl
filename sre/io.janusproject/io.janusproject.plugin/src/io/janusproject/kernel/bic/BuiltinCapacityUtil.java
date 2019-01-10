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

import io.sarl.core.ExternalContextAccess;
import io.sarl.core.InnerContextAccess;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.SREutils;
import io.sarl.lang.util.SynchronizedIterable;

/**
 * Utilities that are dedicated to the built-in capacities.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class BuiltinCapacityUtil {

	private BuiltinCapacityUtil() {
		//
	}

	/**
	 * Replies the contexts in which the agent is located.
	 *
	 * @param agent the agent for which the contexts must be retrieved.
	 * @return the contexts of the agents.
	 * @throws Exception - when it is not possible to retrieve the contexts.
	 */
	public static SynchronizedIterable<AgentContext> getContextsOf(Agent agent) throws Exception {
		final ExternalContextAccess skill = SREutils.getInternalSkill(agent, ExternalContextAccess.class);
		assert skill != null;
		return skill.getAllContexts();
	}

	/**
	 * Replies the inner context of the agent, if it was created.
	 *
	 * @param agent the agent for which the inner context must be retreived.
	 * @return the inner context, or <code>null</code>.
	 * @throws Exception - when it is not possible to retreive the inner context.
	 */
	public static AgentContext getContextIn(Agent agent) throws Exception {
		final InnerContextAccess skill = SREutils.getInternalSkill(agent, InnerContextAccess.class);

		if (skill instanceof InnerContextSkill) {
			final InnerContextSkill janusSkill = (InnerContextSkill) skill;
			if (janusSkill.hasInnerContext()) {
				return janusSkill.getInnerContext();
			}
			return null;
		}
		if (skill == null) {
			return null;
		}
		return skill.getInnerContext();
	}

}
