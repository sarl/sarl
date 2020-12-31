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

/**
 * Thrown when the owner of an {@link AgentTrait} cannot be found.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class OwnerNotFoundException extends RuntimeException {

	private static final long serialVersionUID = -6286153666879425064L;

	private final AgentTrait trait;

	/**
	 * Creates a new instance of the exception.
	 *
	 * @param trait the source of the exception.
	 */
	public OwnerNotFoundException(AgentTrait trait) {
		this.trait = trait;
	}

	/**
	 * The agent trait at the source of the exception.
	 *
	 * @return the agent trait.
	 */
	@Pure
	public AgentTrait getAgentTrait() {
		return this.trait;
	}

}
