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

package io.janusproject.kernel.services.jdk.spawn;

import java.text.MessageFormat;

import io.sarl.lang.core.Agent;

/**
 * This exception is thrown when an agent cannot be spawned.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class CannotSpawnException extends RuntimeException {

	private static final long serialVersionUID = -380402400888610762L;

	/** Constructor.
	 * @param agentClazz
	 *            - the type of the agent to spawn.
	 * @param cause
	 *            - the cause of the exception.
	 */
	public CannotSpawnException(Class<? extends Agent> agentClazz, Throwable cause) {
		super(MessageFormat.format(Messages.StandardSpawnService_3, agentClazz,
				(cause == null) ? null : cause.getLocalizedMessage()), cause);
	}

}
