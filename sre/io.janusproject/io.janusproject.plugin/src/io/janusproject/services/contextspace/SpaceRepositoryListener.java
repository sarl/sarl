/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.services.contextspace;

import java.util.EventListener;

import io.sarl.lang.core.Space;

/**
 * Listener on events related to the spaces.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface SpaceRepositoryListener extends EventListener {

	/**
	 * Invoked when the space is added.
	 *
	 * @param space - reference to the created space.
	 * @param isLocalCreation - indicates if the creation of the space was initiated on the current kernel.
	 */
	void spaceCreated(Space space, boolean isLocalCreation);

	/**
	 * Invoked when the space is destroyed.
	 *
	 * @param space - reference to the destroyed space.
	 * @param isLocalDestruction - indicates if the destruction of the space was initiated on the current kernel.
	 */
	void spaceDestroyed(Space space, boolean isLocalDestruction);

}
