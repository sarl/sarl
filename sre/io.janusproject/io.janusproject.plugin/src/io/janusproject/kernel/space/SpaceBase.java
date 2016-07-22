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

package io.janusproject.kernel.space;

import io.sarl.lang.core.SpaceID;

/**
 * Abstract implementation of a space.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class SpaceBase implements DistributedSpace {

	private final SpaceID id;

	/**
	 * Constructs a space.
	 *
	 * @param id - identifier of the space.
	 */
	public SpaceBase(SpaceID id) {
		this.id = id;
	}

	@Override
	public final SpaceID getID() {
		return this.id;
	}

}
