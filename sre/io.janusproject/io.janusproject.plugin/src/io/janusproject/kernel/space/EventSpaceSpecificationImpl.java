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

import com.google.inject.Inject;
import com.google.inject.Injector;
import io.janusproject.services.distributeddata.DistributedDataStructureService;

import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.SpaceID;

/**
 * Default implementation of the specification of an event space.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class EventSpaceSpecificationImpl implements EventSpaceSpecification {

	@Inject
	private Injector injector;

	@Override
	public EventSpace create(SpaceID id, Object... params) {
		EventSpaceImpl space = new EventSpaceImpl(id, this.injector.getInstance(DistributedDataStructureService.class));
		this.injector.injectMembers(space);
		return space;
	}

}
