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

package io.janusproject.services;

import java.util.Collection;
import java.util.Collections;

import com.google.common.util.concurrent.AbstractService;
import com.google.common.util.concurrent.Service;

/**
 * This service has a priority to be launch/stop.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractDependentService extends AbstractService implements DependentService {

	/**
	 * Construct.
	 */
	public AbstractDependentService() {
		//
	}

	@Override
	public Collection<Class<? extends Service>> getServiceDependencies() {
		return Collections.emptyList();
	}

	@Override
	public Collection<Class<? extends Service>> getServiceWeakDependencies() {
		return Collections.emptyList();
	}

}
