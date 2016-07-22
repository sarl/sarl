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

import com.google.common.collect.Multimap;
import com.google.common.util.concurrent.Service;
import com.google.common.util.concurrent.Service.State;

/**
 * Manager of services.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface IServiceManager {

	/**
	 * Replies the services by state.
	 *
	 * @return the services.
	 */
	Multimap<State, Service> servicesByState();

	/**
	 * Wait for all the services are started.
	 */
	void awaitHealthy();

	/**
	 * Wait for all the services are stopped.
	 */
	void awaitStopped();

}
