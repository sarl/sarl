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
import com.google.common.util.concurrent.ServiceManager;

/**
 * Implementation of a service manager based on the Google service manager.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class GoogleServiceManager implements IServiceManager {

	private final ServiceManager sm;

	/**
	 * @param sm - the Google service to use.
	 */
	public GoogleServiceManager(ServiceManager sm) {
		this.sm = sm;
	}

	/**
	 * @param services - the services to manager.
	 */
	public GoogleServiceManager(Iterable<? extends Service> services) {
		this.sm = new ServiceManager(services);
	}

	@Override
	public Multimap<State, Service> servicesByState() {
		return this.sm.servicesByState();
	}

	@Override
	public void awaitHealthy() {
		this.sm.awaitHealthy();
	}

	@Override
	public void awaitStopped() {
		this.sm.awaitStopped();
	}

}
