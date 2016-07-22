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

import com.google.common.util.concurrent.Service;

/**
 * This service is dependent on other services.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface DependentService extends Service {

	/**
	 * Replies the service interface implemented by this service.
	 *
	 * @return the service interface.
	 */
	Class<? extends Service> getServiceType();

	/**
	 * Replies the services that must be launched BEFORE launching this service, and that must be stop AFTER stopping this
	 * service.
	 *
	 * <p>
	 * If one of the dependencies is a {@link AsyncStateService}, this service will way until
	 * {@link AsyncStateService#isReadyForOtherServices()} is true.
	 *
	 * @return the dependencies.
	 * @see #getServiceWeakDependencies()
	 */
	Collection<Class<? extends Service>> getServiceDependencies();

	/**
	 * Replies the services that must be launched BEFORE launching this service, and that must be stop AFTER stopping this
	 * service.
	 *
	 * <p>
	 * Even if one of the dependencies is a {@link AsyncStateService}, this service will never wait until
	 * {@link AsyncStateService#isReadyForOtherServices()} is true.
	 *
	 * @return the dependencies.
	 * @see #getServiceDependencies()
	 */
	Collection<Class<? extends Service>> getServiceWeakDependencies();

}
