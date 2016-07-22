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

package io.janusproject.services.kerneldiscovery;

import java.net.URI;
import java.util.Collection;

import io.janusproject.services.DependentService;

/**
 * This class enables the Janus kernel to be distributed other a network.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface KernelDiscoveryService extends DependentService {

	/**
	 * Replies the URI of the current kernel.
	 *
	 * @return the uri of the current kernel.
	 */
	URI getCurrentKernel();

	/**
	 * Replies the URIs of the kernels, including the current kernels.
	 *
	 * <p>
	 * The replies collection must not be synchronized on the service.
	 *
	 * @return the uri of the kernels.
	 */
	Collection<URI> getKernels();

	/**
	 * Add a listener on the events in this service and related to the kernel discovery.
	 *
	 * @param listener - listener on discovery events.
	 */
	void addKernelDiscoveryServiceListener(KernelDiscoveryServiceListener listener);

	/**
	 * Remove a listener on the events in this service and related to the kernel discovery.
	 *
	 * @param listener - listener on discovery events.
	 */
	void removeKernelDiscoveryServiceListener(KernelDiscoveryServiceListener listener);

}
