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
import java.util.EventListener;

/**
 * Listener on events related to the kernel discovery service.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface KernelDiscoveryServiceListener extends EventListener {

	/**
	 * Invoked when a remote kernel has been discovered.
	 *
	 * @param peerURI - URI of the kernel was was discovered.
	 */
	void kernelDiscovered(URI peerURI);

	/**
	 * Invoked when a remote kernel has been disconnected.
	 *
	 * @param peerURI - URI of the kernel that was disconnected.
	 */
	void kernelDisconnected(URI peerURI);

}
