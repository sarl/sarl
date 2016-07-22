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

package io.janusproject.services.network;

import java.net.URI;
import java.util.EventListener;

import io.sarl.lang.core.SpaceID;

/**
 * Listener on events related to the network service.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface NetworkServiceListener extends EventListener {

	/**
	 * Invoked when a remote peer has been discovered.
	 *
	 * @param peerURI - URI of the remote kernel.
	 */
	void peerDiscovered(URI peerURI);

	/**
	 * Invoked when a remote peer has been disconnected.
	 *
	 * @param peerURI - URI of the remote kernel.
	 */
	void peerDisconnected(URI peerURI);

	/**
	 * Invoked when a remote peer has been disconnected for a particular space.
	 *
	 * @param peerURI - URI of the remote kernel is connected to the local kernel.
	 * @param space - the identifier of the space for which a connection was opened.
	 */
	void peerDisconnected(URI peerURI, SpaceID space);

	/**
	 * Invoked when a connection to a peer was opened for a particular space.
	 *
	 * @param peerURI - URI of the remote kernel is connected to the local kernel.
	 * @param space - the identifier of the space for which a connection was opened.
	 */
	void peerConnected(URI peerURI, SpaceID space);

}
