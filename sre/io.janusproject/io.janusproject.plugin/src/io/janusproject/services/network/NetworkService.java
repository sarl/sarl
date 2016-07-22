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

import io.janusproject.services.DependentService;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;

/**
 * This class enables the Janus kernel to send messages other the network.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface NetworkService extends DependentService {

	/**
	 * Publish a data over the network.
	 *
	 * @param scope - scope of the published data.
	 * @param data - data to propage over the network.
	 * @throws Exception - when the event cannot be published.
	 */
	void publish(Scope<?> scope, Event data) throws Exception;

	/**
	 * Connect this instance of kernel to the given peer over the network and for the given space.
	 *
	 * <p>
	 * If the network service is not yet ready for connecting the given URI, this URI <strong>MUST</strong> be bufferized until
	 * the network service has been fully started.
	 *
	 * @param peerUri - the URI of the remote kernel to be connected to.
	 * @param space - the identifier of the space to be connected to.
	 * @param listener - listener on the receiving of the events.
	 * @throws Exception - when the peer cannot be connected.
	 */
	void connectToRemoteSpaces(URI peerUri, SpaceID space, NetworkEventReceivingListener listener) throws Exception;

	/**
	 * Disconnect this peer from the given peer for the given space.
	 *
	 * @param peer - the URI of the remote kernel to be disconnected from.
	 * @param space - the identifier of the space to be disconnected from.
	 * @throws Exception - when the peer cannot be disconnected.
	 */
	void disconnectFromRemoteSpace(URI peer, SpaceID space) throws Exception;

	/**
	 * Disconnect this peer from the given peer for all the spaces.
	 *
	 * @param peer - the URI of the remote kernel to be disconnected from.
	 * @throws Exception - when the peer cannot be disconnected.
	 */
	void disconnectPeer(URI peer) throws Exception;

	/**
	 * Replies the URI used by this network interface.
	 *
	 * @return the URI, or <code>null</code> if the network is not started.
	 */
	URI getURI();

	/**
	 * Add a listener on the events in this service and related to the network.
	 *
	 * @param listener - the listener on the nerwork events.
	 */
	void addNetworkServiceListener(NetworkServiceListener listener);

	/**
	 * Remove a listener on the events in this service and related to the network.
	 *
	 * @param listener - the listener on the nerwork events.
	 */
	void removeNetworkServiceListener(NetworkServiceListener listener);

	/**
	 * Listener on events that are received from the network.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	interface NetworkEventReceivingListener extends EventListener {

		/**
		 * Invoked when a data is received from a distant peer.
		 *
		 * @param space - the id of the space.
		 * @param scope - the scope of the received data.
		 * @param event - the event with the data inside.
		 */
		void eventReceived(SpaceID space, Scope<?> scope, Event event);

	}

}
