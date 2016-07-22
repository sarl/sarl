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

package io.janusproject.kernel.services.hazelcast;

import java.net.InetSocketAddress;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.UUID;

import com.google.common.util.concurrent.Service;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.google.inject.name.Named;
import com.hazelcast.core.EntryEvent;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IMap;
import com.hazelcast.core.MemberAttributeEvent;
import com.hazelcast.core.MembershipEvent;
import com.hazelcast.core.MembershipListener;
import com.hazelcast.map.listener.EntryAddedListener;
import com.hazelcast.map.listener.EntryEvictedListener;
import com.hazelcast.map.listener.EntryRemovedListener;
import io.janusproject.JanusConfig;
import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.AsyncStateService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryServiceListener;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.util.ListenerCollection;
import io.janusproject.util.TwoStepConstruction;

/**
 * Service that is providing the access to the repository of the Janus kernels.
 *
 * <p>
 * It uses the Hazelcast library for discovering the nodes over the network.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
@TwoStepConstruction
public class HazelcastKernelDiscoveryService extends AbstractDependentService
		implements KernelDiscoveryService, AsyncStateService {

	private final UUID janusID;

	private URI currentPubURI;

	private URI currentHzURI;

	private IMap<URI, URI> kernels;

	private boolean isReady;

	private String hzRegId1;

	private String hzRegId2;

	private NetworkService network;

	private LogService logger;

	private ExecutorService executorService;

	private final ListenerCollection<KernelDiscoveryServiceListener> listeners = new ListenerCollection<>();

	private HazelcastInstance hazelcastInstance;

	private final HazelcastListener hzListener = new HazelcastListener();

	private NetworkStartListener networkStartListener = new NetworkStartListener();

	/**
	 * Constructs a <code>KernelRepositoryService</code>.
	 *
	 * @param janusID - injected identifier of the Janus context.
	 */
	@Inject
	public HazelcastKernelDiscoveryService(@Named(JanusConfig.DEFAULT_CONTEXT_ID_NAME) UUID janusID) {
		this.janusID = janusID;
	}

	@Override
	public boolean isReadyForOtherServices() {
		return isRunning() && this.isReady;
	}

	@Override
	public final Class<? extends Service> getServiceType() {
		return KernelDiscoveryService.class;
	}

	@Override
	public Collection<Class<? extends Service>> getServiceDependencies() {
		return Arrays.<Class<? extends Service>>asList(LogService.class, ExecutorService.class);
	}

	/**
	 * Do the post initialization.
	 *
	 * @param hazelcastInstance - instance of the Hazelcast service that permits to shared data among the network.
	 * @param networkService - network service to be linked to.
	 * @param executorService - execution service to use.
	 * @param logger - logging service to use.
	 */
	@Inject
	void postConstruction(HazelcastInstance hazelcastInstance, NetworkService networkService, ExecutorService executorService,
			LogService logger) {
		this.executorService = executorService;
		this.hazelcastInstance = hazelcastInstance;
		this.logger = logger;
		this.network = networkService;
		this.kernels = hazelcastInstance.getMap(this.janusID.toString() + "-kernels"); //$NON-NLS-1$
		this.network.addListener(this.networkStartListener, this.executorService.getExecutorService());
	}

	@Override
	public URI getCurrentKernel() {
		return this.currentPubURI;
	}

	@Override
	public synchronized Collection<URI> getKernels() {
		return new ArrayList<>(this.kernels.values());
	}

	@Override
	public void addKernelDiscoveryServiceListener(KernelDiscoveryServiceListener listener) {
		this.listeners.add(KernelDiscoveryServiceListener.class, listener);
	}

	@Override
	public void removeKernelDiscoveryServiceListener(KernelDiscoveryServiceListener listener) {
		this.listeners.remove(KernelDiscoveryServiceListener.class, listener);
	}

	/**
	 * Notifies the listeners about the discovering of a kernel.
	 *
	 * @param uri - URI of the discovered kernel.
	 */
	protected void fireKernelDiscovered(URI uri) {
		this.logger.info(HazelcastKernelDiscoveryService.class, "KERNEL_DISCOVERY", uri, getCurrentKernel()); //$NON-NLS-1$
		for (KernelDiscoveryServiceListener listener : this.listeners.getListeners(KernelDiscoveryServiceListener.class)) {
			listener.kernelDiscovered(uri);
		}
	}

	/**
	 * Notifies the listeners about the killing of a kernel.
	 *
	 * @param uri - URI of the disconnected kernel.
	 */
	protected void fireKernelDisconnected(URI uri) {
		this.logger.info(HazelcastKernelDiscoveryService.class, "KERNEL_DISCONNECTION", uri, getCurrentKernel()); //$NON-NLS-1$
		for (KernelDiscoveryServiceListener listener : this.listeners.getListeners(KernelDiscoveryServiceListener.class)) {
			listener.kernelDisconnected(uri);
		}
	}

	@Override
	protected synchronized void doStart() {
		this.hzRegId1 = this.kernels.addEntryListener(this.hzListener, true);
		this.hzRegId2 = this.hazelcastInstance.getCluster().addMembershipListener(this.hzListener);
		notifyStarted();
	}

	@Override
	protected synchronized void doStop() {
		this.isReady = false;
		if (this.hzRegId1 != null) {
			this.kernels.removeEntryListener(this.hzRegId1);
		}
		if (this.hzRegId2 != null) {
			this.hazelcastInstance.getClientService().removeClientListener(this.hzRegId2);
		}
		// Remove the current kernel from the kernel's list
		if (this.currentHzURI != null) {
			this.kernels.remove(this.currentHzURI);
		}
		// For avoiding memory leaks due to reference loop
		this.network = null;
		notifyStopped();
	}

	/**
	 * Listener on Hazelcast events.
	 *
	 * @author $Author: srodriguez$
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class HazelcastListener implements MembershipListener, EntryAddedListener<URI, URI>, EntryRemovedListener<URI, URI>,
			EntryEvictedListener<URI, URI> {

		/**
		 * Construct.
		 */
		HazelcastListener() {
			//
		}

		@Override
		public void memberAdded(MembershipEvent membershipEvent) {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void memberRemoved(MembershipEvent membershipEvent) {
			InetSocketAddress socketAddress = membershipEvent.getMember().getSocketAddress();
			if (socketAddress != null) {
				URI u = NetworkUtil.toURI(socketAddress);
				if (u != null) {
					synchronized (HazelcastKernelDiscoveryService.this) {
						HazelcastKernelDiscoveryService.this.kernels.remove(u);
					}
				}
			}
		}

		@Override
		public void memberAttributeChanged(MemberAttributeEvent memberAttributeEvent) {
			//
		}

		@Override
		public void entryAdded(EntryEvent<URI, URI> event) {
			URI newPeer = event.getValue();
			assert (newPeer != null);
			if (!newPeer.equals(getCurrentKernel())) {
				fireKernelDiscovered(newPeer);
			}
		}

		@Override
		public void entryRemoved(EntryEvent<URI, URI> event) {
			fireDisconnected(event);
		}

		@Override
		public void entryEvicted(EntryEvent<URI, URI> event) {
			fireDisconnected(event);
		}

		private void fireDisconnected(EntryEvent<URI, URI> event) {
			URI oldPeer = event.getValue();
			assert (oldPeer != null);
			if (!oldPeer.equals(getCurrentKernel())) {
				fireKernelDisconnected(oldPeer);
			}
		}

	}

	/**
	 * Listener on network events.
	 *
	 * @author $Author: srodriguez$
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class NetworkStartListener extends Listener {

		/**
		 * Construct.
		 */
		NetworkStartListener() {
			//
		}

		@SuppressWarnings("synthetic-access")
		@Override
		public void running() {
			// Outside the synchronizing statement to avoid deadlock
			URI uri = HazelcastKernelDiscoveryService.this.network.getURI();
			if (HazelcastKernelDiscoveryService.this.currentPubURI == null) {
				synchronized (HazelcastKernelDiscoveryService.this) {
					HazelcastKernelDiscoveryService.this.currentPubURI = uri;
					HazelcastKernelDiscoveryService.this.currentHzURI = NetworkUtil
							.toURI(HazelcastKernelDiscoveryService.this.hazelcastInstance.getCluster().getLocalMember()
									.getSocketAddress());
				}

				// Notify about the discovery of the already launched kernels
				for (URI remotePublicKernel : getKernels()) {
					fireKernelDiscovered(remotePublicKernel);
				}

				synchronized (HazelcastKernelDiscoveryService.this) {
					HazelcastKernelDiscoveryService.this.isReady = true;
					HazelcastKernelDiscoveryService.this.kernels.putIfAbsent(HazelcastKernelDiscoveryService.this.currentHzURI,
							HazelcastKernelDiscoveryService.this.currentPubURI);
				}
			}
		}
	}

}
