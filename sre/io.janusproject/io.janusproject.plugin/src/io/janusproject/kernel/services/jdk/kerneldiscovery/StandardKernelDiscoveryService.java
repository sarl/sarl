/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject.kernel.services.jdk.kerneldiscovery;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;

import com.google.common.util.concurrent.Service;
import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.AsyncStateService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryServiceListener;
import io.janusproject.services.network.NetworkService;
import io.janusproject.util.TwoStepConstruction;

/**
 * Service that is providing the access to the repository of the Janus kernels.
 *
 * <p>This implementation is not able to discovered other kernels.
 *
 * <p>This service is thread-safe.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
@TwoStepConstruction
public class StandardKernelDiscoveryService extends AbstractDependentService
		implements KernelDiscoveryService, AsyncStateService {

	private URI localURI;

	private boolean isReady;

	private NetworkService network;

	/**
	 * Constructs a <code>KernelRepositoryService</code>.
	 */
	public StandardKernelDiscoveryService() {
		//
	}

	/** Replies the mutex for synchronizing this service.
	 *
	 * @return the mutex.
	 */
	protected final Object getServiceMutex() {
		return this;
	}

	/**
	 * Do the post initialization.
	 *
	 * @param networkService network service to be linked to.
	 * @param executorService execution service to use.
	 */
	@Inject
	void postConstruction(NetworkService networkService, ExecutorService executorService) {
		this.network = networkService;
		this.network.addListener(new NetworkStartListener(), executorService.getExecutorService());
	}

	@Override
	public Collection<Class<? extends Service>> getServiceDependencies() {
		return Arrays.<Class<? extends Service>>asList(ExecutorService.class);
	}

	@Override
	public boolean isReadyForOtherServices() {
		if (isRunning()) {
			synchronized (getServiceMutex()) {
				return this.isReady;
			}
		}
		return false;
	}

	@Override
	public final Class<? extends Service> getServiceType() {
		return KernelDiscoveryService.class;
	}

	@Override
	public URI getCurrentKernel() {
		synchronized (getServiceMutex()) {
			return this.localURI;
		}
	}

	@Override
	public Collection<URI> getKernels() {
		synchronized (getServiceMutex()) {
			return Collections.singleton(this.localURI);
		}
	}

	@Override
	public void addKernelDiscoveryServiceListener(KernelDiscoveryServiceListener listener) {
		//
	}

	@Override
	public void removeKernelDiscoveryServiceListener(KernelDiscoveryServiceListener listener) {
		//
	}

	@Override
	protected void doStart() {
		notifyStarted();
	}

	@Override
	protected void doStop() {
		synchronized (getServiceMutex()) {
			this.isReady = false;
		}
		notifyStopped();
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
			final URI uri = StandardKernelDiscoveryService.this.network.getURI();
			if (StandardKernelDiscoveryService.this.localURI == null) {
				synchronized (getServiceDependencies()) {
					StandardKernelDiscoveryService.this.localURI = uri;
					StandardKernelDiscoveryService.this.isReady = true;
				}
			}
		}
	}

}
