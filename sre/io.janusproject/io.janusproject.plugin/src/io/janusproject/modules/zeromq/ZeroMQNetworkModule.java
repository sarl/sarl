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

package io.janusproject.modules.zeromq;

import java.net.URI;

import com.google.common.util.concurrent.Service;
import com.google.inject.AbstractModule;
import com.google.inject.Key;
import com.google.inject.Singleton;
import com.google.inject.multibindings.Multibinder;
import com.google.inject.name.Names;
import io.janusproject.JanusConfig;
import io.janusproject.kernel.services.zeromq.ZeroMQNetworkService;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.EventSerializer;
import io.janusproject.services.network.NetworkService;

/**
 * Module that provides the network layer based on the ZeroMQ library.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ZeroMQNetworkModule extends AbstractModule {

	@Override
	protected void configure() {
		requireBinding(Key.get(URI.class, Names.named(JanusConfig.PUB_URI)));

		requireBinding(LogService.class);
		requireBinding(KernelDiscoveryService.class);
		requireBinding(ContextSpaceService.class);
		requireBinding(ExecutorService.class);
		requireBinding(EventSerializer.class);

		bind(NetworkService.class).to(ZeroMQNetworkService.class).in(Singleton.class);

		// Complete the binding for: Set<Service>
		// (This set is given to the service manager to launch the services).
		Multibinder<Service> serviceSetBinder = Multibinder.newSetBinder(binder(), Service.class);
		serviceSetBinder.addBinding().to(ZeroMQNetworkService.class);
	}

}
