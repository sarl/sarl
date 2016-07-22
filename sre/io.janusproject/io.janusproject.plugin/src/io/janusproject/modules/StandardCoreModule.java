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

package io.janusproject.modules;

import com.google.common.util.concurrent.Service;
import com.google.inject.AbstractModule;
import com.google.inject.Singleton;
import com.google.inject.multibindings.Multibinder;
import io.janusproject.kernel.services.arakhne.ArakhneLocaleLogService;
import io.janusproject.kernel.services.jdk.contextspace.StandardContextSpaceService;
import io.janusproject.kernel.services.jdk.spawn.StandardSpawnService;
import io.janusproject.modules.executors.JdkExecutorModule;
import io.janusproject.modules.kernel.MandatoryKernelModule;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.services.spawn.SpawnService;

/**
 * The Core Janus Module configures the minimum requirements for Janus to run properly. The network-based modules are skipped in
 * this StandardCoreModule. See {@link StandardJanusPlatformModule} for the configuration of the network-based modules
 *
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardCoreModule extends AbstractModule {

	@Override
	protected void configure() {
		bind(LogService.class).to(ArakhneLocaleLogService.class).in(Singleton.class);
		bind(ContextSpaceService.class).to(StandardContextSpaceService.class).in(Singleton.class);
		bind(SpawnService.class).to(StandardSpawnService.class).in(Singleton.class);

		install(new JdkExecutorModule());

		// Install the elements for the Janus kernel
		install(new MandatoryKernelModule());

		// Check if all the services are binded
		requireBinding(DistributedDataStructureService.class);
		requireBinding(KernelDiscoveryService.class);
		requireBinding(ExecutorService.class);
		requireBinding(ContextSpaceService.class);
		requireBinding(LogService.class);
		requireBinding(NetworkService.class);
		requireBinding(SpawnService.class);

		// Create a binder for: Set<Service>
		// (This set is given to the service manager to launch the services).
		Multibinder<Service> serviceSetBinder = Multibinder.newSetBinder(binder(), Service.class);
		serviceSetBinder.addBinding().to(LogService.class);
		serviceSetBinder.addBinding().to(ExecutorService.class);
		serviceSetBinder.addBinding().to(ContextSpaceService.class);
		serviceSetBinder.addBinding().to(KernelDiscoveryService.class);
		serviceSetBinder.addBinding().to(SpawnService.class);
		serviceSetBinder.addBinding().to(DistributedDataStructureService.class);
	}

}
