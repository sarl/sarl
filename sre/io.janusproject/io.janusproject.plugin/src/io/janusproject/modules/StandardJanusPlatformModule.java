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

import com.google.inject.AbstractModule;
import io.janusproject.JanusConfig;
import io.janusproject.modules.eventserial.NetworkEventModule;
import io.janusproject.modules.hazelcast.HazelcastModule;
import io.janusproject.modules.kernel.LocalDistributedDataStructureServiceModule;
import io.janusproject.modules.kernel.LocalInfrastructureServiceModule;
import io.janusproject.modules.kernel.LocalKernelDiscoveryServiceModule;
import io.janusproject.modules.nonetwork.NoNetworkModule;
import io.janusproject.modules.zeromq.ZeroMQNetworkModule;
import org.arakhne.afc.vmutil.OperatingSystem;

/**
 * The module configures Janus to run using the standard core modules and a ZeroMQ based network.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardJanusPlatformModule extends AbstractModule {

	@Override
	protected void configure() {
		boolean isNetworkEnabled = !JanusConfig.getSystemPropertyAsBoolean(JanusConfig.OFFLINE, false)
				&& !OperatingSystem.ANDROID.isCurrentOS();

		// install the module for the Janus boot loader
		install(new BootModule());

		// Install the modules for network-based services, except NetworkService
		if (isNetworkEnabled) {
			install(new HazelcastModule());
		} else {
			install(new LocalInfrastructureServiceModule());
			install(new LocalDistributedDataStructureServiceModule());
			install(new LocalKernelDiscoveryServiceModule());
		}

		// Install the Janus core modules.
		install(new StandardCoreModule());

		// Install the NetworkService module.
		if (isNetworkEnabled) {
			install(new NetworkEventModule());
			install(new ZeroMQNetworkModule());
		} else {
			install(new NoNetworkModule());
		}
	}

}
