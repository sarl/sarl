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

package io.janusproject.modules.hazelcast;

import java.net.InetAddress;
import java.net.URI;

import com.google.common.util.concurrent.Service;
import com.google.inject.AbstractModule;
import com.google.inject.Provides;
import com.google.inject.Singleton;
import com.google.inject.multibindings.Multibinder;
import com.google.inject.name.Named;
import com.hazelcast.config.Config;
import com.hazelcast.config.MulticastConfig;
import com.hazelcast.config.NetworkConfig;
import com.hazelcast.config.SerializerConfig;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;
import io.janusproject.JanusConfig;
import io.janusproject.kernel.services.hazelcast.HazelcastDistributedDataStructureService;
import io.janusproject.kernel.services.hazelcast.HazelcastInfrastructureService;
import io.janusproject.kernel.services.hazelcast.HazelcastKernelDiscoveryService;
import io.janusproject.modules.StandardJanusPlatformModule;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkUtil;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.SpaceID;

/**
 * The Core Janus Module configures the minimum requirements for Janus to run properly. If you need a standard configuration use
 * {@link StandardJanusPlatformModule}.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class HazelcastModule extends AbstractModule {

	@Override
	protected void configure() {
		SerializerConfig sc = new SerializerConfig();
		sc.setTypeClass(SpaceID.class);
		sc.setImplementation(new SpaceIDSerializer());

		SerializerConfig sc2 = new SerializerConfig();
		sc2.setTypeClass(Address.class);
		sc2.setImplementation(new AddressSerializer());

		Config hazelcastConfig = new Config();
		hazelcastConfig.getSerializationConfig().addSerializerConfig(sc);
		hazelcastConfig.getSerializationConfig().addSerializerConfig(sc2);

		bind(Config.class).toInstance(hazelcastConfig);

		// Ensure the system property for the hazelcast logger factory
		String factory = JanusConfig.getSystemProperty(JanusConfig.HAZELCAST_LOGGER_FACTORY_NAME,
				JanusConfig.HAZELCAST_LOGGER_FACTORY_VALUE);
		assert (factory != null && !factory.isEmpty());
		System.setProperty(JanusConfig.HAZELCAST_LOGGER_FACTORY_NAME, factory);

		// Bind the infrastructure service dedicated to Hazelcast
		Multibinder<Service> serviceSetBinder = Multibinder.newSetBinder(binder(), Service.class);
		serviceSetBinder.addBinding().to(HazelcastInfrastructureService.class).in(Singleton.class);

		// Bind the services based on Hazelcast
		bind(DistributedDataStructureService.class).to(HazelcastDistributedDataStructureService.class).in(Singleton.class);
		bind(KernelDiscoveryService.class).to(HazelcastKernelDiscoveryService.class).in(Singleton.class);
	}

	@Provides
	@Singleton
	private static HazelcastInstance createHazelcastInstance(Config config, LogService logService,
			@Named(JanusConfig.PUB_URI) URI uri) {
		assert (uri != null);
		boolean enableMulticast = true;
		InetAddress adr = null;

		if (JanusConfig.getSystemPropertyAsBoolean(JanusConfig.OFFLINE, false)) {
			adr = NetworkUtil.getLoopbackAddress();
			enableMulticast = false;
		} else {
			try {
				adr = NetworkUtil.toInetAddress(uri);
			} catch (Throwable e) {
				logService.error("INVALID_PUB_URI", e); //$NON-NLS-1$
			}
		}

		// Ensure to have an Inet address
		if (adr == null) {
			if (!NetworkUtil.isConnectedHost()) {
				adr = NetworkUtil.getLoopbackAddress();
			} else {
				adr = NetworkUtil.getPrimaryAddress();
			}
		}

		assert (adr != null);
		String hostname = adr.getHostAddress();
		config.setProperty("hazelcast.local.localAddress", hostname); //$NON-NLS-1$

		NetworkConfig networkConfig = config.getNetworkConfig();
		MulticastConfig multicastConfig = networkConfig.getJoin().getMulticastConfig();

		// The following block of code is fixing the issue hazelcast/hazelcast#2594.
		if (enableMulticast) {
			if (adr.isLoopbackAddress()) {
				multicastConfig.setLoopbackModeEnabled(true);
			}
		} else {
			multicastConfig.setEnabled(false);
		}

		HazelcastKernelLoggerFactory.setLogService(logService);

		return Hazelcast.newHazelcastInstance(config);
	}

}
