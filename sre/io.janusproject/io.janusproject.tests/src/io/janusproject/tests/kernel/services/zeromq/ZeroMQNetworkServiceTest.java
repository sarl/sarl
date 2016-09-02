/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.janusproject.tests.kernel.services.zeromq;

import java.net.InetAddress;
import java.net.URI;

import io.janusproject.kernel.services.zeromq.ZeroMQNetworkService;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.EventSerializer;
import io.janusproject.services.network.NetworkService;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.tests.testutils.AbstractDependentServiceTest;
import io.janusproject.tests.testutils.AvoidServiceStartForTest;
import io.janusproject.tests.testutils.StartServiceForTest;
import org.junit.Before;
import org.mockito.Mock;

import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@StartServiceForTest(startAfterSetUp = true, createAfterSetUp = true)
@SuppressWarnings("all")
public class ZeroMQNetworkServiceTest extends AbstractDependentServiceTest<ZeroMQNetworkService> {

	@Nullable
	private URI uri;

	@Mock
	private LogService logService;

	@Mock
	private LogService logger;

	@Mock
	private KernelDiscoveryService kernelService;

	@Mock
	private ContextSpaceService spaceService;

	@Mock
	private ExecutorService executorService;

	@Mock
	private EventSerializer serializer;

	/**
	 */
	public ZeroMQNetworkServiceTest() {
		super(NetworkService.class);
	}

	@Before
	public void setUp() {
		InetAddress adr = NetworkUtil.getLoopbackAddress();
		this.uri = NetworkUtil.toURI(adr, -1);
	}

	@Override
	public ZeroMQNetworkService newService() {
		return new ZeroMQNetworkService(this.uri);
	}

	@AvoidServiceStartForTest
	@Override
	public void getServiceDependencies() {
		assertContains(this.service.getServiceDependencies(), LogService.class, ExecutorService.class);
	}

	@AvoidServiceStartForTest
	@Override
	public void getServiceWeakDependencies() {
		assertContains(this.service.getServiceWeakDependencies(), KernelDiscoveryService.class);
	}

}
