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
package io.janusproject.kernel.services.jdk.kerneldiscovery;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;

import java.net.InetAddress;
import java.net.URI;
import java.util.Collection;
import java.util.concurrent.Executor;

import com.google.common.util.concurrent.Service.Listener;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.testutils.AbstractDependentServiceTest;
import io.janusproject.testutils.StartServiceForTest;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@StartServiceForTest(startAfterSetUp = true)
@SuppressWarnings("all")
public final class StandardKernelDiscoveryServiceTest extends AbstractDependentServiceTest<StandardKernelDiscoveryService> {

	@Nullable
	private URI uri;

	@Nullable
	private Listener networkServiceListener;

	@Mock
	private NetworkService network;

	@Mock
	private ExecutorService executor;

	public StandardKernelDiscoveryServiceTest() {
		super(KernelDiscoveryService.class);
	}

	@Before
	public void setUp() {
		InetAddress adr = NetworkUtil.getLoopbackAddress();
		this.uri = NetworkUtil.toURI(adr, -1);
		assertNotNull("The URI of the localhost is not set.", this.uri);
		doReturn(this.uri).when(this.network).getURI();
		doAnswer(new Answer<Void>() {
			@Override
			public Void answer(InvocationOnMock invocation) throws Throwable {
				StandardKernelDiscoveryServiceTest.this.networkServiceListener = invocation.getArgument(0);
				return null;
			}
		}).when(this.network).addListener(Matchers.any(Listener.class), Matchers.any(Executor.class));
		this.service.postConstruction(this.network, this.executor);
	}

	/**
	 * Simulate the network connection.
	 */
	protected void connectNetwork() {
		assertNotNull("The kernel discovery service is expected to register a listener on the NetworkService.",
				this.networkServiceListener);
		this.networkServiceListener.starting();
		this.networkServiceListener.running();
	}

	@Override
	public StandardKernelDiscoveryService newService() {
		return new StandardKernelDiscoveryService();
	}

	@Override
	public void getServiceDependencies() {
		assertContains(this.service.getServiceDependencies(), ExecutorService.class);
	}

	@Override
	public void getServiceWeakDependencies() {
		assertContains(this.service.getServiceWeakDependencies());
	}

	@Test
	public void getCurrentKernel() {
		connectNetwork();
		assertSame(this.uri, this.service.getCurrentKernel());
	}

	@Test
	public void getKernels() {
		connectNetwork();
		Collection<URI> kernels = this.service.getKernels();
		assertNotNull(kernels);
		assertContains(kernels, this.uri);
	}

}
