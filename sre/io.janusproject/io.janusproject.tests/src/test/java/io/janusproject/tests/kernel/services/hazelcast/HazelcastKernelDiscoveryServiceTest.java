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
package io.janusproject.tests.kernel.services.hazelcast;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Method;
import java.net.InetSocketAddress;
import java.net.URI;
import java.util.Collection;
import java.util.UUID;

import com.google.common.util.concurrent.Service.Listener;
import com.hazelcast.core.Cluster;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.core.IMap;
import com.hazelcast.core.Member;
import io.janusproject.kernel.services.hazelcast.HazelcastKernelDiscoveryService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.tests.testutils.AbstractDependentServiceTest;
import io.janusproject.tests.testutils.HzMapMock;
import io.janusproject.tests.testutils.StartServiceForTest;
import io.janusproject.util.TwoStepConstruction;
import javassist.Modifier;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Mockito;
import org.mockito.internal.verification.Times;

import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@StartServiceForTest(createAfterSetUp = true)
@SuppressWarnings("all")
public class HazelcastKernelDiscoveryServiceTest extends AbstractDependentServiceTest<HazelcastKernelDiscoveryService> {

	@Nullable
	private URI kernelURI;

	@Nullable
	private URI hazelcastURI;

	@Nullable
	private UUID contextId;

	@Nullable
	private IMap<Object, Object> kernels;

	@Nullable
	private HazelcastInstance hzInstance;

	@Nullable
	private NetworkService networkService;

	@Nullable
	private ExecutorService executorService;

	@Nullable
	private LogService logger;

	/**
	 */
	public HazelcastKernelDiscoveryServiceTest() {
		super(KernelDiscoveryService.class);
	}

	@Override
	public HazelcastKernelDiscoveryService newService() throws Exception {
		HazelcastKernelDiscoveryService serv = new HazelcastKernelDiscoveryService(this.contextId);
		this.reflect.invoke(serv, "postConstruction", this.hzInstance, this.networkService, this.executorService, this.logger);
		return serv;
	}

	@Before
	public void setUp() throws Exception {
		this.hazelcastURI = NetworkUtil.toURI("tcp://123.124.125.126:5023"); //$NON-NLS-1$
		this.kernelURI = NetworkUtil.toURI("tcp://123.124.125.126:34567"); //$NON-NLS-1$
		this.contextId = UUID.randomUUID();
		this.kernels = new HzMapMock<>();
		this.hzInstance = Mockito.mock(HazelcastInstance.class);
		{
			Mockito.when(this.hzInstance.getMap(ArgumentMatchers.any())).thenReturn(this.kernels);
			Cluster clusterMock = Mockito.mock(Cluster.class);
			Mockito.when(this.hzInstance.getCluster()).thenReturn(clusterMock);
			Member memberMock = Mockito.mock(Member.class);
			Mockito.when(clusterMock.getLocalMember()).thenReturn(memberMock);
			InetSocketAddress adr = NetworkUtil.toInetSocketAddress(this.hazelcastURI);
			Mockito.when(memberMock.getSocketAddress()).thenReturn(adr);
		}
		this.networkService = Mockito.mock(NetworkService.class);
		Mockito.when(this.networkService.getURI()).thenReturn(this.kernelURI);
		this.executorService = Mockito.mock(ExecutorService.class);
		this.logger = Mockito.mock(LogService.class);
	}

	@Override
	public void getServiceDependencies() {
		assertContains(this.service.getServiceDependencies(), LogService.class, ExecutorService.class);
	}

	@Override
	public void getServiceWeakDependencies() {
		assertContains(this.service.getServiceWeakDependencies());
	}

	@Test
	public void postConstruction() {
		ArgumentCaptor<Listener> argument1 = ArgumentCaptor.forClass(Listener.class);
		ArgumentCaptor<java.util.concurrent.ExecutorService> argument2 = ArgumentCaptor
				.forClass(java.util.concurrent.ExecutorService.class);
		Mockito.verify(this.networkService, new Times(1)).addListener(argument1.capture(), argument2.capture());
	}

	@Test
	public void getCurrentKernel_nonetworknotification() {
		assertNull(this.service.getCurrentKernel());
	}

	@Test
	public void getKernels_nonetworknotification() {
		assertTrue(this.service.getKernels().isEmpty());
	}

	@Test
	public void getCurrentKernel_networknotification() {
		ArgumentCaptor<Listener> argument1 = ArgumentCaptor.forClass(Listener.class);
		ArgumentCaptor<java.util.concurrent.ExecutorService> argument2 = ArgumentCaptor
				.forClass(java.util.concurrent.ExecutorService.class);
		Mockito.verify(this.networkService, new Times(1)).addListener(argument1.capture(), argument2.capture());
		Listener listener = argument1.getValue();
		listener.running();
		//
		assertEquals(this.kernelURI, this.service.getCurrentKernel());
	}

	@Test
	public void getKernels_networknotification() {
		ArgumentCaptor<Listener> argument1 = ArgumentCaptor.forClass(Listener.class);
		ArgumentCaptor<java.util.concurrent.ExecutorService> argument2 = ArgumentCaptor
				.forClass(java.util.concurrent.ExecutorService.class);
		Mockito.verify(this.networkService, new Times(1)).addListener(argument1.capture(), argument2.capture());
		Listener listener = argument1.getValue();
		listener.running();
		//
		Collection<URI> c = this.service.getKernels();
		assertNotNull(c);
		assertEquals(1, c.size());
		assertTrue(c.contains(this.kernelURI));
	}

	@Test
	public void twoStepConstruction() throws Exception {
		TwoStepConstruction annotation = HazelcastKernelDiscoveryService.class.getAnnotation(TwoStepConstruction.class);
		assertNotNull(annotation);
		for (String name : annotation.names()) {
			for (Method method : HazelcastKernelDiscoveryService.class.getMethods()) {
				if (name.equals(method.getName())) {
					assertTrue(Modifier.isPackage(method.getModifiers()) || Modifier.isPublic(method.getModifiers()));
					break;
				}
			}
		}
	}

}
