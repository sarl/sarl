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
package io.janusproject.tests.services;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import com.google.common.collect.LinkedListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.util.concurrent.Service;
import com.google.common.util.concurrent.Service.State;
import io.janusproject.services.IServiceManager;
import io.janusproject.services.Services;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.services.spawn.SpawnService;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class ServicesTest extends AbstractJanusTest {

	@Nullable
	private Multimap<State, Service> services;

	@Nullable
	private IServiceManager serviceManager;

	@Nullable
	private List<Service> encounteredServices;

	@Nullable
	private ContextSpaceService s1;

	@Nullable
	private ExecutorService s2;

	@Nullable
	private KernelDiscoveryService s3;

	@Nullable
	private LogService s4;

	@Nullable
	private NetworkService s5;

	@Nullable
	private SpawnService s6;

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Before
	public void setUp() {
		this.encounteredServices = new LinkedList<>();
		this.services = LinkedListMultimap.create();

		this.s1 = Mockito.mock(ContextSpaceService.class, "s1"); //$NON-NLS-1$
		this.s2 = Mockito.mock(ExecutorService.class, "s2"); //$NON-NLS-1$
		this.s3 = Mockito.mock(KernelDiscoveryService.class, "s3"); //$NON-NLS-1$
		this.s4 = Mockito.mock(LogService.class, "s4"); //$NON-NLS-1$
		this.s5 = Mockito.mock(NetworkService.class, "s5"); //$NON-NLS-1$
		this.s6 = Mockito.mock(SpawnService.class, "s6"); //$NON-NLS-1$

		Mockito.when(this.s1.startAsync()).thenAnswer(new EncounteredServiceAnswer(this.s1));
		Mockito.when(this.s1.stopAsync()).thenAnswer(new EncounteredServiceAnswer(this.s1));
		Mockito.when(this.s1.getServiceType()).thenReturn((Class) ContextSpaceService.class);
		Mockito.when(this.s1.getServiceDependencies())
				.thenReturn(Arrays.<Class<? extends Service>>asList(NetworkService.class, KernelDiscoveryService.class));
		Mockito.when(this.s1.getServiceWeakDependencies()).thenReturn(Arrays.<Class<? extends Service>>asList());
		this.services.put(State.NEW, this.s1);

		Mockito.when(this.s2.startAsync()).thenAnswer(new EncounteredServiceAnswer(this.s2));
		Mockito.when(this.s2.stopAsync()).thenAnswer(new EncounteredServiceAnswer(this.s2));
		Mockito.when(this.s2.getServiceType()).thenReturn((Class) ExecutorService.class);
		Mockito.when(this.s2.getServiceDependencies()).thenReturn(Arrays.<Class<? extends Service>>asList());
		Mockito.when(this.s2.getServiceWeakDependencies()).thenReturn(Arrays.<Class<? extends Service>>asList());
		this.services.put(State.NEW, this.s2);

		Mockito.when(this.s3.startAsync()).thenAnswer(new EncounteredServiceAnswer(this.s3));
		Mockito.when(this.s3.stopAsync()).thenAnswer(new EncounteredServiceAnswer(this.s3));
		Mockito.when(this.s3.getServiceType()).thenReturn((Class) KernelDiscoveryService.class);
		Mockito.when(this.s3.getServiceDependencies())
				.thenReturn(Arrays.<Class<? extends Service>>asList(LogService.class, ExecutorService.class));
		Mockito.when(this.s3.getServiceWeakDependencies()).thenReturn(Arrays.<Class<? extends Service>>asList());
		this.services.put(State.NEW, this.s3);

		Mockito.when(this.s4.startAsync()).thenAnswer(new EncounteredServiceAnswer(this.s4));
		Mockito.when(this.s4.stopAsync()).thenAnswer(new EncounteredServiceAnswer(this.s4));
		Mockito.when(this.s4.getServiceType()).thenReturn((Class) LogService.class);
		Mockito.when(this.s4.getServiceDependencies()).thenReturn(Arrays.<Class<? extends Service>>asList());
		Mockito.when(this.s4.getServiceWeakDependencies()).thenReturn(Arrays.<Class<? extends Service>>asList());
		this.services.put(State.NEW, this.s4);

		Mockito.when(this.s5.startAsync()).thenAnswer(new EncounteredServiceAnswer(this.s5));
		Mockito.when(this.s5.stopAsync()).thenAnswer(new EncounteredServiceAnswer(this.s5));
		Mockito.when(this.s5.getServiceType()).thenReturn((Class) NetworkService.class);
		Mockito.when(this.s5.getServiceDependencies())
				.thenReturn(Arrays.<Class<? extends Service>>asList(LogService.class, ExecutorService.class));
		Mockito.when(this.s5.getServiceWeakDependencies())
				.thenReturn(Arrays.<Class<? extends Service>>asList(KernelDiscoveryService.class));
		this.services.put(State.NEW, this.s5);

		Mockito.when(this.s6.startAsync()).thenAnswer(new EncounteredServiceAnswer(this.s6));
		Mockito.when(this.s6.stopAsync()).thenAnswer(new EncounteredServiceAnswer(this.s6));
		Mockito.when(this.s6.getServiceType()).thenReturn((Class) SpawnService.class);
		Mockito.when(this.s6.getServiceDependencies())
				.thenReturn(Arrays.<Class<? extends Service>>asList(ContextSpaceService.class));
		Mockito.when(this.s6.getServiceWeakDependencies()).thenReturn(Arrays.<Class<? extends Service>>asList());
		this.services.put(State.NEW, this.s6);

		for (int i = 0; i < 10; ++i) {
			Service serv = Mockito.mock(Service.class);
			Mockito.when(serv.startAsync()).thenAnswer(new EncounteredServiceAnswer(serv));
			Mockito.when(serv.stopAsync()).thenAnswer(new EncounteredServiceAnswer(serv));
			this.services.put(State.NEW, serv);
		}
		this.serviceManager = Mockito.mock(IServiceManager.class);
		Mockito.when(this.serviceManager.servicesByState()).thenReturn(this.services);
	}

	@Test
	public void startServices() {
		Services.startServices(this.serviceManager);
		// s1 : ContextSpace -> Network, KernelDiscovery
		// s2 : Executor ->
		// s3 : KernelDiscovery -> Log, Executor
		// s4 : Log ->
		// s5 : Network -> Log, Executor, KernelDiscovery
		// s6 : Spawn -> ContextSpace

		// Executor(s2)
		// Log(s4)
		// KernelDiscovery(s3)
		// Network(s5)
		// ContextSpace(s1)
		// Spawn(s6)
		assertEquals(16, this.encounteredServices.size());

		assertSame(this.s2, this.encounteredServices.get(0));
		assertSame(this.s4, this.encounteredServices.get(1));
		assertSame(this.s3, this.encounteredServices.get(2));
		assertSame(this.s5, this.encounteredServices.get(3));
		assertSame(this.s1, this.encounteredServices.get(4));
		assertSame(this.s6, this.encounteredServices.get(5));
	}

	@Test
	public void stopServices() {
		Services.stopServices(this.serviceManager);
		// s1 : ContextSpace -> Network, KernelDiscovery
		// s2 : Executor ->
		// s3 : KernelDiscovery -> Log, Executor
		// s4 : Log ->
		// s5 : Network -> Log, Executor, KernelDiscovery
		// s6 : Spawn -> ContextSpace

		// Executor(s2)
		// Log(s4)
		// KernelDiscovery(s3)
		// Network(s5)
		// ContextSpace(s1)
		// Spawn(s6)
		assertEquals(16, this.encounteredServices.size());

		assertSame(this.s6, this.encounteredServices.get(10));
		assertSame(this.s1, this.encounteredServices.get(11));
		assertSame(this.s5, this.encounteredServices.get(12));
		assertSame(this.s3, this.encounteredServices.get(13));
		assertSame(this.s4, this.encounteredServices.get(14));
		assertSame(this.s2, this.encounteredServices.get(15));
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class EncounteredServiceAnswer implements Answer<Service> {

		private final Service service;

		/**
		 * @param service
		 */
		EncounteredServiceAnswer(Service service) {
			this.service = service;
		}

		/**
		 * {@inheritDoc}
		 */
		@SuppressWarnings("synthetic-access")
		@Override
		public Service answer(InvocationOnMock invocation) throws Throwable {
			ServicesTest.this.encounteredServices.add(this.service);
			return this.service;
		}

	}

}
