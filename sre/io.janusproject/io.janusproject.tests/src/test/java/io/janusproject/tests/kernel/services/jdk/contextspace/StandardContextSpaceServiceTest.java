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
package io.janusproject.tests.kernel.services.jdk.contextspace;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.logging.Logger;

import com.google.inject.Injector;
import com.google.inject.Provider;
import javassist.Modifier;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.internal.verification.Times;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.janusproject.kernel.services.jdk.contextspace.Context;
import io.janusproject.kernel.services.jdk.contextspace.ContextFactory;
import io.janusproject.kernel.services.jdk.contextspace.StandardContextSpaceService;
import io.janusproject.kernel.services.jdk.distributeddata.DMapView;
import io.janusproject.services.contextspace.ContextRepositoryListener;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.tests.testutils.AbstractDependentServiceTest;
import io.janusproject.tests.testutils.AvoidServiceStartForTest;
import io.janusproject.tests.testutils.StartServiceForTest;
import io.janusproject.util.TwoStepConstruction;

import io.sarl.core.OpenEventSpace;
import io.sarl.core.OpenEventSpaceSpecification;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.Nullable;
import io.sarl.util.concurrent.NoReadWriteLock;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@StartServiceForTest(startAfterSetUp = true)
public class StandardContextSpaceServiceTest extends AbstractDependentServiceTest<StandardContextSpaceService> {

	@Nullable
	private UUID contextId;
	@Nullable
	private SpaceID spaceId;
	@Nullable
	private DMap<Object, Object> innerData;

	@Nullable
	private DistributedDataStructureService dds;

	@Nullable
	private LogService logService;

	@Nullable
	private Logger logger;

	@Nullable
	private Injector injector;

	@Nullable
	private EventSpace defaultSpace;

	@Nullable
	private Context context;

	@Nullable
	private ContextFactory contextFactory;

	@Nullable
	private ContextRepositoryListener contextListener;

	/**
	 */
	public StandardContextSpaceServiceTest() {
		super(ContextSpaceService.class);
	}

	@Override
	public StandardContextSpaceService newService() {
		this.dds = mock(DistributedDataStructureService.class);
		this.injector = mock(Injector.class);
		when(this.injector.getProvider(any(Class.class))).thenAnswer((it) -> {
			Provider<?> provider = null;
			if (ReadWriteLock.class.equals(it.getArgument(0))) {
				provider = () -> NoReadWriteLock.SINGLETON;
			}
			return provider;
		});
		this.defaultSpace = mock(EventSpace.class);
		this.context = mock(Context.class);
		this.contextFactory = mock(ContextFactory.class);
		this.contextListener = mock(ContextRepositoryListener.class);

		this.contextId = UUID.randomUUID();
		this.innerData = new DMapView<>(UUID.randomUUID().toString(), new HashMap<>());		
		this.spaceId = new SpaceID(this.contextId, UUID.randomUUID(), OpenEventSpaceSpecification.class);
		when(this.context.postConstruction()).thenReturn(this.defaultSpace);
		when(this.context.getID()).thenReturn(this.contextId);
		when(this.defaultSpace.getSpaceID()).thenReturn(this.spaceId);
		when(this.contextFactory.newInstance(ArgumentMatchers.any(), ArgumentMatchers.any(),
				ArgumentMatchers.any(), ArgumentMatchers.any()))
				.thenAnswer(new Answer<Context>() {
					@Override
					public Context answer(InvocationOnMock invocation) throws Throwable {
						Context ctx = mock(Context.class);
						OpenEventSpace mock = mock(OpenEventSpace.class);
						SpaceID spaceId = new SpaceID((UUID) invocation.getArguments()[0], (UUID) invocation.getArguments()[1],
								OpenEventSpaceSpecification.class);
						when(ctx.getID()).thenReturn(spaceId.getContextID());
						when(reflect.invoke(ctx, "postConstruction")).thenReturn(mock);
						when(ctx.getDefaultSpace()).thenReturn(mock);
						when(mock.getSpaceID()).thenReturn(spaceId);
						return ctx;
					}
				});
		when(this.dds.getMap(ArgumentMatchers.any(), ArgumentMatchers.any())).thenReturn(this.innerData);
		when(this.dds.getMap(ArgumentMatchers.any())).thenReturn(this.innerData);
		this.logger = mock(Logger.class);
		this.logService = mock(LogService.class);
		when(this.logService.getKernelLogger()).thenReturn(this.logger);
		StandardContextSpaceService serv = new StandardContextSpaceService(this.contextId, this.dds, this.logService, this.injector);
		try {
			this.reflect.invoke(serv, "setContextFactory", this.contextFactory);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		serv.addContextRepositoryListener(this.contextListener);
		return serv;
	}

	@Override
	public void getServiceDependencies() {
		assertContains(this.service.getServiceDependencies(), DistributedDataStructureService.class, NetworkService.class,
				KernelDiscoveryService.class);
	}

	@Override
	public void getServiceWeakDependencies() {
		assertContains(this.service.getServiceWeakDependencies());
	}

	private AgentContext createOneTestingContext(UUID id) {
		return createOneTestingContext(id, UUID.randomUUID());
	}

	private AgentContext createOneTestingContext(UUID id, UUID spaceId) {
		return this.service.createContext(id, spaceId);
	}

	@Test
	public void twoStepConstruction() throws Exception {
		TwoStepConstruction annotation = StandardContextSpaceService.class.getAnnotation(TwoStepConstruction.class);
		assertNotNull(annotation);
		for (String name : annotation.names()) {
			for (Method method : StandardContextSpaceService.class.getMethods()) {
				if (name.equals(method.getName())) {
					assertTrue(Modifier.isPackage(method.getModifiers()) || Modifier.isPublic(method.getModifiers()));
					break;
				}
			}
		}
	}

	@Test
	public void getLock() {
		ReadWriteLock lock = this.service.getLock();
		assertNotNull(lock);
		assertSame(lock, this.service.getLock());
	}

	@Test
	public void getContextFactory() throws Exception {
		assertSame(this.contextFactory,
				this.reflect.invoke(this.service, "getContextFactory"));
	}

	@Test
	public void setContextFactory() throws Exception {
		assertSame(this.contextFactory,
				this.reflect.invoke(this.service, "getContextFactory"));
		this.reflect.invoke(this.service, "setContextFactory", null);
		assertSame(this.contextFactory,
				this.reflect.invoke(this.service, "getContextFactory"));
		ContextFactory mock = mock(ContextFactory.class);
		this.reflect.invoke(this.service, "setContextFactory", mock);
		assertSame(mock, this.reflect.invoke(this.service, "getContextFactory"));
	}

	@Test
	public void containsContext() {
		UUID id = UUID.randomUUID();
		assertFalse(this.service.containsContext(id));
		createOneTestingContext(id);
		assertTrue(this.service.containsContext(id));
	}

	@Test
	public void createContext() {
		UUID cid = UUID.randomUUID();
		UUID sid = UUID.randomUUID();
		AgentContext ctx = this.service.createContext(cid, sid);
		//
		assertNotNull(ctx);
		assertTrue(this.service.containsContext(cid));
		assertEquals(cid, ctx.getID());
		assertEquals(sid, ctx.getDefaultSpace().getSpaceID().getID());
		//
		ArgumentCaptor<AgentContext> argument = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(1)).contextCreated(argument.capture());
		assertSame(ctx, argument.getValue());
	}

	@Test
	public void ensureDefaultSpaceDefinition() throws Exception {
		SpaceID spaceId = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class);
		assertFalse(this.service.containsContext(spaceId.getContextID()));
		//
		// First call
		this.reflect.invoke(this.service, "ensureDefaultSpaceDefinition", spaceId);
		//
		assertTrue(this.service.containsContext(spaceId.getContextID()));
		//
		ArgumentCaptor<AgentContext> argument1 = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(1)).contextCreated(argument1.capture());
		assertSame(this.service.getContext(spaceId.getContextID()), argument1.getValue());
		//
		// Second call
		this.reflect.invoke(this.service, "ensureDefaultSpaceDefinition", spaceId);
		//
		assertTrue(this.service.containsContext(spaceId.getContextID()));
		//
		ArgumentCaptor<AgentContext> argument2 = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(1)).contextCreated(argument2.capture());
	}

	@Test
	public void removeDefaultSpaceDefinition() throws Exception {
		SpaceID spaceId = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class);
		assertFalse(this.service.containsContext(spaceId.getContextID()));
		//
		// First call
		this.reflect.invoke(this.service, "removeDefaultSpaceDefinition", spaceId);
		//
		assertFalse(this.service.containsContext(spaceId.getContextID()));
		verifyZeroInteractions(this.contextListener);
		//
		AgentContext ctx = createOneTestingContext(spaceId.getContextID(), spaceId.getID());
		assertTrue(this.service.containsContext(spaceId.getContextID()));
		//
		// Second call
		this.reflect.invoke(this.service, "removeDefaultSpaceDefinition", spaceId);
		//
		assertFalse(this.service.containsContext(spaceId.getContextID()));
		//
		ArgumentCaptor<AgentContext> argument2 = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(1)).contextDestroyed(argument2.capture());
		assertSame(ctx, argument2.getValue());
	}

	@Test
	public void isEmptyContextRepository() {
		assertTrue(this.service.isEmptyContextRepository());
		AgentContext ctx = createOneTestingContext(UUID.randomUUID());
		assertFalse(this.service.isEmptyContextRepository());
		this.service.removeContext(ctx);
		assertTrue(this.service.isEmptyContextRepository());
	}

	@Test
	public void getNumberOfContexts() {
		assertEquals(0, this.service.getNumberOfContexts());
		AgentContext ctx1 = createOneTestingContext(UUID.randomUUID());
		assertEquals(1, this.service.getNumberOfContexts());
		AgentContext ctx2 = createOneTestingContext(UUID.randomUUID());
		assertEquals(2, this.service.getNumberOfContexts());
		this.service.removeContext(ctx2);
		assertEquals(1, this.service.getNumberOfContexts());
		this.service.removeContext(ctx2);
		assertEquals(1, this.service.getNumberOfContexts());
		this.service.removeContext(ctx1);
		assertEquals(0, this.service.getNumberOfContexts());
	}

	@Test
	public void removeContextAgentContext() {
		AgentContext ctx1 = createOneTestingContext(UUID.randomUUID());
		AgentContext ctx2 = createOneTestingContext(UUID.randomUUID());
		//
		// First call
		this.service.removeContext(ctx2);
		//
		assertTrue(this.service.containsContext(ctx1.getID()));
		assertFalse(this.service.containsContext(ctx2.getID()));
		ArgumentCaptor<AgentContext> argument1 = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(1)).contextDestroyed(argument1.capture());
		assertSame(ctx2, argument1.getValue());
		//
		// Second call
		this.service.removeContext(ctx2);
		//
		assertTrue(this.service.containsContext(ctx1.getID()));
		assertFalse(this.service.containsContext(ctx2.getID()));
		ArgumentCaptor<AgentContext> argument3 = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(1)).contextDestroyed(argument3.capture());
	}

	@Test
	public void removeContextUUID() {
		AgentContext ctx1 = createOneTestingContext(UUID.randomUUID());
		AgentContext ctx2 = createOneTestingContext(UUID.randomUUID());
		//
		// First call
		this.service.removeContext(ctx2.getID());
		//
		assertTrue(this.service.containsContext(ctx1.getID()));
		assertFalse(this.service.containsContext(ctx2.getID()));
		ArgumentCaptor<AgentContext> argument1 = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(1)).contextDestroyed(argument1.capture());
		assertSame(ctx2, argument1.getValue());
		//
		// Second call
		this.service.removeContext(ctx2.getID());
		//
		assertTrue(this.service.containsContext(ctx1.getID()));
		assertFalse(this.service.containsContext(ctx2.getID()));
		ArgumentCaptor<AgentContext> argument3 = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(1)).contextDestroyed(argument3.capture());
	}

	@Test
	public void getContexts() {
		Collection<AgentContext> c;
		//
		c = this.service.getContexts();
		assertNotNull(c);
		assertTrue(c.isEmpty());
		//
		AgentContext ctx1 = createOneTestingContext(UUID.randomUUID());
		AgentContext ctx2 = createOneTestingContext(UUID.randomUUID());
		//
		c = this.service.getContexts();
		assertNotNull(c);
		assertFalse(c.isEmpty());
		assertEquals(2, c.size());
		assertTrue(c.contains(ctx1));
		assertTrue(c.contains(ctx2));
	}

	@Test
	public void getContextIDs() {
		Collection<UUID> c;
		//
		c = this.service.getContextIDs();
		assertNotNull(c);
		assertTrue(c.isEmpty());
		//
		AgentContext ctx1 = createOneTestingContext(UUID.randomUUID());
		AgentContext ctx2 = createOneTestingContext(UUID.randomUUID());
		//
		c = this.service.getContextIDs();
		assertNotNull(c);
		assertFalse(c.isEmpty());
		assertEquals(2, c.size());
		assertTrue(c.contains(ctx1.getID()));
		assertTrue(c.contains(ctx2.getID()));
	}

	@Test
	public void getContextUUID() {
		UUID cid1 = UUID.randomUUID();
		UUID cid2 = UUID.randomUUID();
		UUID cid3 = UUID.randomUUID();
		//
		assertNull(this.service.getContext(cid1));
		assertNull(this.service.getContext(cid2));
		assertNull(this.service.getContext(cid3));
		//
		AgentContext ctx1 = createOneTestingContext(cid1);
		AgentContext ctx2 = createOneTestingContext(cid2);
		//
		assertSame(ctx1, this.service.getContext(cid1));
		assertSame(ctx2, this.service.getContext(cid2));
		assertNull(this.service.getContext(cid3));
	}

	@Test
	public void getContextsCollection() {
		Collection<AgentContext> c;
		UUID cid1 = UUID.randomUUID();
		UUID cid2 = UUID.randomUUID();
		UUID cid3 = UUID.randomUUID();
		//
		c = this.service.getContexts(Arrays.asList(cid1, cid3));
		assertNotNull(c);
		assertTrue(c.isEmpty());
		//
		AgentContext ctx1 = createOneTestingContext(cid1);
		createOneTestingContext(cid2);
		//
		c = this.service.getContexts(Arrays.asList(cid1, cid3));
		assertNotNull(c);
		assertFalse(c.isEmpty());
		assertEquals(1, c.size());
		assertTrue(c.contains(ctx1));
	}

	@AvoidServiceStartForTest
	@Test
	public void doStop_noinit() throws Exception {
		try {
			this.reflect.invoke(this.service, "doStop");
			fail("Expecting IllegalStateException"); //$NON-NLS-1$
		} catch (InvocationTargetException exception) {
			Throwable ex = exception.getCause();
			if (!(ex instanceof IllegalStateException)) {
				fail("Expecting IllegalStateException"); //$NON-NLS-1$
			}
		}
		verifyNoMoreInteractions(this.contextListener);
	}

	@Test
	@Ignore
	public void doStop_init() throws Exception {
		AgentContext ctx1 = createOneTestingContext(UUID.randomUUID());
		AgentContext ctx2 = createOneTestingContext(UUID.randomUUID());
		//
		this.reflect.invoke(this.service, "doStop");
		ArgumentCaptor<AgentContext> argument = ArgumentCaptor.forClass(AgentContext.class);
		verify(this.contextListener, new Times(2)).contextDestroyed(argument.capture());
		if (ctx1.getID().compareTo(ctx2.getID()) <= 0) {
			assertSame(ctx1, argument.getAllValues().get(0));
			assertSame(ctx2, argument.getAllValues().get(1));
		} else {
			assertSame(ctx1, argument.getAllValues().get(1));
			assertSame(ctx2, argument.getAllValues().get(0));
		}
	}

}
