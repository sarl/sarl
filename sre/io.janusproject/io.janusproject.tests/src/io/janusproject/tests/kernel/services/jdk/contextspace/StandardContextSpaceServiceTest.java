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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.UUID;

import com.google.inject.Injector;
import io.janusproject.kernel.services.jdk.contextspace.Context;
import io.janusproject.kernel.services.jdk.contextspace.ContextFactory;
import io.janusproject.kernel.services.jdk.contextspace.SpaceRepositoryFactory;
import io.janusproject.kernel.services.jdk.contextspace.StandardContextSpaceService;
import io.janusproject.kernel.services.jdk.distributeddata.DMapView;
import io.janusproject.services.contextspace.ContextRepositoryListener;
import io.janusproject.services.contextspace.ContextSpaceService;
import io.janusproject.services.contextspace.SpaceRepositoryListener;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.kerneldiscovery.KernelDiscoveryService;
import io.janusproject.services.logging.LogService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.tests.testutils.AbstractDependentServiceTest;
import io.janusproject.tests.testutils.AvoidServiceStartForTest;
import io.janusproject.tests.testutils.StartServiceForTest;
import io.janusproject.util.TwoStepConstruction;
import javassist.Modifier;
import org.eclipse.jdt.annotation.Nullable;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.internal.verification.Times;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.SpaceID;
import io.sarl.util.OpenEventSpace;
import io.sarl.util.OpenEventSpaceSpecification;

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

	@Mock
	private DistributedDataStructureService dds;

	@Mock
	private LogService logger;

	@Mock
	private Injector injector;

	@Mock
	private EventSpace defaultSpace;

	@Mock
	private Context context;

	@Mock
	private ContextFactory contextFactory;

	@Mock
	private ContextRepositoryListener contextListener;

	/**
	 */
	public StandardContextSpaceServiceTest() {
		super(ContextSpaceService.class);
	}

	@Override
	public StandardContextSpaceService newService() {
		return new StandardContextSpaceService();
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

	@Before
	public void setUp() throws Exception {
		this.contextId = UUID.randomUUID();
		this.innerData = new DMapView<>(UUID.randomUUID().toString(), new HashMap<>());
		this.spaceId = new SpaceID(this.contextId, UUID.randomUUID(), OpenEventSpaceSpecification.class);
		Mockito.when(this.reflect.invoke(this.context, "postConstruction")).thenReturn(this.defaultSpace);
		Mockito.when(this.context.getID()).thenReturn(this.contextId);
		Mockito.when(this.defaultSpace.getID()).thenReturn(this.spaceId);
		Mockito.when(this.contextFactory.newInstance(ArgumentMatchers.any(UUID.class), ArgumentMatchers.any(UUID.class),
				ArgumentMatchers.any(SpaceRepositoryFactory.class), ArgumentMatchers.any(SpaceRepositoryListener.class)))
				.thenAnswer(new Answer<Context>() {
					@Override
					public Context answer(InvocationOnMock invocation) throws Throwable {
						Context ctx = Mockito.mock(Context.class);
						OpenEventSpace mock = Mockito.mock(OpenEventSpace.class);
						SpaceID spaceId = new SpaceID((UUID) invocation.getArguments()[0], (UUID) invocation.getArguments()[1],
								OpenEventSpaceSpecification.class);
						Mockito.when(ctx.getID()).thenReturn(spaceId.getContextID());
						Mockito.when(reflect.invoke(ctx, "postConstruction")).thenReturn(mock);
						Mockito.when(ctx.getDefaultSpace()).thenReturn(mock);
						Mockito.when(mock.getID()).thenReturn(spaceId);
						return ctx;
					}
				});
		Mockito.when(this.dds.getMap(ArgumentMatchers.anyString(), ArgumentMatchers.any(Comparator.class))).thenReturn(this.innerData);
		Mockito.when(this.dds.getMap(ArgumentMatchers.anyString())).thenReturn(this.innerData);
		this.reflect.invoke(this.service, "postConstruction", this.contextId, this.dds, this.logger, this.injector);
		this.reflect.invoke(this.service, "setContextFactory", this.contextFactory);
		this.service.addContextRepositoryListener(this.contextListener);
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
	public void mutex() {
		assertSame(this.service, this.service.mutex());
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
		ContextFactory mock = Mockito.mock(ContextFactory.class);
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
		assertEquals(sid, ctx.getDefaultSpace().getID().getID());
		//
		ArgumentCaptor<AgentContext> argument = ArgumentCaptor.forClass(AgentContext.class);
		Mockito.verify(this.contextListener, new Times(1)).contextCreated(argument.capture());
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
		Mockito.verify(this.contextListener, new Times(1)).contextCreated(argument1.capture());
		assertSame(this.service.getContext(spaceId.getContextID()), argument1.getValue());
		//
		// Second call
		this.reflect.invoke(this.service, "ensureDefaultSpaceDefinition", spaceId);
		//
		assertTrue(this.service.containsContext(spaceId.getContextID()));
		//
		ArgumentCaptor<AgentContext> argument2 = ArgumentCaptor.forClass(AgentContext.class);
		Mockito.verify(this.contextListener, new Times(1)).contextCreated(argument2.capture());
		assertSame(this.service.getContext(spaceId.getContextID()), argument2.getValue());
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
		Mockito.verifyZeroInteractions(this.contextListener);
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
		Mockito.verify(this.contextListener, new Times(1)).contextDestroyed(argument2.capture());
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
		Mockito.verify(this.contextListener, new Times(1)).contextDestroyed(argument1.capture());
		assertSame(ctx2, argument1.getValue());
		//
		// Second call
		this.service.removeContext(ctx2);
		//
		assertTrue(this.service.containsContext(ctx1.getID()));
		assertFalse(this.service.containsContext(ctx2.getID()));
		ArgumentCaptor<AgentContext> argument3 = ArgumentCaptor.forClass(AgentContext.class);
		Mockito.verify(this.contextListener, new Times(1)).contextDestroyed(argument3.capture());
		assertSame(ctx2, argument3.getValue());
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
		Mockito.verify(this.contextListener, new Times(1)).contextDestroyed(argument1.capture());
		assertSame(ctx2, argument1.getValue());
		//
		// Second call
		this.service.removeContext(ctx2.getID());
		//
		assertTrue(this.service.containsContext(ctx1.getID()));
		assertFalse(this.service.containsContext(ctx2.getID()));
		ArgumentCaptor<AgentContext> argument3 = ArgumentCaptor.forClass(AgentContext.class);
		Mockito.verify(this.contextListener, new Times(1)).contextDestroyed(argument3.capture());
		assertSame(ctx2, argument3.getValue());
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
		} catch (IllegalStateException exception) {
			// Expected excpetion fired by notifyStopped()
		}
		Mockito.verifyNoMoreInteractions(this.contextListener);
	}

	@Test
	public void doStop_init() throws Exception {
		AgentContext ctx1 = createOneTestingContext(UUID.randomUUID());
		AgentContext ctx2 = createOneTestingContext(UUID.randomUUID());
		//
		this.reflect.invoke(this.service, "doStop");
		ArgumentCaptor<AgentContext> argument = ArgumentCaptor.forClass(AgentContext.class);
		Mockito.verify(this.contextListener, new Times(2)).contextDestroyed(argument.capture());
		if (ctx1.getID().compareTo(ctx2.getID()) <= 0) {
			assertSame(ctx1, argument.getAllValues().get(0));
			assertSame(ctx2, argument.getAllValues().get(1));
		} else {
			assertSame(ctx1, argument.getAllValues().get(1));
			assertSame(ctx2, argument.getAllValues().get(0));
		}
	}

}
