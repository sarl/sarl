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
package io.janusproject.tests.kernel.space;

import java.security.Principal;
import java.security.acl.Acl;
import java.security.acl.Permission;
import java.util.HashMap;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;

import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.internal.verification.Times;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import io.janusproject.kernel.services.jdk.distributeddata.DMapView;
import io.janusproject.kernel.space.RestrictedAccessEventSpaceImpl;
import io.janusproject.services.distributeddata.DMap;
import io.janusproject.services.distributeddata.DistributedDataStructureService;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.network.NetworkService;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.OpenEventSpaceSpecification;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.ManualMocking;
import io.sarl.tests.api.Nullable;
import io.sarl.util.Scopes;
import io.sarl.util.concurrent.NoReadWriteLock;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@ManualMocking
public class RestrictedAccessEventSpaceImplTest extends AbstractJanusTest {

	@Nullable
	private UUID agentId;

	@Nullable
	private DistributedDataStructureService service;

	@Nullable
	private SpaceID spaceId;

	@Nullable
	private Address address;

	@Nullable
	private EventListener listener;

	@Nullable
	private Acl acl;

	@Nullable
	private Permission permission;

	@Mock
	private NetworkService network;

	@Mock
	private ExecutorService executor;

	@InjectMocks
	private RestrictedAccessEventSpaceImpl space;

	@Before
	public void setUp() {
		this.agentId = UUID.randomUUID();

		this.service = Mockito.mock(DistributedDataStructureService.class);
		DMap<Object, Object> mapMock = new DMapView<>(UUID.randomUUID().toString(), new HashMap<>());
		Mockito.when(this.service.getMap(ArgumentMatchers.any(), ArgumentMatchers.any())).thenReturn(mapMock);
		Mockito.when(this.service.getMap(ArgumentMatchers.any())).thenReturn(mapMock);

		this.spaceId = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class);

		this.address = new Address(this.spaceId, this.agentId);

		this.acl = Mockito.mock(Acl.class);
		Mockito.when(this.acl.checkPermission(Mockito.any(), Mockito.any())).thenReturn(true);
		this.permission = Mockito.mock(Permission.class);

		this.space = new RestrictedAccessEventSpaceImpl(this.spaceId, this.acl, this.permission, this.service,
				() -> NoReadWriteLock.SINGLETON);

		this.listener = Mockito.mock(EventListener.class);
		Mockito.when(this.listener.getID()).thenReturn(this.agentId);

		MockitoAnnotations.initMocks(this);

		Mockito.when(this.executor.submit(Mockito.any(Runnable.class))).thenAnswer(new Answer<Future<?>>() {
			@Override
			public Future<?> answer(InvocationOnMock invocation) throws Throwable {
				Runnable r = (Runnable) invocation.getArguments()[0];
				r.run();
				return null;
			}
		});
	}

	private void register() {
		this.space.register(this.listener, Mockito.mock(Principal.class));
	}

	private void unregister() {
		this.space.unregister(this.listener);
	}

	@Test
	public void getAddressEventListener() {
		assertNull(this.space.getAddress(this.listener));
		register();
		assertEquals(this.address, this.space.getAddress(this.listener));
		unregister();
		assertNull(this.space.getAddress(this.listener));
	}

	@Test
	public void getAddressUUID() {
		assertNull(this.space.getAddress(this.listener.getID()));
		register();
		assertEquals(this.address, this.space.getAddress(this.listener.getID()));
		unregister();
		assertNull(this.space.getAddress(this.listener.getID()));
	}

	@Test
	public void getParticipants() {
		Set<UUID> set;
		set = this.space.getParticipants();
		assertNotNull(set);
		assertTrue(set.isEmpty());
		register();
		set = this.space.getParticipants();
		assertNotNull(set);
		assertEquals(1, set.size());
		assertTrue(set.contains(this.listener.getID()));
		unregister();
		set = this.space.getParticipants();
		assertNotNull(set);
		assertTrue(set.isEmpty());
	}

	@Test
	public void doEmit_fullscope() throws Exception {
		Event event;

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.<Address> allParticipants());
		Mockito.verifyZeroInteractions(this.listener);

		register();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.<Address> allParticipants());

		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());

		unregister();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.<Address> allParticipants());
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertNotSame(event, argument.getValue());
	}

	@Test
	public void doEmit_scopeaddress() throws Exception {
		Event event;

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(this.address));
		Mockito.verifyZeroInteractions(this.listener);

		register();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(this.address));

		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());

		unregister();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(this.address));
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertNotSame(event, argument.getValue());
	}

	@Test
	public void doEmit_scopeotheraddress() throws Exception {
		Address otherAddress = new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class),
				UUID.randomUUID());

		Event event;

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(otherAddress));
		Mockito.verifyZeroInteractions(this.listener);

		register();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(otherAddress));
		Mockito.verify(this.listener, new Times(0)).receiveEvent(Mockito.any());

		unregister();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(otherAddress));
		Mockito.verify(this.listener, new Times(0)).receiveEvent(Mockito.any());
	}

	@Test
	public void doEmit_scopebothaddresses() throws Exception {
		Address otherAddress = new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class),
				UUID.randomUUID());

		Event event;

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(this.address, otherAddress));
		Mockito.verifyZeroInteractions(this.listener);

		register();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(this.address, otherAddress));

		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());

		unregister();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.reflect.invoke(this.space, "doEmit", event, Scopes.addresses(this.address, otherAddress));
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertNotSame(event, argument.getValue());
	}

	@Test
	public void emitEventScope_fullscope() throws Exception {
		Event event;
		Scope<Address> scope = Scopes.<Address> allParticipants();

		register();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.space.emit(event, scope);

		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
		{
			ArgumentCaptor<Scope> netscope = ArgumentCaptor.forClass(Scope.class);
			ArgumentCaptor<Event> netarg = ArgumentCaptor.forClass(Event.class);
			Mockito.verify(this.network).publish(netscope.capture(), netarg.capture());
			assertSame(scope, netscope.getValue());
			assertSame(event, netarg.getValue());
		}
	}

	@Test
	public void emitEventScope_scopeaddress() throws Exception {
		Event event;
		Scope<Address> scope = Scopes.addresses(this.address);

		register();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.space.emit(event, scope);

		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
		{
			ArgumentCaptor<Scope> netscope = ArgumentCaptor.forClass(Scope.class);
			ArgumentCaptor<Event> netarg = ArgumentCaptor.forClass(Event.class);
			Mockito.verify(this.network).publish(netscope.capture(), netarg.capture());
			assertSame(scope, netscope.getValue());
			assertSame(event, netarg.getValue());
		}
	}

	@Test
	public void emitEventScope_scopeotheraddress() throws Exception {
		Address otherAddress = new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class),
				UUID.randomUUID());

		Event event;
		Scope<Address> scope = Scopes.addresses(otherAddress);

		register();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.space.emit(event, scope);
		Mockito.verify(this.listener, new Times(0)).receiveEvent(Mockito.any());
		{
			ArgumentCaptor<Scope> netscope = ArgumentCaptor.forClass(Scope.class);
			ArgumentCaptor<Event> netarg = ArgumentCaptor.forClass(Event.class);
			Mockito.verify(this.network).publish(netscope.capture(), netarg.capture());
			assertSame(scope, netscope.getValue());
			assertSame(event, netarg.getValue());
		}
	}

	@Test
	public void emitEventScope_scopebothaddresses() throws Exception {
		Address otherAddress = new Address(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class),
				UUID.randomUUID());

		Event event;
		Scope<Address> scope = Scopes.addresses(this.address, otherAddress);

		register();

		event = Mockito.mock(Event.class);
		Mockito.when(event.getSource()).thenReturn(this.address);
		this.space.emit(event, scope);

		ArgumentCaptor<Event> argument = ArgumentCaptor.forClass(Event.class);
		Mockito.verify(this.listener).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
		{
			ArgumentCaptor<Scope> netscope = ArgumentCaptor.forClass(Scope.class);
			ArgumentCaptor<Event> netarg = ArgumentCaptor.forClass(Event.class);
			Mockito.verify(this.network).publish(netscope.capture(), netarg.capture());
			assertSame(scope, netscope.getValue());
			assertSame(event, netarg.getValue());
		}
	}

}
