/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.api.core.tests.spaces;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;

import io.sarl.api.core.spaces.OpenEventSpaceSpecification;
import io.sarl.api.core.spaces.OpenLocalEventSpace;
import io.sarl.api.core.spaces.SpaceListener;
import io.sarl.api.core.spaces.SpaceParticipantListener;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.InformedEventListener;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.PropertyRestoreExtension;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version api.core 0.15.0 20250909-115748
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 */
@ExtendWith({
	ContextInitExtension.class,
	JavaVersionCheckExtension.class,
	PropertyRestoreExtension.class
})
@DisplayName("OpenLocalEventSpace")
@Tag("unit")
@Tag("api")
@SuppressWarnings("all")
public class OpenLocalEventSpaceTest {

	private UUID agentId;

	private SpaceID spaceId;

	private Address address;

	private InformedEventListener listener; 

	private ExecutorService executor; 

	private OpenLocalEventSpace space;

	private Logger logger;
	
	@BeforeEach
	public void setUp() {
		this.logger = mock(Logger.class);
		this.agentId = UUID.randomUUID();
		this.spaceId = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class);
		this.space = new OpenLocalEventSpace(this.spaceId, mock(SpaceParticipantListener.class), () -> this.logger);
		this.address = new Address(this.spaceId, this.agentId);

		this.listener = mock(InformedEventListener.class);
		when(this.listener.getID()).thenReturn(this.agentId);

		this.executor = mock(ExecutorService.class);
		doAnswer(it -> {
				var r = (Runnable) it.getArguments()[0];
				r.run();
				return null;
		}).when(this.executor).execute(any(Runnable.class));
	}

	private void doRegister(boolean weak) {
		if (weak) {
			this.space.registerWeakParticipant(this.listener);
		} else {
			this.space.registerStrongParticipant(this.listener);
		}
	}

	private void doUnregister() {
		this.space.unregister(this.listener);
	}

	@Test
	@DisplayName("getAddress strong participant")
	public void getAddressUUID_false() {
		assertNull(this.space.getAddress(this.listener.getID()));
		doRegister(false);
		assertEquals(this.address, this.space.getAddress(this.listener.getID()));
		doUnregister();
		assertNull(this.space.getAddress(this.listener.getID()));
	}

	@Test
	@DisplayName("getAddress weak participant")
	public void getAddressUUID_true() {
		assertNull(this.space.getAddress(this.listener.getID()));
		doRegister(true);
		assertEquals(this.address, this.space.getAddress(this.listener.getID()));
		doUnregister();
		assertNull(this.space.getAddress(this.listener.getID()));
	}

	@Test
	@DisplayName("Register weak participant")
	public void registerWeakParticipant() {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);
		verifyNoMoreInteractions(this.listener);

		doRegister(true);

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);

		var argument = ArgumentCaptor.forClass(Event.class);

		verify(this.listener).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	@DisplayName("Register strong participant")
	public void registerStrongParticipant() {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);
		verifyNoMoreInteractions(this.listener);

		doRegister(false);

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);

		var argument = ArgumentCaptor.forClass(Event.class);

		verify(this.listener).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
	}
	
	@Test
	@DisplayName("Unregister weak participant")
	public void unregister_true() {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);
		verifyNoMoreInteractions(this.listener);

		doRegister(true);
		doUnregister();

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);

		var argument = ArgumentCaptor.forClass(Event.class);
		verify(this.listener, never()).receiveEvent(argument.capture());
	}

	@Test
	@DisplayName("Unregister strong participant")
	public void unregister_false() {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);
		verifyNoMoreInteractions(this.listener);

		doRegister(false);
		doUnregister();

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);

		var argument = ArgumentCaptor.forClass(Event.class);
		verify(this.listener, never()).receiveEvent(argument.capture());

	}

	@Test
	@DisplayName("destroyableSpace weak participant")
	public void destoryableSpaceEvent_true() {
		doRegister(true);
		var listener = mock(SpaceListener.class);
		this.space.setSpaceListenerIfNone(listener);
		doUnregister();
		var dspace = ArgumentCaptor.forClass(Space.class);
		verify(listener).destroyableSpace(dspace.capture());
		assertSame(this.space, dspace.getValue());
	}

	@Test
	@DisplayName("destroyableSpace strong participant")
	public void destoryableSpaceEvent_false() {
		doRegister(false);
		var listener = mock(SpaceListener.class);
		this.space.setSpaceListenerIfNone(listener);
		doUnregister();
		var dspace = ArgumentCaptor.forClass(Space.class);
		verify(listener).destroyableSpace(dspace.capture());
		assertSame(this.space, dspace.getValue());
	}

}
