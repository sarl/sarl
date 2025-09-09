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

import static io.sarl.tests.api.tools.TestAssertions.assertContains;
import static io.sarl.tests.api.tools.TestAssertions.assertContainsCollection;
import static io.sarl.tests.api.tools.TestMockito.mock;
import static io.sarl.tests.api.tools.TestReflections.invokeProc;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;

import io.sarl.api.core.spaces.AbstractEventSpace;
import io.sarl.api.core.spaces.OpenEventSpaceSpecification;
import io.sarl.api.core.spaces.Participant;
import io.sarl.api.core.tests.spaces.mocks.XAbstractEventSpaceMock;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.InformedEventListener;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.Nullable;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.PropertyRestoreExtension;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version api.core 0.15.0 20250909-115748
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 */
@SuppressWarnings("all")
@ExtendWith({
	ContextInitExtension.class,
	JavaVersionCheckExtension.class,
	PropertyRestoreExtension.class
})
@DisplayName("AbstractEventSpace")
@Tag("unit")
@Tag("api")
public class XAbstractEventSpaceTest {

	@Nullable
	private UUID agentId;

	@Nullable
	private UUID agentId2;

	@Nullable
	private SpaceID spaceId;

	@Nullable
	private Address address;

	@Nullable
	private InformedEventListener listener1; 

	@Nullable
	private InformedEventListener listener2; 

	@Nullable
	private ConcurrentHashMap<UUID, Participant> strongParticipants;

	@Nullable
	private ConcurrentHashMap<UUID, Participant> weakParticipants;
	
	@Nullable
	private AbstractEventSpace space; 

	@BeforeEach
	public void setUp() {
		this.strongParticipants = new ConcurrentHashMap();
		this.weakParticipants = new ConcurrentHashMap();

		this.agentId = UUID.randomUUID();

		this.agentId2 = UUID.randomUUID();

		this.spaceId = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), OpenEventSpaceSpecification.class);

		this.address = new Address(this.spaceId, this.agentId);

		this.space = new XAbstractEventSpaceMock(this.strongParticipants, this.weakParticipants, this.spaceId);

		this.listener1 = mock(InformedEventListener.class);
		when(this.listener1.getID()).thenReturn(this.agentId);

		this.listener2 = mock(InformedEventListener.class);
		when(this.listener2.getID()).thenReturn(this.agentId2);

		openMocks(this);
	}

	private Set<UUID> register() {
		return register(true, true, 1);
	}

	private Set<UUID> register(int nbWeaks) {
		return register(true, true, nbWeaks);
	}

	private Set<UUID> register(boolean registerAgent2, int nbWeaks) {
		return register(registerAgent2, true, nbWeaks);
	}

	private Set<UUID> register(boolean registerAgent2) {
		return register(registerAgent2, true, 1);
	}

	private Set<UUID> register(boolean registerAgent2, boolean registerAgent3) {
		return register(registerAgent2, registerAgent3, 1);
	}

	private Set<UUID> register(boolean registerAgent2, boolean registerAgent3, int nbWeaks) {
		var participant1 = mock(Participant.class);
		when(participant1.getAddress()).thenReturn(this.address);
		when(participant1.getParticipant()).thenReturn(this.listener1);
		this.strongParticipants.put(this.agentId, participant1);

		if (registerAgent2) {
			var participant2 = mock(Participant.class);
			when(participant2.getAddress()).thenReturn(new Address(this.spaceId, this.agentId2));
			when(participant2.getParticipant()).thenReturn(this.listener2);
			this.strongParticipants.put(this.agentId2, participant2);
		}
		if (registerAgent3) {
			var id3 = UUID.randomUUID();
			var participant3 = mock(Participant.class);
			when(participant3.getAddress()).thenReturn(new Address(this.spaceId, id3));
			when(participant3.getParticipant()).thenReturn(mock(InformedEventListener.class));
			this.strongParticipants.put(id3, participant3);
		}

		var weaks = new HashSet<UUID>();
		for (var i = 1; i <= nbWeaks; ++i) {
			var id1w = UUID.randomUUID();
			var participant1w = mock(Participant.class);
			when(participant1w.getAddress()).thenReturn(new Address(this.spaceId, id1w));
			when(participant1w.getParticipant()).thenReturn(mock(InformedEventListener.class));
			this.weakParticipants.put(id1w, participant1w);
			weaks.add(id1w);
		}
		return weaks;
	}

	@Test
	@DisplayName("getAddress")
	public void getAddressUUID() {
		assertNull(this.space.getAddress(this.listener1.getID()));
		register();
		assertSame(this.address, this.space.getAddress(this.listener1.getID()));
	}

	private void emitLocally(Event event, Scope<Address> scope) throws Exception {
		invokeProc(this.space.getClass(), this.space, "emitLocally", new Class[] {Event.class, Scope.class}, event, scope);
	}

	@Test
	@DisplayName("emitLocally(e, null)")
	public void emitLocally_nullScope() throws Exception {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		emitLocally(event, null);
		verifyNoMoreInteractions(this.listener1);
		verifyNoMoreInteractions(this.listener2);

		register();

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		emitLocally(event, null);

		var argument = ArgumentCaptor.forClass(Event.class);

		verify(this.listener1).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());

		verify(this.listener2).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	@DisplayName("emitLocally(e) [true]")
	public void emitLocally_allParticipants() throws Exception {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		emitLocally(event, it -> true);
		verifyNoMoreInteractions(this.listener1);
		verifyNoMoreInteractions(this.listener2);

		register();

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		emitLocally(event, it -> true);

		var argument = ArgumentCaptor.forClass(Event.class);

		verify(this.listener1).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());

		verify(this.listener2).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	@DisplayName("emitLocally(e) [it.ID == i]")
	public void emitLocally_singleAddress() throws Exception {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		emitLocally(event, it -> this.address.equals(it));
		verifyNoMoreInteractions(this.listener1);
		verifyNoMoreInteractions(this.listener2);

		register();

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		emitLocally(event, it -> this.address.equals(it));

		var argument = ArgumentCaptor.forClass(Event.class);

		verify(this.listener1).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
		verifyNoMoreInteractions(this.listener2);
	}

	@Test
	@DisplayName("emit(e, null)")
	public void emit_nullScope() {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);
		verifyNoMoreInteractions(this.listener1);
		verifyNoMoreInteractions(this.listener2);

		register();

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, null);

		var argument = ArgumentCaptor.forClass(Event.class);

		verify(this.listener1).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());

		verify(this.listener2).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	@DisplayName("emit(e) [true]")
	public void emit_allParticipants() {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, it -> true);
		verifyNoMoreInteractions(this.listener1);
		verifyNoMoreInteractions(this.listener2);

		register();

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, it -> true);

		var argument = ArgumentCaptor.forClass(Event.class);

		verify(this.listener1).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());

		verify(this.listener2).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
	}

	@Test
	@DisplayName("emit(e) [it.ID == i]")
	public void emit_singleAddress() {
		var event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, it -> this.address.equals(it));
		verifyNoMoreInteractions(this.listener1);
		verifyNoMoreInteractions(this.listener2);

		register();

		event = mock(Event.class);
		when(event.getSource()).thenReturn(this.address);
		this.space.emit(null, event, it -> this.address.equals(it));

		var argument = ArgumentCaptor.forClass(Event.class);

		verify(this.listener1).receiveEvent(argument.capture());
		assertSame(event, argument.getValue());
		verifyNoMoreInteractions(this.listener2);
	}

	@Test
	@DisplayName("getNumberOfStrongParticipants without participant")
	public void getNumberOfStrongParticipants_registration0() {
		assertEquals(0, this.space.getNumberOfStrongParticipants());
	}

	@Test
	@DisplayName("getNumberOfStrongParticipants with 1 participant")
	public void getNumberOfStrongParticipants_registration1() {
		register(false, false);
		assertEquals(1, this.space.getNumberOfStrongParticipants());
	}

	@Test
	@DisplayName("getNumberOfStrongParticipants with 2 participants")
	public void getNumberOfStrongParticipants_registration2() {
		register(true, false);
		assertEquals(2, this.space.getNumberOfStrongParticipants());
	}

	@Test
	@DisplayName("getNumberOfStrongParticipants with 3 participants")
	public void getNumberOfStrongParticipants_registration3() {
		register();
		assertEquals(3, this.space.getNumberOfStrongParticipants());
	}

	@Test
	@DisplayName("getNumberOfWeakParticipants without participant")
	public void getNumberOfWeakParticipants_registration0() {
		assertEquals(0, this.space.getNumberOfWeakParticipants());
	}

	@Test
	@DisplayName("getNumberOfWeakParticipants with 1 strong and 1 weak")
	public void getNumberOfWeakParticipants_registration1() {
		register(false, false);
		assertEquals(1, this.space.getNumberOfWeakParticipants());
	}

	@Test
	@DisplayName("getNumberOfWeakParticipants with 2 strongs and 1 weak")
	public void getNumberOfWeakParticipants_registration2() {
		register(true, false);
		assertEquals(1, this.space.getNumberOfWeakParticipants());
	}

	@Test
	@DisplayName("getNumberOfWeakParticipants with 3 strongs and 1 weak")
	public void getNumberOfWeakParticipants_registration3() {
		register();
		assertEquals(1, this.space.getNumberOfWeakParticipants());
	}

	@Test
	@DisplayName("getNumberOfWeakParticipants with 1 strong and 2 weaks")
	public void getNumberOfWeakParticipants_registration4() {
		register(false, false, 2);
		assertEquals(2, this.space.getNumberOfWeakParticipants());
	}

	@Test
	@DisplayName("getNumberOfWeakParticipants with 2 strongs and 2 weaks")
	public void getNumberOfWeakParticipants_registration5() {
		register(true, false, 2);
		assertEquals(2, this.space.getNumberOfWeakParticipants());
	}

	@Test
	@DisplayName("getNumberOfWeakParticipants with 3 strongs and 2 weaks")
	public void getNumberOfWeakParticipants_registration6() {
		register(true, true, 2);
		assertEquals(2, this.space.getNumberOfWeakParticipants());
	}

	@RepeatedTest(5)
	@DisplayName("isPseudoEmpty(UUID) without participant")
	public void isPseudoEmptyUUID_noRegistration() {
		assertTrue(this.space.isPseudoEmpty(this.agentId));
		assertTrue(this.space.isPseudoEmpty(this.agentId2));
		assertTrue(this.space.isPseudoEmpty(UUID.randomUUID()));
	}

	@RepeatedTest(5)
	@DisplayName("isPseudoEmpty(UUID) with 1 participant")
	public void isPseudoEmptyUUID_registration1() {
		register(false, false);
		assertTrue(this.space.isPseudoEmpty(this.agentId));
		assertFalse(this.space.isPseudoEmpty(this.agentId2));
		assertFalse(this.space.isPseudoEmpty(UUID.randomUUID()));
	}

	@RepeatedTest(5)
	@DisplayName("isPseudoEmpty(UUID) with 2 participants")
	public void isPseudoEmptyUUID_registration2() {
		register(true, false);
		assertFalse(this.space.isPseudoEmpty(this.agentId));
		assertFalse(this.space.isPseudoEmpty(this.agentId2));
		assertFalse(this.space.isPseudoEmpty(UUID.randomUUID()));
	}

	@RepeatedTest(5)
	@DisplayName("isPseudoEmpty(UUID) with 3 participants")
	public void isPseudoEmptyUUID_registration3() {
		register();
		assertFalse(this.space.isPseudoEmpty(this.agentId));
		assertFalse(this.space.isPseudoEmpty(this.agentId2));
		assertFalse(this.space.isPseudoEmpty(UUID.randomUUID()));
	}

	@Test
	@DisplayName("isPseudoEmpty without participant")
	public void isPseudoEmpty_noRegistration() {
		assertTrue(this.space.isPseudoEmpty());
	}

	@Test
	@DisplayName("isPseudoEmpty with 1 participant")
	public void isPseudoEmpty_registration1() {
		register(false, false);
		assertFalse(this.space.isPseudoEmpty());
	}

	@Test
	@DisplayName("isPseudoEmpty with 2 participants")
	public void isPseudoEmpty_registration2() {
		register(true, false);
		assertFalse(this.space.isPseudoEmpty());
	}

	@Test
	@DisplayName("isPseudoEmpty with 3 participants")
	public void isPseudoEmpty_registration3() {
		register();
		assertFalse(this.space.isPseudoEmpty());
	}

	@Test
	@DisplayName("forEachStrongParticipant without participant")
	public void forEachStrongParticipant_registration0() {
		var col = new ArrayList<UUID>();
		this.space.forEachStrongParticipant(it -> col.add(it));
		assertTrue(col.isEmpty());
	}

	@Test
	@DisplayName("forEachStrongParticipant with 1 participant")
	public void forEachStrongParticipant_registration1() {
		register(false, false);
		var col = new ArrayList<UUID>();
		this.space.forEachStrongParticipant(it -> col.add(it));
		assertContains(col, this.agentId);
	}

	@Test
	@DisplayName("forEachStrongParticipant with 2 participants")
	public void forEachStrongParticipant_registration2() {
		register(true, false);
		var col = new ArrayList<UUID>();
		this.space.forEachStrongParticipant(it -> col.add(it));
		assertContains(col, this.agentId, this.agentId2);
	}

	@Test
	@DisplayName("forEachStrongParticipant with 3 participants")
	public void forEachStrongParticipant_registration3() {
		register();
		var col = new ArrayList<UUID>();
		this.space.forEachStrongParticipant(it -> col.add(it));
		assertEquals(3, col.size());
		assertTrue(col.contains(this.agentId));
		assertTrue(col.contains(this.agentId2));
	}

	@Test
	@DisplayName("forEachWeakParticipant without participant")
	public void forEachWeakParticipant_registration0() {
		var col = new ArrayList<UUID>();
		this.space.forEachWeakParticipant(it -> col.add(it));
		assertTrue(col.isEmpty());
	}

	@Test
	@DisplayName("forEachWeakParticipant with 1 strong and 1 weak")
	public void forEachWeakParticipant_registration1() {
		var weaks = register(false, false);
		var col = new ArrayList<UUID>();
		this.space.forEachWeakParticipant(it -> col.add(it));
		assertContainsCollection(weaks, col);
	}

	@Test
	@DisplayName("forEachWeakParticipant with 2 strongs and 1 weak")
	public void forEachWeakParticipant_registration2() {
		var weaks = register(true, false);
		var col = new ArrayList<UUID>();
		this.space.forEachWeakParticipant(it -> col.add(it));
		assertContainsCollection(weaks, col);
	}

	@Test
	@DisplayName("forEachWeakParticipant with 3 strongs and 1 weak")
	public void forEachWeakParticipant_registration3() {
		var weaks = register();
		var col = new ArrayList<UUID>();
		this.space.forEachWeakParticipant(it -> col.add(it));
		assertContainsCollection(weaks, col);
	}

	@Test
	@DisplayName("forEachWeakParticipant with 2 strongs and 2 weaks")
	public void forEachWeakParticipant_registration4() {
		var weaks = register(true, false, 2);
		var col = new ArrayList<UUID>();
		this.space.forEachWeakParticipant(it -> col.add(it));
		assertContainsCollection(weaks, col);
	}

	@Test
	@DisplayName("forEachWeakParticipant with 3 strongs and 2 weaks")
	public void forEachWeakParticipant_registration5() {
		var weaks = register(true, true, 2);
		var col = new ArrayList<UUID>();
		this.space.forEachWeakParticipant(it -> col.add(it));
		assertContainsCollection(weaks, col);
	}

}
