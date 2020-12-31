/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
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
package io.sarl.lang.core.tests.core;

import static io.sarl.tests.api.tools.TestMockito.mock;
import static io.sarl.tests.api.tools.TestMockito.spy;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Event")
@Tag("unit")
@Tag("core")
public class EventTest extends AbstractSarlTest {

	private Event event;

	private static Event mockEvent() {
		return new Event() {
			private static final long serialVersionUID = -1997616003372673851L;
			//
		};
	}

	@BeforeEach
	public void setUp() {
		this.event = mockEvent();
	}

	private static Address mockAddress(UUID contextId, UUID spaceId, UUID agentID) {
		SpaceID sid = new SpaceID(contextId, spaceId, null);
		Address adr = new Address(spy(sid), agentID);
		return spy(adr);
	}

	@Test
	public void getSource() {
		assertNull(this.event.getSource());
		Address adr = mock(Address.class);
		this.event.setSource(adr);
		assertSame(adr, this.event.getSource());
	}

	@Test
	public void setSource() {
		assertNull(this.event.getSource());
		Address adr = mock(Address.class);
		this.event.setSource(adr);
		assertSame(adr, this.event.getSource());
	}

	@Test
	public void equals() {
		Event e;
		Address adr1 = mock(Address.class);
		Address adr2 = mock(Address.class);
		doReturn(Boolean.TRUE).when(adr1).equals(ArgumentMatchers.eq(adr1));
		doReturn(Boolean.FALSE).when(adr1).equals(ArgumentMatchers.eq(adr2));
		doReturn(Boolean.FALSE).when(adr2).equals(ArgumentMatchers.eq(adr1));
		doReturn(Boolean.TRUE).when(adr2).equals(ArgumentMatchers.eq(adr2));
		this.event.setSource(adr1);
		//
		assertTrue(this.event.equals(this.event));
		//
		e = mockEvent();
		e.setSource(adr1);
		assertTrue(this.event.equals(e));
		//
		e = mockEvent();
		e.setSource(adr2);
		assertFalse(this.event.equals(e));
	}

	@Test
	public void testHashCode() {
		Event e;
		Address adr1 = mock(Address.class);
		Address adr2 = mock(Address.class);
		this.event.setSource(adr1);
		//
		assertEquals(this.event.hashCode(), this.event.hashCode());
		//
		e = mockEvent();
		e.setSource(adr1);
		assertEquals(this.event.hashCode(), e.hashCode());
		//
		e = mockEvent();
		e.setSource(adr2);
		assertNotEquals(this.event.hashCode(), e.hashCode());
	}

	@Test
	public void isFromAddress() {
		Address adr = mockAddress(UUID.randomUUID(), UUID.randomUUID(), UUID.randomUUID());
		this.event.setSource(adr);

		Address adr0 = mock(Address.class);
		Address adr1 = mock(Address.class);

		assertFalse(this.event.isFrom((Address) null));
		assertTrue(this.event.isFrom(adr));
		assertFalse(this.event.isFrom(adr0));
		assertFalse(this.event.isFrom(adr1));
	}

	@Test
	public void isFromUUID() {
		UUID id = UUID.randomUUID();
		Address adr = mockAddress(UUID.randomUUID(), UUID.randomUUID(), id);
		this.event.setSource(adr);

		UUID id0 = UUID.randomUUID();
		UUID id1 = UUID.randomUUID();

		assertFalse(this.event.isFrom((UUID) null));
		assertTrue(this.event.isFrom(id));
		assertFalse(this.event.isFrom(id0));
		assertFalse(this.event.isFrom(id1));
	}

}
