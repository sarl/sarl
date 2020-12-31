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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.core.tests.core;

import static io.sarl.tests.api.tools.TestMockito.mock;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Address")
@Tag("unit")
@Tag("core")
public class AddressTest extends AbstractSarlTest {

	private SpaceID spaceID;

	private UUID agentID;

	private Address adr;

	private static SpaceID mockSpaceID(final UUID agentUID, final UUID spaceUID) {
		SpaceID spaceID = mock(SpaceID.class);
		when(spaceID.getContextID()).thenReturn(agentUID);
		when(spaceID.getID()).thenReturn(spaceUID);
		return spaceID;
	}

	private static SpaceID mockSpaceID(UUID agentUID) {
		return mockSpaceID(agentUID, UUID.randomUUID());
	}

	/**
	 */
	@BeforeEach
	public void setUp() {
		this.agentID = UUID.fromString("d9162392-dedf-43a6-be1a-b9fed6d5253c"); //$NON-NLS-1$
		this.spaceID = mockSpaceID(this.agentID);
		this.adr = new Address(this.spaceID, this.agentID);
	}

	/**
	 */
	@Test
	public void getID() {
		assertSame(this.agentID, this.adr.getID());
	}

	/**
	 */
	@Test
	public void getSpaceId() {
		assertSame(this.spaceID, this.adr.getSpaceID());
	}

	/**
	 */
	@Test
	public void equalsAddress() {
		Address newAdr;
		UUID newAgentID = UUID.randomUUID();
		SpaceID newSpaceID = mockSpaceID(newAgentID);
		//
		newAdr = new Address(this.spaceID, this.agentID);
		assertTrue(this.adr.equals(newAdr));
		//
		newAdr = new Address(newSpaceID, newAgentID);
		assertFalse(this.adr.equals(newAdr));
	}

	/**
	 */
	@Test
	public void equalsObject() {
		Object newAdr;
		UUID newAgentID = UUID.randomUUID();
		SpaceID newSpaceID = mockSpaceID(newAgentID);
		//
		newAdr = new Address(this.spaceID, this.agentID);
		assertTrue(this.adr.equals(newAdr));
		//
		newAdr = new Address(newSpaceID, newAgentID);
		assertFalse(this.adr.equals(newAdr));
		//
		assertFalse(this.adr.equals(new Object()));
	}

	/**
	 */
	@Test
	public void compareTo() {
		Address newAdr;
		UUID newAgentID1 = UUID.fromString("00000000-0000-0000-0000-000000000000"); //$NON-NLS-1$
		UUID newAgentID2 = UUID.fromString("ffffffff-ffff-ffff-ffff-ffffffffffff"); //$NON-NLS-1$
		SpaceID newSpaceID1 = mockSpaceID(newAgentID1);
		SpaceID newSpaceID2 = mockSpaceID(newAgentID1);
		SpaceID newSpaceID3 = mockSpaceID(newAgentID2);
		SpaceID newSpaceID4 = mockSpaceID(newAgentID2);
		//
		newAdr = new Address(this.spaceID, this.agentID);
		assertEquals(
				this.agentID.compareTo(this.agentID),
				this.adr.compareTo(newAdr));
		//
		newAdr = new Address(newSpaceID1, newAgentID1);
		assertEquals(
				this.agentID.compareTo(newAgentID1),
				this.adr.compareTo(newAdr));
		//
		newAdr = new Address(newSpaceID2, newAgentID1);
		assertEquals(
				this.agentID.compareTo(newAgentID1),
				this.adr.compareTo(newAdr));
		//
		newAdr = new Address(newSpaceID3, newAgentID2);
		assertEquals(
				this.agentID.compareTo(newAgentID2),
				this.adr.compareTo(newAdr));
		//
		newAdr = new Address(newSpaceID4, newAgentID2);
		assertEquals(
				this.agentID.compareTo(newAgentID2),
				this.adr.compareTo(newAdr));
	}

}
