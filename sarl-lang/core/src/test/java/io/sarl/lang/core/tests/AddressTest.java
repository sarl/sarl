/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
package io.sarl.lang.core.tests;

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
import io.sarl.lang.core.EventSpaceSpecification;
import io.sarl.lang.core.SpaceID;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("Address")
@Tag("unit")
@Tag("core")
public class AddressTest {

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
	@DisplayName("getID")
	public void getID() {
		assertSame(this.agentID, this.adr.getID());
	}

	/**
	 */
	@Test
	@DisplayName("getSpaceID")
	public void getSpaceID() {
		assertSame(this.spaceID, this.adr.getSpaceID());
	}

	/**
	 */
	@Test
	@DisplayName("equals(Address)")
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
	@DisplayName("equals(Object)")
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
	@DisplayName("compareTo")
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

	/**
	 */
	@Test
	@DisplayName("this == (UUID) null")
	public void operatorEqualsAddressUUIDNull() {
		assertFalse(this.adr.operator_equals((UUID) null));
	}

	/**
	 */
	@Test
	@DisplayName("this == UUID")
	public void operatorEqualsAddressUUID() {
		assertTrue(this.adr.operator_equals(this.agentID));
		assertFalse(this.adr.operator_equals(UUID.randomUUID()));
	}

	/**
	 */
	@Test
	@DisplayName("this == (SpaceID) null")
	public void operatorEqualsAddressSpaceIDNull() {
		assertFalse(this.adr.operator_equals((SpaceID) null));
	}

	/**
	 */
	@Test
	@DisplayName("this == SpaceID")
	public void operatorEqualsAddressSpaceID() {
		assertTrue(this.adr.operator_equals(this.spaceID));
		assertFalse(this.adr.operator_equals(new SpaceID(UUID.randomUUID(), this.agentID, EventSpaceSpecification.class)));
		assertFalse(this.adr.operator_equals(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), EventSpaceSpecification.class)));
	}

	/**
	 */
	@Test
	@DisplayName("this != (UUID) null")
	public void operatorNotEqualsAddressUUIDNull() {
		assertTrue(this.adr.operator_notEquals((UUID) null));
	}

	/**
	 */
	@Test
	@DisplayName("this != UUID")
	public void operatorNotEqualsAddressUUID() {
		assertFalse(this.adr.operator_notEquals(this.agentID));
		assertTrue(this.adr.operator_notEquals(UUID.randomUUID()));
	}

	/**
	 */
	@Test
	@DisplayName("this != (SpaceID) null")
	public void operatorNotEqualsAddressSpaceIDNull() {
		assertTrue(this.adr.operator_notEquals((SpaceID) null));
	}

	/**
	 */
	@Test
	@DisplayName("this != SpaceID")
	public void operatorNotEqualsAddressSpaceID() {
		assertFalse(this.adr.operator_notEquals(this.spaceID));
		assertTrue(this.adr.operator_notEquals(new SpaceID(UUID.randomUUID(), this.agentID, EventSpaceSpecification.class)));
		assertTrue(this.adr.operator_notEquals(new SpaceID(UUID.randomUUID(), UUID.randomUUID(), EventSpaceSpecification.class)));
	}

}
