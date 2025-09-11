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
package io.sarl.lang.core.tests.scoping.extensions.uuid;

import static io.sarl.tests.api.tools.TestMockito.mock;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.scoping.extensions.uuid.UUIDExtensions;

/**
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("UUIDExtensions")
@Tag("unit")
@Tag("core")
public class UUIDExtensionsTest {

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
		SpaceID spaceID = mockSpaceID(this.agentID);
		this.adr = new Address(spaceID, this.agentID);
	}

	/**
	 */
	@Test
	@DisplayName("(UUID) null == this")
	public void operatorEqualsUUIDNullAddress() {
		assertFalse(UUIDExtensions.operator_equals((UUID) null, this.adr));
	}

	/**
	 */
	@SuppressWarnings("static-method")
	@Test
	@DisplayName("(UUID) null == (Address) null")
	public void operatorEqualsUUIDNullAddressNull() {
		assertTrue(UUIDExtensions.operator_equals((UUID) null, (Address) null));
	}

	/**
	 */
	@Test
	@DisplayName("UUID == this")
	public void operatorEqualsUUIDAddress() {
		assertTrue(UUIDExtensions.operator_equals(this.agentID, this.adr));
		assertFalse(UUIDExtensions.operator_equals(UUID.randomUUID(), this.adr));
	}

	/**
	 */
	@Test
	@DisplayName("(UUID) null != this")
	public void operatorNotEqualsUUIDNullAddress() {
		assertTrue(UUIDExtensions.operator_notEquals((UUID) null, this.adr));
	}

	/**
	 */
	@SuppressWarnings("static-method")
	@Test
	@DisplayName("(UUID) null != (Address) null")
	public void operatorNotEqualsUUIDNullAddressNull() {
		assertFalse(UUIDExtensions.operator_notEquals((UUID) null, (Address) null));
	}

	/**
	 */
	@Test
	@DisplayName("UUID != this")
	public void operatorNotEqualsUUIDAddress() {
		assertFalse(UUIDExtensions.operator_notEquals(this.agentID, this.adr));
		assertTrue(UUIDExtensions.operator_notEquals(UUID.randomUUID(), this.adr));
	}

}
