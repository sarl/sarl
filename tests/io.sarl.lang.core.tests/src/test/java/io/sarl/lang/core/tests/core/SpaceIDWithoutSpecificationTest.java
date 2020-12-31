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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("SpaceID without specification")
@Tag("unit")
@Tag("core")
public class SpaceIDWithoutSpecificationTest extends AbstractSarlTest {

	private UUID contextID;

	private UUID id;

	private SpaceID spaceID;

	private int cmp(UUID a, UUID b) {
		int cmp = this.contextID.compareTo(a);
		if (cmp == 0) {
			cmp = this.id.compareTo(b);
		}
		return cmp;
	}

	@BeforeEach
	public void setUp() {
		this.contextID = UUID.randomUUID();
		this.id = UUID.randomUUID();
		this.spaceID = new SpaceID(this.contextID, this.id, null);
	}

	/**
	 */
	@Test
	public void getID() {
		assertSame(this.id, this.spaceID.getID());
	}

	/**
	 */
	@Test
	public void getContextID() {
		assertSame(this.contextID, this.spaceID.getContextID());
	}

	/**
	 */
	@Test
	public void getSpaceSpecification() {
		assertNull(this.spaceID.getSpaceSpecification());
	}

	/**
	 */
	@Test
	public void equals() {
		SpaceID sid;
		//
		assertTrue(this.spaceID.equals(this.spaceID));
		//
		sid = new SpaceID(this.contextID, this.id, null);
		assertTrue(this.spaceID.equals(sid));
		//
		sid = new SpaceID(
				UUID.fromString(this.contextID.toString()),
				UUID.fromString(this.id.toString()),
				null);
		assertTrue(this.spaceID.equals(sid));
		//
		sid = new SpaceID(UUID.randomUUID(), this.id, null);
		assertFalse(this.spaceID.equals(sid));
		//
		sid = new SpaceID(this.contextID, UUID.randomUUID(), null);
		assertFalse(this.spaceID.equals(sid));
		//
		sid = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), null);
		assertFalse(this.spaceID.equals(sid));
	}

	/**
	 */
	@Test
	public void testHashCode() {
		SpaceID sid;
		//
		assertEquals(this.spaceID.hashCode(), this.spaceID.hashCode());
		//
		sid = new SpaceID(this.contextID, this.id, null);
		assertEquals(this.spaceID.hashCode(), sid.hashCode());
		//
		sid = new SpaceID(
				UUID.fromString(this.contextID.toString()),
				UUID.fromString(this.id.toString()),
				null);
		assertEquals(this.spaceID.hashCode(), sid.hashCode());
		//
		sid = new SpaceID(UUID.randomUUID(), this.id, null);
		assertNotEquals(this.spaceID.hashCode(), sid.hashCode());
		//
		sid = new SpaceID(this.contextID, UUID.randomUUID(), null);
		assertNotEquals(this.spaceID.hashCode(), sid.hashCode());
		//
		sid = new SpaceID(UUID.randomUUID(), UUID.randomUUID(), null);
		assertNotEquals(this.spaceID.hashCode(), sid.hashCode());
	}

	/**
	 */
	@Test
	public void compareTo() {
		SpaceID sid;
		UUID uuid1;
		UUID uuid2;
		//
		assertEquals(0, this.spaceID.compareTo(this.spaceID));
		//
		sid = new SpaceID(this.contextID, this.id, null);
		assertEquals(0, this.spaceID.compareTo(sid));
		//
		sid = new SpaceID(
				UUID.fromString(this.contextID.toString()),
				UUID.fromString(this.id.toString()),
				null);
		assertEquals(0, this.spaceID.compareTo(sid));
		//
		uuid1 = UUID.randomUUID();
		sid = new SpaceID(uuid1, this.id, null);
		assertEquals(
				cmp(uuid1, this.id),
				this.spaceID.compareTo(sid));
		//
		uuid1 = UUID.randomUUID();
		sid = new SpaceID(this.contextID, uuid1, null);
		assertEquals(
				cmp(this.contextID, uuid1),
				this.spaceID.compareTo(sid));
		//
		uuid1 = UUID.randomUUID();
		uuid2 = UUID.randomUUID();
		sid = new SpaceID(uuid1, uuid2, null);
		assertEquals(
				cmp(uuid1, uuid2),
				this.spaceID.compareTo(sid));
	}

}
