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
package io.sarl.core.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Space;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.util.ConcurrentSet;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("InnerContextAccess")
@Tag("unit")
@Tag("api")
public class InnerContextAccessTest extends AbstractSarlCoreTest<Capacity> {

	/**
	 */
	@BeforeEach
	public void setUp() {
		loadSARL("io.sarl.core.InnerContextAccess", Capacity.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void memberCount() {
		assertEquals(9, this.type.getDeclaredMethods().length);
	}

	/**
	 */
	@Test
	public void getInnerContext() {
		assertMethod("getInnerContext", AgentContext.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void getInnerDefaultSpace() {
		assertMethod("getInnerDefaultSpace", EventSpace.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void hasMemberAgent() {
		assertMethod("hasMemberAgent", boolean.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void getMemberAgentCount() {
		assertMethod("getMemberAgentCount", int.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void getMemberAgents() {
		assertMethod("getMemberAgents", ConcurrentSet.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void isInnerDefaultSpaceSpace() {
		assertMethod("isInnerDefaultSpace", boolean.class, Space.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void isInnerDefaultSpaceSpaceID() {
		assertMethod("isInnerDefaultSpace", boolean.class, SpaceID.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void isInnerDefaultSpaceUUID() {
		assertMethod("isInnerDefaultSpace", boolean.class, UUID.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void isInInnerDefaultSpace() {
		assertMethod("isInInnerDefaultSpace", boolean.class, Event.class); //$NON-NLS-1$
	}

}
