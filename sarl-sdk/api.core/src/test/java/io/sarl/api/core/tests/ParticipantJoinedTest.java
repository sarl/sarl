/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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
package io.sarl.api.core.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.SpaceID;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("ParticipantJoined")
@Tag("unit")
@Tag("api")
public class ParticipantJoinedTest extends AbstractSarlCoreTest<Event> {

	/**
	 */
	@BeforeEach
	public void setUp() {
		loadSARL("io.sarl.api.core.ParticipantJoined", Event.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void memberCount() {
		assertEquals(3, this.type.getDeclaredFields().length, "invalid field count: " + Arrays.toString(this.type.getDeclaredFields()));
		assertEquals(2, this.type.getDeclaredConstructors().length, "invalid constructor count: " + Arrays.toString(this.type.getDeclaredConstructors()));
	}

	/**
	 */
	@Test
	public void serialVersionUID() {
		assertField("serialVersionUID", long.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void spaceID() {
		assertField("spaceID", SpaceID.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void agentType() {
		assertField("agentType", String.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void constructor() {
		assertConstructor(Address.class, SpaceID.class, String.class);
		assertDeprecated(assertConstructor(Address.class, SpaceID.class));
	}

}
