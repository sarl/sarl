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

import io.sarl.lang.core.Address;
import io.sarl.lang.core.Event;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@DisplayName("AgentSpawnFailure")
@Tag("unit")
@Tag("api")
public class AgentSpawnFailureTest extends AbstractSarlCoreTest<Event> {

	/**
	 */
	@BeforeEach
	public void setUp() {
		loadSARL("io.sarl.core.AgentSpawnFailure", Event.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void memberCount() {
		assertEquals(2+1, this.type.getDeclaredFields().length, "number of fields");
		assertEquals(1+1, this.type.getDeclaredConstructors().length, "number of constructors");
		assertEquals(1+3, this.type.getDeclaredMethods().length, "number of methods");
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
	public void constructors() {
		assertConstructor(Address.class, UUID.class, Class.class, Object.class);
		assertConstructor(UUID.class, Class.class, Object.class);
	}

}
