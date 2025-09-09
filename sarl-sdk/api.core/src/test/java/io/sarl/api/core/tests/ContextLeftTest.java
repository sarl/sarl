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

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.Event;

/**
 * @author $Author: sgalland$
 * @version api.core 0.15.0 20250909-115748
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 */
@DisplayName("ContextLeft")
@Tag("unit")
@Tag("api")
public class ContextLeftTest extends AbstractSarlCoreTest<Event> {

	/**
	 */
	@BeforeEach
	public void setUp() {
		loadSARL("io.sarl.api.core.ContextLeft", Event.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void memberCount() {
		assertEquals(1+1, this.type.getDeclaredFields().length, "number of fields"); //$NON-NLS-1$
		assertEquals(1, this.type.getDeclaredConstructors().length, "number of constructors"); //$NON-NLS-1$
		assertEquals(3, this.type.getDeclaredMethods().length, "number of methods"); //$NON-NLS-1$
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
	public void holonContextID() {
		assertField("holonContextID", UUID.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void constructor() {
		assertConstructor(UUID.class);
	}

}
