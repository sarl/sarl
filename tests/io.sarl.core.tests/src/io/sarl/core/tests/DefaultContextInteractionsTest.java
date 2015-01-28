/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import static org.junit.Assert.assertEquals;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventSpace;
import io.sarl.lang.core.Scope;

import java.util.UUID;

import org.junit.Before;
import org.junit.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultContextInteractionsTest extends AbstractSARLTest<Capacity> {

	/**
	 */
	@Before
	public void setUp() {
		loadSARL("io.sarl.core.DefaultContextInteractions", Capacity.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void memberCount() {
		assertEquals(7, this.type.getDeclaredMethods().length);
	}
	
	/**
	 */
	@Test
	public void getDefaultContext() {
		assertMethod("getDefaultContext", AgentContext.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void getDefaultSpace() {
		assertMethod("getDefaultSpace", EventSpace.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void getDefaultAddress() {
		assertMethod("getDefaultAddress", Address.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void emitEventScope() {
		assertMethod("emit", void.class, Event.class, Scope.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void emitEvent() {
		assertMethod("emit", void.class, Event.class); //$NON-NLS-1$
	}

	/**
	 */
	@Test
	public void receive() {
		assertMethod("receive", void.class, UUID.class, Event.class); //$NON-NLS-1$
	}
	
	/**
	 */
	@Test
	public void spawn() {
		assertMethod("spawn", UUID.class, Class.class, Object[].class); //$NON-NLS-1$
	}

}
