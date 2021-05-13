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

import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.Scope;
import io.sarl.lang.util.ConcurrentCollection;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@DisplayName("Behaviors")
@Tag("unit")
@Tag("api")
public class BehaviorsTest extends AbstractSarlCoreTest<Capacity> {

	@BeforeEach
	public void setUp() {
		loadSARL("io.sarl.core.Behaviors", Capacity.class); //$NON-NLS-1$
	}

	@Test
	public void memberCount() {
		assertEquals(0, this.type.getDeclaredFields().length, "number of fields");
		assertEquals(0, this.type.getDeclaredConstructors().length, "number of constructors");
		assertEquals(8+2+2, this.type.getDeclaredMethods().length, "number of methods");
	}

	@Test
	public void hasRegisteredBehavior() {
		assertMethod("hasRegisteredBehavior", boolean.class); //$NON-NLS-1$
	}

	@Test
	public void getRegisteredBehaviors() {
		assertMethod("getRegisteredBehaviors", ConcurrentCollection.class); //$NON-NLS-1$
	}

	@Test
	public void registerBehavior() {
		assertMethod("registerBehavior", Behavior.class, Behavior.class, Function1.class, Object[].class); //$NON-NLS-1$
	}

	@Test
	public void unregisterBehavior() {
		assertMethod("unregisterBehavior", Behavior.class, Behavior.class); //$NON-NLS-1$
	}

	@Test
	public void wake_noScope() {
		assertMethod("wake", void.class, Event.class); //$NON-NLS-1$
	}

	@Test
	public void wake_scope() {
		assertMethod("wake", void.class, Event.class, Scope.class); //$NON-NLS-1$
	}

	@Test
	public void asEventListener() {
		assertMethod("asEventListener", EventListener.class); //$NON-NLS-1$
	}

}
