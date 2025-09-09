/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
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

package io.sarl.api.core.tests.spaces;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.sarl.api.core.spaces.Participant;
import io.sarl.api.core.tests.spaces.mocks.ParticipantMock;
import io.sarl.lang.core.Address;
import io.sarl.lang.core.EventListener;
import io.sarl.lang.core.annotation.PrivateAPI;
import io.sarl.lang.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.PropertyRestoreExtension;

/** 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version api.core 0.15.0 20250909-115748
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 */
@ExtendWith({
	ContextInitExtension.class,
	JavaVersionCheckExtension.class,
	PropertyRestoreExtension.class
})
@DisplayName("Participant")
@Tag("unit")
@Tag("api")
@SuppressWarnings("all")
public class ParticipantTest {

	private Address address;

	private EventListener eventListener;
	
	private Participant test;

	@BeforeEach
	public void setUp() {
		this.address = mock(Address.class);
		this.eventListener = mock(EventListener.class);
		this.test = new ParticipantMock(this.address, this.eventListener);
	}

	@Test
	@DisplayName("createAndInit(null, null)")
	public void createAndInit_null_null() {
		assertThrows(AssertionError.class, () -> {
			Participant.createAndInit(null, null);
		});
	}

	@Test
	@DisplayName("createAndInit(a, null)")
	public void createAndInit_address_null() {
		var a = mock(Address.class);
		var p = Participant.createAndInit(a, null);
		assertNotNull(p);
		assertSame(a, p.getAddress());
		assertNull(p.getParticipant());
	}

	@Test
	@DisplayName("createAndInit(null, l)")
	public void createAndInit_null_listener() {
		var l = mock(EventListener.class);
		assertThrows(AssertionError.class, () -> {
			Participant.createAndInit(null, l);
		});
	}

	@Test
	@DisplayName("createAndInit(a, l)")
	public void createAndInit_address_listener() {
		var a = mock(Address.class);
		var l = mock(EventListener.class);
		var p = Participant.createAndInit(a, l);
		assertNotNull(p);
		assertSame(a, p.getAddress());
		assertSame(l, p.getParticipant());
	}

	@Test
	@DisplayName("getAddress")
	public void getAddress() {
		assertSame(this.address, this.test.getAddress());
	}

	@Test
	@DisplayName("getParticipant")
	public void getParticipant() {
		assertSame(this.eventListener, this.test.getParticipant());
	}

	@Test
	@DisplayName("equals")
	public void javaEquals() {
	}

}
