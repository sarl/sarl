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

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import io.sarl.api.core.spaces.AddressLazyLinks;
import io.sarl.api.core.spaces.Participant;
import io.sarl.lang.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.PropertyRestoreExtension;

/** 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version api.core 0.15.1 20250911-224825
 * @mavengroupid io.sarl.sdk
 * @mavenartifactid api.core
 */
@ExtendWith({
	ContextInitExtension.class,
	JavaVersionCheckExtension.class,
	PropertyRestoreExtension.class
})
@DisplayName("AddressLazyLinks")
@Tag("unit")
@Tag("api")
@SuppressWarnings("all")
public class AddressLazyLinksTest {

	private AddressLazyLinks test;
	
	@BeforeEach
	public void setUp() {
		this.test = new AddressLazyLinks();
	}
	
	@Test
	@DisplayName("getParticipant")
	public void getParticipant() {
		assertNull(this.test.getParticipant());
	}

	@Test
	@DisplayName("setParticipant(null)")
	public void setParticipant_null() {
		var p = mock(Participant.class);
		this.test.setParticipant(p);
		assertSame(p, this.test.getParticipant());

		this.test.setParticipant(null);
		assertNull(this.test.getParticipant());
	}

	@Test
	@DisplayName("setParticipant w/o loose")
	public void setParticipant_noLoose() {
		var p = mock(Participant.class);
		this.test.setParticipant(p);
		assertSame(p, this.test.getParticipant());
	}

	@Test
	@DisplayName("setParticipant w/ loose")
	public void setParticipant_loose() {
		{
			var p = mock(Participant.class);
			this.test.setParticipant(p);
			assertSame(p, this.test.getParticipant());
			// Remove strong referencing
			p = null;
		}
		// Force release of weakly referenced objects
		System.gc();
		System.gc();
		System.gc();
		assertNull(this.test.getParticipant());
	}

}
