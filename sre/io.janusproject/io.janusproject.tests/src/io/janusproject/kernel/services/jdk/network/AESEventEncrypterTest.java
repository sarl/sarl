/*
 * $Id$
 * 
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 * 
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.janusproject.kernel.services.jdk.network;

import static org.junit.Assert.assertArrayEquals;
import io.janusproject.services.network.EventEnvelope;
import io.janusproject.testutils.AbstractJanusTest;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings({ "javadoc" })
public class AESEventEncrypterTest extends AbstractJanusTest {

	private AESEventEncrypter encrypter;

	@Before
	public void setUp() throws Exception {
		this.encrypter = new AESEventEncrypter();
		this.encrypter.setKey("abcdefghijklmnop"); //$NON-NLS-1$
	}

	@After
	public void tearDown() {
		this.encrypter = null;
	}

	@Test
	public void encrypt() throws Exception {
		byte[] contextId = new byte[] {1, 2, 3, 4, 5 };
		byte[] spaceId = new byte[] {6, 7, 8, 9, 10 };
		byte[] event = new byte[] {11, 12, 13, 14 };
		byte[] scope = new byte[] {15, 16, 17 };
		byte[] headers = new byte[] {18, 19, 20, 21 };
		EventEnvelope envelope = new EventEnvelope(contextId, spaceId, scope, headers, event);

		this.encrypter.encrypt(envelope);

		assertArrayEquals(new byte[] {76, 85, 9, -83, -86, -108, 17, 119, 91, 121, -32, 98, 19, 58, 74, -6 },
				envelope.getContextId());

		assertArrayEquals(new byte[] {-108, 30, 124, -69, -57, 41, 73, -3, -85, -67, 45, -57, 36, 101, 97, 71 },
				envelope.getSpaceId());

		assertArrayEquals(new byte[] {3, 46, -127, 55, -60, 36, -87, -85, 112, 16, -13, 55, 104, 64, 6, 104 },
				envelope.getScope());

		assertArrayEquals(new byte[] {-15, -97, -127, 31, -113, -85, -69, 32, 33, -60, 105, -119, 71, -46, -79, 119 },
				envelope.getCustomHeaders());

		assertArrayEquals(new byte[] {-104, -99, 112, 110, 65, 46, 68, 44, 0, -85, -69, -33, -125, -53, 100, -21 },
				envelope.getBody());
	}

	@Test
	public void decrypt() throws Exception {
		byte[] contextId = new byte[] {76, 85, 9, -83, -86, -108, 17, 119, 91, 121, -32, 98, 19, 58, 74, -6 };
		byte[] spaceId = new byte[] {-108, 30, 124, -69, -57, 41, 73, -3, -85, -67, 45, -57, 36, 101, 97, 71 };
		byte[] event = new byte[] {-104, -99, 112, 110, 65, 46, 68, 44, 0, -85, -69, -33, -125, -53, 100, -21 };
		byte[] scope = new byte[] {3, 46, -127, 55, -60, 36, -87, -85, 112, 16, -13, 55, 104, 64, 6, 104 };
		byte[] headers = new byte[] {-15, -97, -127, 31, -113, -85, -69, 32, 33, -60, 105, -119, 71, -46, -79, 119 };
		EventEnvelope envelope = new EventEnvelope(contextId, spaceId, scope, headers, event);

		this.encrypter.decrypt(envelope);

		assertArrayEquals(new byte[] {1, 2, 3, 4, 5 }, envelope.getContextId());

		assertArrayEquals(new byte[] {6, 7, 8, 9, 10 }, envelope.getSpaceId());

		assertArrayEquals(new byte[] {15, 16, 17 }, envelope.getScope());

		assertArrayEquals(new byte[] {18, 19, 20, 21 }, envelope.getCustomHeaders());

		assertArrayEquals(new byte[] {11, 12, 13, 14 }, envelope.getBody());
	}

}
