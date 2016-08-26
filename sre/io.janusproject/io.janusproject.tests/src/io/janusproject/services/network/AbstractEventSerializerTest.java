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
package io.janusproject.services.network;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.UUID;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import io.janusproject.kernel.services.jdk.network.PlainTextEventEncrypter;
import io.janusproject.testutils.AbstractJanusTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class AbstractEventSerializerTest extends AbstractJanusTest {

	private static AbstractEventSerializer serializer;

	@BeforeClass
	public static void setUp() {
		serializer = new AbstractEventSerializer(new PlainTextEventEncrypter()) {
			@Override
			public EventEnvelope serialize(EventDispatch dispatch) throws Exception {
				return null;
			}

			@Override
			public EventDispatch deserialize(EventEnvelope envelope) throws Exception {
				return null;
			}
		};
	}

	@AfterClass
	public static void tearDown() {
		serializer = null;
	}

	@Test
	public void serializeContextID() throws Exception {
		byte[] b = serializer.serializeContextID(UUID.fromString("005dd043-8553-40d2-8094-ad159bfabf86")); //$NON-NLS-1$
		assertNotNull(b);
		assertEquals(36, b.length);
		assertEquals((byte) '0', b[0]);
		assertEquals((byte) '0', b[1]);
		assertEquals((byte) '5', b[2]);
		assertEquals((byte) 'd', b[3]);
		assertEquals((byte) 'd', b[4]);
		assertEquals((byte) '0', b[5]);
		assertEquals((byte) '4', b[6]);
		assertEquals((byte) '3', b[7]);
		assertEquals((byte) '-', b[8]);
		assertEquals((byte) '8', b[9]);
		assertEquals((byte) '5', b[10]);
		assertEquals((byte) '5', b[11]);
		assertEquals((byte) '3', b[12]);
		assertEquals((byte) '-', b[13]);
		assertEquals((byte) '4', b[14]);
		assertEquals((byte) '0', b[15]);
		assertEquals((byte) 'd', b[16]);
		assertEquals((byte) '2', b[17]);
		assertEquals((byte) '-', b[18]);
		assertEquals((byte) '8', b[19]);
		assertEquals((byte) '0', b[20]);
		assertEquals((byte) '9', b[21]);
		assertEquals((byte) '4', b[22]);
		assertEquals((byte) '-', b[23]);
		assertEquals((byte) 'a', b[24]);
		assertEquals((byte) 'd', b[25]);
		assertEquals((byte) '1', b[26]);
		assertEquals((byte) '5', b[27]);
		assertEquals((byte) '9', b[28]);
		assertEquals((byte) 'b', b[29]);
		assertEquals((byte) 'f', b[30]);
		assertEquals((byte) 'a', b[31]);
		assertEquals((byte) 'b', b[32]);
		assertEquals((byte) 'f', b[33]);
		assertEquals((byte) '8', b[34]);
		assertEquals((byte) '6', b[35]);
	}

}
