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
import static org.junit.Assert.assertNull;

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.URI;
import java.util.UUID;

import org.junit.Test;

import io.janusproject.testutils.AbstractJanusTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class NetworkUtilTest extends AbstractJanusTest {

	@Test
	public void toByteArrayUUID() {
		byte[] b = NetworkUtil.toByteArray(UUID.fromString("005dd043-8553-40d2-8094-ad159bfabf86")); //$NON-NLS-1$
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

	@Test(expected = IllegalArgumentException.class)
	public void fromByteArray_invalid() {
		byte[] b = new byte[] { (byte) '0', (byte) '8', (byte) '5', (byte) '5', (byte) '3', };
		NetworkUtil.fromByteArray(b);
	}

	@Test
	public void fromByteArray() {
		byte[] b = new byte[] { (byte) '0', (byte) '0', (byte) '5', (byte) 'd', (byte) 'd', (byte) '0', (byte) '4', (byte) '3',
				(byte) '-', (byte) '8', (byte) '5', (byte) '5', (byte) '3', (byte) '-', (byte) '4', (byte) '0', (byte) 'd',
				(byte) '2', (byte) '-', (byte) '8', (byte) '0', (byte) '9', (byte) '4', (byte) '-', (byte) 'a', (byte) 'd',
				(byte) '1', (byte) '5', (byte) '9', (byte) 'b', (byte) 'f', (byte) 'a', (byte) 'b', (byte) 'f', (byte) '8',
				(byte) '6', };
		UUID id = NetworkUtil.fromByteArray(b);
		assertNotNull(id);
		assertEquals(UUID.fromString("005dd043-8553-40d2-8094-ad159bfabf86"), id); //$NON-NLS-1$
	}

	@Test
	public void toURIInetAddress() throws Exception {
		InetAddress a;
		a = InetAddress.getByName("190.191.192.193"); //$NON-NLS-1$
		assertEquals(NetworkUtil.toURI("tcp://190.191.192.193"), NetworkUtil.toURI(a)); //$NON-NLS-1$
	}

	@Test
	public void toURIInetSocketAddress() throws Exception {
		InetAddress a;
		InetSocketAddress sa;
		a = InetAddress.getByName("190.191.192.193"); //$NON-NLS-1$
		sa = new InetSocketAddress(a, 12346);
		assertEquals(NetworkUtil.toURI("tcp://190.191.192.193:12346"), NetworkUtil.toURI(sa)); //$NON-NLS-1$
	}

	@Test
	public void toURIInetAddressInt() throws Exception {
		InetAddress a;
		a = InetAddress.getByName("190.191.192.193"); //$NON-NLS-1$
		assertEquals(NetworkUtil.toURI("tcp://190.191.192.193:12346"), NetworkUtil.toURI(a, 12346)); //$NON-NLS-1$
	}

	@Test
	public void toInetAddress() throws Exception {
		URI uri = NetworkUtil.toURI("tcp://190.191.192.193:12346"); //$NON-NLS-1$
		InetAddress a = NetworkUtil.toInetAddress(uri);
		assertEquals("190.191.192.193", a.getHostAddress()); //$NON-NLS-1$
	}

	@Test
	public void toInetSocketAddress() throws Exception {
		URI uri = NetworkUtil.toURI("tcp://190.191.192.193:12346"); //$NON-NLS-1$
		InetSocketAddress a = NetworkUtil.toInetSocketAddress(uri);
		assertEquals("190.191.192.193", a.getHostName()); //$NON-NLS-1$
		assertEquals(12346, a.getPort());
	}

	@Test
	public void getPrimaryAddress() {
		InetAddress adr1 = NetworkUtil.getPrimaryAddress();
		InetAddress adr2 = NetworkUtil.getPrimaryAddress();
		if (adr1 == null) {
			assertNull(adr2);
		} else {
			assertEquals(adr1, adr2);
		}
	}

	@Test
	public void getLoopbackAddress() {
		InetAddress ref = InetAddress.getLoopbackAddress();
		assertNotNull(ref);
		if (ref instanceof Inet4Address) {
			InetAddress adr1 = NetworkUtil.getLoopbackAddress();
			InetAddress adr2 = NetworkUtil.getLoopbackAddress();
			assertEquals(ref, adr1);
			assertEquals(adr1, adr2);
		} else {
			InetAddress adr1 = NetworkUtil.getLoopbackAddress();
			InetAddress adr2 = NetworkUtil.getLoopbackAddress();
			assertNull(adr1);
			assertNull(adr2);
		}
	}

	@Test
	public void isConnectedHost() {
		InetAddress adr = NetworkUtil.getPrimaryAddress();
		assertEquals((adr != null), NetworkUtil.isConnectedHost());
	}

}
