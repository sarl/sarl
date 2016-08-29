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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.net.InetAddress;
import java.net.URI;

import org.junit.Assume;
import org.junit.Test;

import io.janusproject.services.network.NetworkService;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.testutils.AbstractDependentServiceTest;
import io.janusproject.testutils.AvoidServiceStartForTest;
import io.janusproject.testutils.StartServiceForTest;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@StartServiceForTest
@SuppressWarnings("all")
public class NoNetworkServiceTest extends AbstractDependentServiceTest<NoNetworkService> {

	public NoNetworkServiceTest() {
		super(NetworkService.class);
	}

	@Override
	public final NoNetworkService newService() {
		return new NoNetworkService();
	}

	@Override
	public void getServiceDependencies() {
		assertContains(this.service.getServiceDependencies());
	}

	@Override
	public void getServiceWeakDependencies() {
		assertContains(this.service.getServiceWeakDependencies());
	}

	@AvoidServiceStartForTest
	@Test
	public void getURI_notStarted() throws Exception {
		assertNull(this.service.getURI());
	}

	@Test
	public void getURI_started_loopback() throws Exception {
		InetAddress adr = NetworkUtil.getLoopbackAddress();
		Assume.assumeNotNull(adr);
		assertEquals(new URI("tcp://" + adr.getHostAddress() + ":0"), this.service.getURI());
	}

	@Test
	public void getURI_started_noLoopback() throws Exception {
		InetAddress adr = NetworkUtil.getLoopbackAddress();
		Assume.assumeTrue(adr == null);
		assertEquals(new URI("tcp://127.0.0.1:0"), this.service.getURI());
	}

	@Test
	public void publish() throws Exception {
		this.service.publish(null, null);
	}

	@Test
	public void connectToRemoteSpaces() throws Exception {
		this.service.connectToRemoteSpaces(null, null, null);
	}

	@Test
	public void disconnectFromRemoteSpace() throws Exception {
		this.service.disconnectFromRemoteSpace(null, null);
	}

	@Test
	public void disconnectPeer() throws Exception {
		this.service.disconnectPeer(null);
	}

}
