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
package io.janusproject.tests.services.network;

import static org.junit.Assert.assertSame;

import io.janusproject.services.network.EventEnvelope;
import io.janusproject.tests.testutils.AbstractJanusTest;
import org.junit.Before;
import org.junit.Test;

import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class EventEnvelopeTest extends AbstractJanusTest {

	@Nullable
	private byte[] contextId;

	@Nullable
	private byte[] spaceId;

	@Nullable
	private byte[] event;

	@Nullable
	private byte[] scope;

	@Nullable
	private byte[] headers;

	@Nullable
	private EventEnvelope envelope;

	@Before
	public void setUp() {
		this.contextId = new byte[] { 32 };
		this.spaceId = new byte[] { 32 };
		this.event = new byte[] { 32 };
		this.scope = new byte[] { 32 };
		this.headers = new byte[] { 32 };
		this.envelope = new EventEnvelope(this.contextId, this.spaceId, this.scope, this.headers, this.event);
	}

	@Test
	public void getCustomHeaders() {
		assertSame(this.headers, this.envelope.getCustomHeaders());
	}

	@Test
	public void getBody() {
		assertSame(this.event, this.envelope.getBody());
	}

	@Test
	public void getContextId() {
		assertSame(this.contextId, this.envelope.getContextId());
	}

	@Test
	public void getSpaceId() {
		assertSame(this.spaceId, this.envelope.getSpaceId());
	}

	@Test
	public void getScope() {
		assertSame(this.scope, this.envelope.getScope());
	}

	@Test
	public void setCustomHeaders() {
		byte[] newMock = new byte[] { 32 };
		this.envelope.setCustomHeaders(newMock);
		assertSame(newMock, this.envelope.getCustomHeaders());
	}

	@Test
	public void setBody() {
		byte[] newMock = new byte[] { 32 };
		this.envelope.setBody(newMock);
		assertSame(newMock, this.envelope.getBody());
	}

	@Test
	public void setContextId() {
		byte[] newMock = new byte[] { 32 };
		this.envelope.setContextId(newMock);
		assertSame(newMock, this.envelope.getContextId());
	}

	@Test
	public void setSpaceId() {
		byte[] newMock = new byte[] { 32 };
		this.envelope.setSpaceId(newMock);
		assertSame(newMock, this.envelope.getSpaceId());
	}

	@Test
	public void setScope() {
		byte[] newMock = new byte[] { 32 };
		this.envelope.setScope(newMock);
		assertSame(newMock, this.envelope.getScope());
	}

}
