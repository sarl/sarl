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
package io.janusproject.tests.kernel.services.jdk.network;

import static org.junit.Assert.assertSame;

import io.janusproject.kernel.services.jdk.network.PlainTextEventEncrypter;
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
public class PlainTextEventEncrypterTest extends AbstractJanusTest {

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

	@Nullable
	private PlainTextEventEncrypter encrypter;

	@Before
	public void setUp() {
		this.contextId = new byte[] { 1, 2, 3, 4, 5 };
		this.spaceId = new byte[] { 6, 7, 8, 9, 10 };
		this.event = new byte[] { 11, 12, 13, 14 };
		this.scope = new byte[] { 15, 16, 17 };
		this.headers = new byte[] { 18, 19, 20, 21 };
		this.envelope = new EventEnvelope(this.contextId, this.spaceId, this.scope, this.headers, this.event);
		this.encrypter = new PlainTextEventEncrypter();
	}

	@Test
	public void encrypt() {
		this.encrypter.encrypt(this.envelope);
		assertSame(this.contextId, this.envelope.getContextId());
		assertSame(this.spaceId, this.envelope.getSpaceId());
		assertSame(this.scope, this.envelope.getScope());
		assertSame(this.headers, this.envelope.getCustomHeaders());
		assertSame(this.event, this.envelope.getBody());
	}

	@Test
	public void decrypt() {
		this.encrypter.decrypt(this.envelope);
		assertSame(this.contextId, this.envelope.getContextId());
		assertSame(this.spaceId, this.envelope.getSpaceId());
		assertSame(this.scope, this.envelope.getScope());
		assertSame(this.headers, this.envelope.getCustomHeaders());
		assertSame(this.event, this.envelope.getBody());
	}

}
