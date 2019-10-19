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

import java.io.ByteArrayOutputStream;
import java.io.ObjectOutputStream;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.arakhne.afc.vmutil.ClassLoaderFinder;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import io.janusproject.kernel.services.jdk.network.JavaBinaryEventSerializer;
import io.janusproject.kernel.services.jdk.network.PlainTextEventEncrypter;
import io.janusproject.services.network.EventDispatch;
import io.janusproject.services.network.EventEnvelope;
import io.janusproject.tests.testutils.AbstractJanusTest;

import io.sarl.core.OpenEventSpaceSpecification;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.tests.api.Nullable;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class JavaBinaryEventSerializerTest extends AbstractJanusTest {

	@Nullable
	private JavaBinaryEventSerializer serializer;

	@Nullable
	private UUID rawContextId;

	@Nullable
	private byte[] serializedContextId;

	@Nullable
	private UUID rawSpaceId;

	@Nullable
	private byte[] serializedSpaceId;

	@Nullable
	private Scope<?> rawScope;

	@Nullable
	private byte[] serializedScope;

	@Nullable
	private Map<String, String> rawSimpleHeader;

	@Nullable
	private Map<String, String> rawAutofilledHeader;

	@Nullable
	private byte[] serializedHeader;

	@Nullable
	private Event rawEvent;

	@Nullable
	private byte[] serializedEvent;

	@Before
	public void setUp() throws Exception {
		ClassLoaderFinder.setPreferredClassLoader(getClass().getClassLoader());
		this.serializer = new JavaBinaryEventSerializer(new PlainTextEventEncrypter());

		this.rawContextId = UUID.fromString("005dd043-8553-40d2-8094-ad159bfabf86"); //$NON-NLS-1$
		this.serializedContextId = "005dd043-8553-40d2-8094-ad159bfabf86".getBytes(Charset.forName("UTF-8")); //$NON-NLS-1$ //$NON-NLS-2$

		this.rawSpaceId = UUID.fromString("76595ddf-bc40-479d-b92a-7c1785642f9c"); //$NON-NLS-1$
		this.serializedSpaceId = "76595ddf-bc40-479d-b92a-7c1785642f9c".getBytes(Charset.forName("UTF-8")); //$NON-NLS-1$ //$NON-NLS-2$

		this.rawScope = new ScopeMock();
		this.serializedScope = new byte[] {-84, -19, 0, 5, 115, 114, 0, 89, 105, 111, 46, 106, 97, 110, 117,
				115, 112, 114, 111, 106, 101, 99, 116, 46, 116, 101, 115, 116, 115, 46, 107, 101, 114, 110,
				101, 108, 46, 115, 101, 114, 118, 105, 99, 101, 115, 46, 106, 100, 107, 46, 110, 101, 116,
				119, 111, 114, 107, 46, 74, 97, 118, 97, 66, 105, 110, 97, 114, 121, 69, 118, 101, 110, 116,
				83, 101, 114, 105, 97, 108, 105, 122, 101, 114, 84, 101, 115, 116, 36, 83, 99, 111, 112, 101,
				77, 111, 99, 107, -46, -8, -41, 2, 51, 82, -99, -22, 2, 0, 0, 120, 112};

		this.rawSimpleHeader = new HashMap<>();
		this.rawSimpleHeader.put("a", "b"); //$NON-NLS-1$//$NON-NLS-2$
		this.rawAutofilledHeader = new HashMap<>();
		this.rawAutofilledHeader.putAll(this.rawSimpleHeader);
		this.rawAutofilledHeader.put("x-java-spacespec-class", //$NON-NLS-1$
				OpenEventSpaceSpecification.class.getName());
		this.serializedHeader = new byte[] {-84, -19, 0, 5, 115, 114, 0, 17, 106, 97, 118, 97, 46, 117, 116, 105, 108, 46, 72, 97, 115, 104, 77, 97, 112, 5, 7, -38, -63, -61, 22, 96, -47, 3, 0, 2, 70, 0, 10, 108, 111, 97, 100, 70, 97, 99, 116, 111, 114, 73, 0, 9, 116, 104, 114, 101, 115, 104, 111, 108, 100, 120, 112, 63, 64, 0, 0, 0, 0, 0, 3, 119, 8, 0, 0, 0, 4, 0, 0, 0, 2, 116, 0, 1, 97, 116, 0, 1, 98, 116, 0, 22, 120, 45, 106, 97, 118, 97, 45, 115, 112, 97, 99, 101, 115, 112, 101, 99, 45, 99, 108, 97, 115, 115, 116, 0, 40, 105, 111, 46, 115, 97, 114, 108, 46, 99, 111, 114, 101, 46, 79, 112, 101, 110, 69, 118, 101, 110, 116, 83, 112, 97, 99, 101, 83, 112, 101, 99, 105, 102, 105, 99, 97, 116, 105, 111, 110, 120};

		this.rawEvent = new EventMock();
		this.serializedEvent = new byte[] {-84, -19, 0, 5, 115, 114, 0, 89, 105, 111, 46, 106, 97, 110, 117, 115, 112, 114, 111, 106, 101, 99, 116, 46, 116, 101, 115, 116, 115, 46, 107, 101, 114, 110, 101, 108, 46, 115, 101, 114, 118, 105, 99, 101, 115, 46, 106, 100, 107, 46, 110, 101, 116, 119, 111, 114, 107, 46, 74, 97, 118, 97, 66, 105, 110, 97, 114, 121, 69, 118, 101, 110, 116, 83, 101, 114, 105, 97, 108, 105, 122, 101, 114, 84, 101, 115, 116, 36, 69, 118, 101, 110, 116, 77, 111, 99, 107, 118, 53, 99, -95, 11, -80, -90, 102, 2, 0, 0, 120, 114, 0, 23, 105, 111, 46, 115, 97, 114, 108, 46, 108, 97, 110, 103, 46, 99, 111, 114, 101, 46, 69, 118, 101, 110, 116, -60, 32, 18, 125, -41, 61, 90, -122, 2, 0, 1, 76, 0, 6, 115, 111, 117, 114, 99, 101, 116, 0, 27, 76, 105, 111, 47, 115, 97, 114, 108, 47, 108, 97, 110, 103, 47, 99, 111, 114, 101, 47, 65, 100, 100, 114, 101, 115, 115, 59, 120, 112, 112};

//		 {
//		 ByteArrayOutputStream baos = new ByteArrayOutputStream();
//		 ObjectOutputStream oos = new ObjectOutputStream(baos);
//		 oos.writeObject(this.rawAutofilledHeader);
//		 oos.close();
//		 System.out.println(Arrays.toString(baos.toByteArray()));
//		 }
	}

	@After
	public void tearDown() {
		ClassLoaderFinder.popPreferredClassLoader();
	}

	@Test
	public void serialize() throws Exception {
		EventDispatch dispatch = new EventDispatch(
				new SpaceID(this.rawContextId, this.rawSpaceId, OpenEventSpaceSpecification.class), this.rawEvent, this.rawScope);
		dispatch.getCustomHeaders().clear();
		dispatch.getCustomHeaders().putAll(this.rawSimpleHeader);
		EventEnvelope e = this.serializer.serialize(dispatch);
		assertNotNull(e);

		assertArrayEquals(this.serializedContextId, e.getContextId());

		assertArrayEquals(this.serializedSpaceId, e.getSpaceId());

		assertArrayEquals(this.serializedScope, e.getScope());

		assertArrayEquals(this.serializedHeader, e.getCustomHeaders());

		assertArrayEquals(this.serializedEvent, e.getBody());
	}

	@Test
	public void deserialize() throws Exception {
		EventEnvelope envelope = new EventEnvelope(this.serializedContextId, this.serializedSpaceId, this.serializedScope,
				this.serializedHeader, this.serializedEvent);
		EventDispatch d = this.serializer.deserialize(envelope);
		assertNotNull(d);

		assertEquals(this.rawContextId, d.getSpaceID().getContextID());

		assertEquals(this.rawSpaceId, d.getSpaceID().getID());

		assertEquals(this.rawScope, d.getScope());

		assertEquals(this.rawAutofilledHeader, d.getCustomHeaders());

		assertEquals(this.rawEvent, d.getEvent());
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class EventMock extends Event {

		private static final long serialVersionUID = 8517823813578172006L;

		/**
		 */
		EventMock() {
			//
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public boolean equals(Object obj) {
			return (obj instanceof EventMock);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int hashCode() {
			return 1234567890;
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ScopeMock implements Scope<String> {

		private static final long serialVersionUID = -3244607127069483542L;

		/**
		 */
		ScopeMock() {
			//
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public boolean equals(Object obj) {
			return (obj instanceof ScopeMock);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public int hashCode() {
			return 987654321;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public boolean matches(String element) {
			throw new UnsupportedOperationException();
		}

	}

}
