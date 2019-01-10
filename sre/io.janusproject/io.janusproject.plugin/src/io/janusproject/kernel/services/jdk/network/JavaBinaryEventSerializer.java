/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.janusproject.kernel.services.jdk.network;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.StreamCorruptedException;
import java.lang.reflect.Proxy;
import java.text.MessageFormat;
import java.util.Map;
import java.util.UUID;

import com.google.inject.Inject;

import io.janusproject.services.network.AbstractEventSerializer;
import io.janusproject.services.network.EventDispatch;
import io.janusproject.services.network.EventEncrypter;
import io.janusproject.services.network.EventEnvelope;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.util.ClassFinder;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;

/**
 * Serialize the {@link EventDispatch} content using the Java serialization mechanism to generate the corresponding
 * {@link EventEnvelope}.
 *
 * <p>This implementation assumes that an {@link EventEncrypter} is injected.
 *
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JavaBinaryEventSerializer extends AbstractEventSerializer {

	private static final String SPACE_SPECIFICATION_HEADER = "x-java-spacespec-class"; //$NON-NLS-1$

	/**
	 * Constructs an GsonEventSerializer. The {@link EventEncrypter} is injected.
	 *
	 * @param encrypter the object that will permits to encrypt the events.
	 */
	@Inject
	public JavaBinaryEventSerializer(EventEncrypter encrypter) {
		super(encrypter);
	}

	@Override
	public EventEnvelope serialize(EventDispatch dispatch) throws Exception {
		assert this.encrypter != null : "Invalid injection of the encrypter"; //$NON-NLS-1$
		assert dispatch != null : "Parameter 'dispatch' must not be null"; //$NON-NLS-1$
		final Event event = dispatch.getEvent();
		assert event != null;
		final SpaceID spaceID = dispatch.getSpaceID();
		assert spaceID != null;
		assert spaceID.getSpaceSpecification() != null;

		final Map<String, String> headers = dispatch.getCustomHeaders();
		assert headers != null;
		headers.put(SPACE_SPECIFICATION_HEADER,
				spaceID.getSpaceSpecification().getName());

		final Scope<?> scope = dispatch.getScope();

		final byte[] serializedContextID = NetworkUtil.toByteArray(spaceID.getContextID());
		final byte[] serializedSpaceID = NetworkUtil.toByteArray(spaceID.getID());
		final byte[] serializedScope = toBytes(scope);
		final byte[] serializedHeaders = toBytes(dispatch.getCustomHeaders());
		final byte[] serializedEvent = toBytes(event);
		final EventEnvelope envelope = new EventEnvelope(
				serializedContextID, serializedSpaceID, serializedScope, serializedHeaders, serializedEvent);

		this.encrypter.encrypt(envelope);

		return envelope;
	}

	private static byte[] toBytes(Object object) throws IOException {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
			final ObjectOutputStream oos = new ObjectOutputStream(baos);
			oos.writeObject(object);
			return baos.toByteArray();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public EventDispatch deserialize(EventEnvelope envelope) throws Exception {
		assert this.encrypter != null : "Invalid injection of the encrypter"; //$NON-NLS-1$
		assert envelope != null : "Parameter 'envelope' must not be null"; //$NON-NLS-1$

		this.encrypter.decrypt(envelope);

		final Map<String, String> headers = fromBytes(envelope.getCustomHeaders(), Map.class);
		assert headers != null;

		Class<?> spaceSpec = null;
		final String classname = headers.get(SPACE_SPECIFICATION_HEADER);
		if (classname != null) {
			try {
				spaceSpec = Class.forName(classname);
			} catch (Throwable exception) {
				//
			}
		}

		if (spaceSpec == null || !SpaceSpecification.class.isAssignableFrom(spaceSpec)) {
			throw new ClassCastException(MessageFormat.format(Messages.JavaBinaryEventSerializer_0, spaceSpec));
		}

		final UUID contextId = NetworkUtil.fromByteArray(envelope.getContextId());
		final UUID spaceId = NetworkUtil.fromByteArray(envelope.getSpaceId());

		final SpaceID spaceID = new SpaceID(contextId, spaceId, (Class<? extends SpaceSpecification<?>>) spaceSpec);

		final Event event = fromBytes(envelope.getBody(), Event.class);
		assert event != null;
		final Scope<?> scope = fromBytes(envelope.getScope(), Scope.class);
		return new EventDispatch(spaceID, event, scope, headers);

	}

	private static <T> T fromBytes(byte[] data, Class<T> type) throws IOException, ClassNotFoundException {
		try (ByteArrayInputStream bais = new ByteArrayInputStream(data)) {
			try (ObjectInputStream oos = new ClassLoaderObjectInputStream(bais)) {
				final Object object = oos.readObject();
				if (object != null && type.isInstance(object)) {
					return type.cast(object);
				}
				throw new ClassCastException(MessageFormat.format(Messages.JavaBinaryEventSerializer_0, type.getName()));
			}
		}
	}

	/** Object input stream that is aware of the Bundle class loaders.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ClassLoaderObjectInputStream extends ObjectInputStream {

		/**
		 * Constructor.
		 *
		 * @param inputStream  the InputStream to work on
		 * @throws IOException in case of an I/O error
		 * @throws StreamCorruptedException if the stream is corrupted
		 */
		public ClassLoaderObjectInputStream(InputStream inputStream)
						throws IOException, StreamCorruptedException {
			super(inputStream);
		}

		/**
		 * Resolve a class specified by the descriptor using the
		 * specified ClassLoader or the super ClassLoader.
		 *
		 * @param objectStreamClass  descriptor of the class
		 * @return the Class object described by the ObjectStreamClass
		 * @throws IOException in case of an I/O error
		 * @throws ClassNotFoundException if the Class cannot be found
		 */
		@Override
		protected Class<?> resolveClass(ObjectStreamClass objectStreamClass)
				throws IOException, ClassNotFoundException {
			final Class<?> type = ClassFinder.findClass(objectStreamClass.getName());
			if (type != null) {
				return type;
			}
			// classloader knows not of class, let the super classloader do it
			return super.resolveClass(objectStreamClass);
		}

		/**
		 * Create a proxy class that implements the specified interfaces using
		 * the specified ClassLoader or the super ClassLoader.
		 *
		 * @param interfaces the interfaces to implement
		 * @return a proxy class implementing the interfaces
		 * @throws IOException in case of an I/O error
		 * @throws ClassNotFoundException if the Class cannot be found
		 */
		@Override
		protected Class<?> resolveProxyClass(String[] interfaces) throws IOException, ClassNotFoundException {
			final Class<?>[] interfaceClasses = new Class[interfaces.length];
			ClassLoader cl = null;
			for (int i = 0; i < interfaces.length; i++) {
				final Class<?> type = ClassFinder.findClass(interfaces[i]);
				if (type != null) {
					interfaceClasses[i] = type;
					cl = type.getClassLoader();
				} else {
					throw new ClassNotFoundException(interfaces[i]);
				}
			}
			try {
				return Proxy.getProxyClass(cl, interfaceClasses);
			} catch (IllegalArgumentException e) {
				return super.resolveProxyClass(interfaces);
			}
		}

	}

}
