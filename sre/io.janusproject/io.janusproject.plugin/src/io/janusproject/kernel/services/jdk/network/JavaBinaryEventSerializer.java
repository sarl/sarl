/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Map;
import java.util.UUID;

import com.google.inject.Inject;
import io.janusproject.services.network.AbstractEventSerializer;
import io.janusproject.services.network.EventDispatch;
import io.janusproject.services.network.EventEncrypter;
import io.janusproject.services.network.EventEnvelope;
import io.janusproject.services.network.NetworkUtil;
import org.arakhne.afc.vmutil.locale.Locale;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;

/**
 * Serialize the {@link EventDispatch} content using the Java serialization mechanism to generate the corresponding
 * {@link EventEnvelope}.
 *
 * <p>
 * This implementation assumes that an {@link EventEncrypter} is injected.
 *
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JavaBinaryEventSerializer extends AbstractEventSerializer {

	/**
	 * Constructs an GsonEventSerializer. The {@link EventEncrypter} is injected.
	 *
	 * @param encrypter - the object that will permits to encrypt the events.
	 */
	@Inject
	public JavaBinaryEventSerializer(EventEncrypter encrypter) {
		super(encrypter);
	}

	@Override
	public EventEnvelope serialize(EventDispatch dispatch) throws Exception {
		assert (this.encrypter != null) : "Invalid injection of the encrypter"; //$NON-NLS-1$
		assert (dispatch != null) : "Parameter 'dispatch' must not be null"; //$NON-NLS-1$
		Event event = dispatch.getEvent();
		assert (event != null);
		SpaceID spaceID = dispatch.getSpaceID();
		assert (spaceID != null);
		assert (spaceID.getSpaceSpecification() != null);

		Map<String, String> headers = dispatch.getCustomHeaders();
		assert (headers != null);
		headers.put("x-java-spacespec-class", //$NON-NLS-1$
				spaceID.getSpaceSpecification().getName());

		Scope<?> scope = dispatch.getScope();

		EventEnvelope envelope = new EventEnvelope(NetworkUtil.toByteArray(spaceID.getContextID()),
				NetworkUtil.toByteArray(spaceID.getID()), toBytes(scope), toBytes(dispatch.getCustomHeaders()), toBytes(event));

		this.encrypter.encrypt(envelope);

		return envelope;
	}

	private static byte[] toBytes(Object object) throws IOException {
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
			ObjectOutputStream oos = new ObjectOutputStream(baos);
			oos.writeObject(object);
			return baos.toByteArray();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public EventDispatch deserialize(EventEnvelope envelope) throws Exception {
		assert (this.encrypter != null) : "Invalid injection of the encrypter"; //$NON-NLS-1$
		assert (envelope != null) : "Parameter 'envelope' must not be null"; //$NON-NLS-1$

		this.encrypter.decrypt(envelope);

		Map<String, String> headers = fromBytes(envelope.getCustomHeaders(), Map.class);
		assert (headers != null);

		Class<?> spaceSpec = null;
		String classname = headers.get("x-java-spacespec-class"); //$NON-NLS-1$
		if (classname != null) {
			try {
				spaceSpec = Class.forName(classname);
			} catch (Throwable exception) {
				//
			}
		}

		if (spaceSpec == null || !SpaceSpecification.class.isAssignableFrom(spaceSpec)) {
			throw new ClassCastException(Locale.getString("INVALID_TYPE", spaceSpec)); //$NON-NLS-1$
		}

		UUID contextId = NetworkUtil.fromByteArray(envelope.getContextId());
		UUID spaceId = NetworkUtil.fromByteArray(envelope.getSpaceId());

		SpaceID spaceID = new SpaceID(contextId, spaceId, (Class<? extends SpaceSpecification<?>>) spaceSpec);

		Event event = fromBytes(envelope.getBody(), Event.class);
		assert (event != null);
		Scope<?> scope = fromBytes(envelope.getScope(), Scope.class);
		return new EventDispatch(spaceID, event, scope, headers);

	}

	private static <T> T fromBytes(byte[] data, Class<T> type) throws IOException, ClassNotFoundException {
		try (ByteArrayInputStream bais = new ByteArrayInputStream(data)) {
			ObjectInputStream oos = new ObjectInputStream(bais);
			Object object = oos.readObject();
			if (object != null && type.isInstance(object)) {
				return type.cast(object);
			}
			throw new ClassCastException(Locale.getString("INVALID_TYPE", //$NON-NLS-1$
					type.getName()));
		}
	}

}
