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

package io.janusproject.kernel.services.gson;

import java.lang.reflect.Type;
import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.Map;
import java.util.UUID;

import com.google.common.base.Strings;
import com.google.gson.Gson;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import com.google.gson.reflect.TypeToken;
import com.google.inject.Inject;

import io.janusproject.services.network.AbstractEventSerializer;
import io.janusproject.services.network.EventDispatch;
import io.janusproject.services.network.EventEncrypter;
import io.janusproject.services.network.EventEnvelope;
import io.janusproject.services.network.NetworkConfig;
import io.janusproject.services.network.NetworkUtil;
import io.janusproject.util.ClassFinder;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;

/**
 * Serialize the {@link EventDispatch} content using GSON to generate the corresponding {@link EventEnvelope}.
 * Caution: the Gson serializer is not able to serialize lambda expressions.
 *
 * <p>This implementation assumes that an {@link EventEncrypter} and {@link Gson} are injected.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class GsonEventSerializer extends AbstractEventSerializer {

	private static final String X_ENCODING = "x-encoding"; //$NON-NLS-1$

	private static final String X_SPACESPEC_CLASS = "x-java-spacespec-class"; //$NON-NLS-1$

	private static final String X_EVENT_CLASS = "x-java-event-class"; //$NON-NLS-1$

	private static final String X_SCOPE_CLASS = "x-java-scope-class"; //$NON-NLS-1$

	/**
	 * Gson serializer.
	 */
	protected final Gson gson;

	/**
	 * Constructs an GsonEventSerializer. The {@link Gson} is injected. The {@link EventEncrypter} is injected.
	 *
	 * @param gson the instance of the Gson engine to use.
	 * @param encrypter the object that permits to encrypter the events.
	 */
	@Inject
	public GsonEventSerializer(Gson gson, EventEncrypter encrypter) {
		super(encrypter);
		this.gson = gson;
	}

	@Override
	public EventEnvelope serialize(EventDispatch dispatch) throws Exception {
		assert this.encrypter != null : "Invalid injection of the encrypter"; //$NON-NLS-1$
		assert this.gson != null : "Invalid injection of Gson"; //$NON-NLS-1$
		assert dispatch != null : "Parameter 'dispatch' must not be null"; //$NON-NLS-1$

		final Event event = dispatch.getEvent();
		assert event != null;
		final Scope<?> scope = dispatch.getScope();
		assert scope != null;
		final SpaceID spaceID = dispatch.getSpaceID();
		assert spaceID != null;
		assert spaceID.getSpaceSpecification() != null;
		final Map<String, String> headers = dispatch.getCustomHeaders();
		assert headers != null;

		final Charset encoding = NetworkConfig.getStringEncodingCharset();

		headers.put(X_ENCODING,
				encoding.name());
		headers.put(X_EVENT_CLASS,
				event.getClass().getName());
		headers.put(X_SCOPE_CLASS,
				scope.getClass().getName());
		headers.put(X_SPACESPEC_CLASS,
				spaceID.getSpaceSpecification().getName());

		final byte[] serializedContextID = NetworkUtil.toByteArray(spaceID.getContextID());
		final byte[] serializedSpaceID = NetworkUtil.toByteArray(spaceID.getID());
		final byte[] serializedHeaders = this.gson.toJson(dispatch.getCustomHeaders()).getBytes(encoding);
		final byte[] serializedScope = this.gson.toJson(scope).getBytes(encoding);
		final byte[] serializedEvent = this.gson.toJson(event).getBytes(encoding);
		final EventEnvelope envelope = new EventEnvelope(
				serializedContextID, serializedSpaceID, serializedScope, serializedHeaders, serializedEvent);

		this.encrypter.encrypt(envelope);

		return envelope;

	}

	private static Map<String, String> getHeadersFromString(String src) {
		final Gson gson = new Gson();
		final Type headersType = new TypeToken<Map<String, String>>() {
			//
		}.getType();
		return gson.fromJson(src, headersType);

	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public EventDispatch deserialize(EventEnvelope envelope) throws Exception {
		assert this.encrypter != null : "Invalid injection of the encrypter"; //$NON-NLS-1$
		assert this.gson != null : "Invalid injection of Gson"; //$NON-NLS-1$

		this.encrypter.decrypt(envelope);

		final UUID contextId = NetworkUtil.fromByteArray(envelope.getContextId());
		final UUID spaceId = NetworkUtil.fromByteArray(envelope.getSpaceId());

		final Map<String, String> headers = getHeadersFromString(
				new String(envelope.getCustomHeaders(), NetworkConfig.getStringEncodingCharset()));

		final String encodingName = headers.get(X_ENCODING);
		Charset encoding = null;
		if (!Strings.isNullOrEmpty(encodingName)) {
			try {
				encoding = Charset.forName(encodingName);
			} catch (Throwable exception) {
				//
			}
		}
		if (encoding == null) {
			encoding = NetworkConfig.getStringEncodingCharset();
		}

		final Class<? extends SpaceSpecification> spaceSpec = extractClass(X_SPACESPEC_CLASS, headers, SpaceSpecification.class);
		final Class<? extends Event> eventClazz = extractClass(X_EVENT_CLASS, headers, Event.class);
		final Class<? extends Scope> scopeClazz = extractClass(X_SCOPE_CLASS, headers, Scope.class);

		final SpaceID spaceID = new SpaceID(contextId, spaceId, (Class<? extends SpaceSpecification<?>>) spaceSpec);

		final Event event = this.gson.fromJson(new String(envelope.getBody(), encoding), eventClazz);
		assert event != null;

		final Scope scope = this.gson.fromJson(new String(envelope.getScope(), encoding), scopeClazz);
		assert scope != null;

		return new EventDispatch(spaceID, event, scope, headers);
	}

	private static <T> Class<? extends T> extractClass(String key, Map<String, String> headers, Class<T> expectedType) {
		assert key != null;
		assert headers != null;
		final String classname = headers.get(key);
		Class<?> type = null;
		if (classname != null) {
			type = ClassFinder.findClass(classname);
		}
		if (type == null) {
			throw new RuntimeException(new ClassNotFoundException(MessageFormat.format(Messages.GsonEventSerializer_0, classname)));
		}
		if (!expectedType.isAssignableFrom(type)) {
			throw new ClassCastException(MessageFormat.format(Messages.GsonEventSerializer_1, classname, expectedType.getName()));
		}
		assert type != null;
		return type.asSubclass(expectedType);
	}

	/**
	 * Json adapter for supporting the {@link Class} type.
	 *
	 * @author $Author: ngaud$
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ClassTypeAdapter implements JsonSerializer<Class<?>>, JsonDeserializer<Class<?>> {

		@Override
		public JsonElement serialize(Class<?> src, Type typeOfSrc, JsonSerializationContext context) {
			return new JsonPrimitive(src.getName());
		}

		@Override
		public Class<?> deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
				throws JsonParseException {
			final String className = json.getAsString();
			final Class<?> type = ClassFinder.findClass(className);
			if (type == null) {
				throw new JsonParseException(new ClassNotFoundException(className));
			}
			return type;
		}

	}

}
