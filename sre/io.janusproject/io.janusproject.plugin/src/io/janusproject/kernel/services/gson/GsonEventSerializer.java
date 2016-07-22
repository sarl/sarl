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

package io.janusproject.kernel.services.gson;

import java.lang.reflect.Type;
import java.util.Map;
import java.util.UUID;

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
import org.arakhne.afc.vmutil.locale.Locale;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.Scope;
import io.sarl.lang.core.SpaceID;
import io.sarl.lang.core.SpaceSpecification;

/**
 * Serialize the {@link EventDispatch} content using GSON to generate the corresponding {@link EventEnvelope}.
 *
 * <p>
 * This implementation assumes that an {@link EventEncrypter} and {@link Gson} are injected.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class GsonEventSerializer extends AbstractEventSerializer {

	/**
	 * Gson serializer.
	 */
	protected final Gson gson;

	/**
	 * Constructs an GsonEventSerializer. The {@link Gson} is injected. The {@link EventEncrypter} is injected.
	 *
	 * @param gson - the instance of the Gson engine to use.
	 * @param encrypter - the object that permits to encrypter the events.
	 */
	@Inject
	public GsonEventSerializer(Gson gson, EventEncrypter encrypter) {
		super(encrypter);
		this.gson = gson;
	}

	@Override
	public EventEnvelope serialize(EventDispatch dispatch) throws Exception {
		assert (this.encrypter != null) : "Invalid injection of the encrypter"; //$NON-NLS-1$
		assert (this.gson != null) : "Invalid injection of Gson"; //$NON-NLS-1$
		assert (dispatch != null) : "Parameter 'dispatch' must not be null"; //$NON-NLS-1$

		Event event = dispatch.getEvent();
		assert (event != null);
		Scope<?> scope = dispatch.getScope();
		assert (scope != null);
		SpaceID spaceID = dispatch.getSpaceID();
		assert (spaceID != null);
		assert (spaceID.getSpaceSpecification() != null);
		Map<String, String> headers = dispatch.getCustomHeaders();
		assert (headers != null);

		headers.put("x-java-event-class", //$NON-NLS-1$
				event.getClass().getName());
		headers.put("x-java-scope-class", //$NON-NLS-1$
				scope.getClass().getName());
		headers.put("x-java-spacespec-class", //$NON-NLS-1$
				spaceID.getSpaceSpecification().getName());

		EventEnvelope envelope = new EventEnvelope(NetworkUtil.toByteArray(spaceID.getContextID()),
				NetworkUtil.toByteArray(spaceID.getID()),
				this.gson.toJson(scope).getBytes(NetworkConfig.getStringEncodingCharset()),
				this.gson.toJson(dispatch.getCustomHeaders()).getBytes(NetworkConfig.getStringEncodingCharset()),
				this.gson.toJson(event).getBytes(NetworkConfig.getStringEncodingCharset()));

		this.encrypter.encrypt(envelope);

		return envelope;

	}

	private static Map<String, String> getHeadersFromString(String src) {
		Gson gson = new Gson();
		Type headersType = new TypeToken<Map<String, String>>() {
			//
		}.getType();
		return gson.fromJson(src, headersType);

	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public EventDispatch deserialize(EventEnvelope envelope) throws Exception {
		assert (this.encrypter != null) : "Invalid injection of the encrypter"; //$NON-NLS-1$
		assert (this.gson != null) : "Invalid injection of Gson"; //$NON-NLS-1$

		this.encrypter.decrypt(envelope);

		UUID contextId = NetworkUtil.fromByteArray(envelope.getContextId());
		UUID spaceId = NetworkUtil.fromByteArray(envelope.getSpaceId());

		Map<String, String> headers = getHeadersFromString(
				new String(envelope.getCustomHeaders(), NetworkConfig.getStringEncodingCharset()));

		Class<? extends SpaceSpecification> spaceSpec = extractClass("x-java-spacespec-class", headers, SpaceSpecification.class); //$NON-NLS-1$
		Class<? extends Event> eventClazz = extractClass("x-java-event-class", headers, Event.class); //$NON-NLS-1$
		Class<? extends Scope> scopeClazz = extractClass("x-java-scope-class", headers, Scope.class); //$NON-NLS-1$

		SpaceID spaceID = new SpaceID(contextId, spaceId, (Class<? extends SpaceSpecification<?>>) spaceSpec);

		Event event = this.gson.fromJson(new String(envelope.getBody(), NetworkConfig.getStringEncodingCharset()), eventClazz);
		assert (event != null);
		Scope scope = this.gson.fromJson(new String(envelope.getScope(), NetworkConfig.getStringEncodingCharset()), scopeClazz);
		assert (scope != null);

		return new EventDispatch(spaceID, event, scope, headers);
	}

	private static <T> Class<? extends T> extractClass(String key, Map<String, String> headers, Class<T> expectedType) {
		assert (key != null);
		assert (headers != null);
		String classname = headers.get(key);
		Class<?> type = null;
		if (classname != null) {
			try {
				type = Class.forName(classname);
			} catch (Throwable exception) {
				//
			}
		}
		if (type == null || !expectedType.isAssignableFrom(type)) {
			throw new ClassCastException(Locale.getString("INVALID_TYPE", type)); //$NON-NLS-1$
		}
		assert (type != null);
		return type.asSubclass(expectedType);
	}

	/**
	 * Json adapter for supporting the {@link Class} type.
	 *
	 * @author $Author: ngaud$
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
			try {
				return Class.forName(json.getAsString());
			} catch (ClassNotFoundException e) {
				throw new JsonParseException(e);
			}
		}

	}

}
