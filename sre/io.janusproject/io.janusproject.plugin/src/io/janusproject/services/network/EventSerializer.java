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

package io.janusproject.services.network;

import java.util.UUID;

/**
 * A serializer of events to be published over the network.
 *
 * <p>
 * A serializer convert an {@link EventDispatch} into/from an {@link EventEnvelope}.
 *
 * <p>
 * The implementation of a serializer may use an {@link EventEncrypter} for encrypting/decrypting the envelope.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see EventDispatch
 * @see EventEnvelope
 */
public interface EventSerializer {

	/**
	 * Serialize the given event.
	 *
	 * @param dispatch - event to serialize.
	 * @return the envelope with the serialized event.
	 * @throws Exception - if error when serializing
	 */
	EventEnvelope serialize(EventDispatch dispatch) throws Exception;

	/**
	 * Deserialize the given envelope to obtain an event.
	 *
	 * @param envelope - envelope to deserialize.
	 * @return the dispatched event.
	 * @throws Exception - if error when deserializing
	 */
	EventDispatch deserialize(EventEnvelope envelope) throws Exception;

	/**
	 * Serialize the given identifier of context.
	 *
	 * @param id - identifier ti serialize.
	 * @return the byte-representation of the given identifier.
	 * @throws Exception - if error when serializing
	 */
	byte[] serializeContextID(UUID id) throws Exception;

}
