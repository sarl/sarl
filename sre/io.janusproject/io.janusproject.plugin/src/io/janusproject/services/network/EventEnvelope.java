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

import java.io.Serializable;
import java.util.Arrays;

/**
 * Envelope of a message that is exchanged other the network.
 *
 * <p>
 * A {@link EventEnvelope} can be transformed into/from a {@link EventDispatch} with a {@link EventSerializer}.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see EventDispatch
 * @see EventSerializer
 */
public class EventEnvelope implements Serializable {

	private static final long serialVersionUID = 3618628129423203542L;

	private byte[] contextId;

	private byte[] spaceId;

	private byte[] scope;

	private byte[] customHeaders;

	private byte[] body;

	/**
	 * Construct an envelope.
	 *
	 * @param contextId - identifier of the context in which the event occurs.
	 * @param spaceID - identifier of the space in which the event occurs.
	 * @param scope - scope for the event.
	 * @param headers - custom header associated to the event.
	 * @param body - body of the event.
	 */
	public EventEnvelope(byte[] contextId, byte[] spaceID, byte[] scope, byte[] headers, byte[] body) {

		assert (contextId != null && contextId.length > 0) : "Parameter 'contextId' must not be null or zero-length"; //$NON-NLS-1$
		assert (spaceID != null && spaceID.length > 0) : "Parameter 'spaceID' must not be null or zero-length"; //$NON-NLS-1$
		assert (scope != null && scope.length > 0) : "Parameter 'scope' must not be null or zero-length"; //$NON-NLS-1$
		assert (headers != null && headers.length > 0) : "Parameter 'headers' must not be null or zero-length"; //$NON-NLS-1$
		assert (body != null && body.length > 0) : "Parameter 'body' must not be null or zero-length"; //$NON-NLS-1$

		this.contextId = contextId;
		this.spaceId = spaceID;
		this.scope = scope;
		this.customHeaders = headers;
		this.body = body;
	}

	/**
	 * Replies the custom header.
	 *
	 * @return the custom header.
	 */
	public byte[] getCustomHeaders() {
		return this.customHeaders;
	}

	/**
	 * Replies the body of the event.
	 *
	 * @return the body.
	 */
	public byte[] getBody() {
		return this.body;
	}

	/**
	 * Replies the identifier of the context in which the event occurs.
	 *
	 * @return the content identifier.
	 */
	public byte[] getContextId() {
		return this.contextId;
	}

	/**
	 * Replies the identifier of the space in which the event occurs.
	 *
	 * @return the space identifier.
	 */
	public byte[] getSpaceId() {
		return this.spaceId;
	}

	/**
	 * Replies the scope of the event.
	 *
	 * @return the scope.
	 */
	public byte[] getScope() {
		return this.scope;
	}

	/**
	 * Change the custom header.
	 *
	 * @param headers - the serialized header for the message
	 */
	public void setCustomHeaders(byte[] headers) {
		assert (headers != null && headers.length > 0) : "Parameter 'headers' must not be null or zero-length"; //$NON-NLS-1$
		this.customHeaders = headers;
	}

	/**
	 * Change the body of the event.
	 *
	 * @param body - the serialized message content
	 */
	public void setBody(byte[] body) {
		assert (body != null && body.length > 0) : "Parameter 'body' must not be null or zero-length"; //$NON-NLS-1$
		this.body = body;
	}

	/**
	 * Change the identifier of the context in which the event occurs.
	 *
	 * @param contextID - the serialized context identifier
	 */
	public void setContextId(byte[] contextID) {
		assert (contextID != null && contextID.length > 0) : "Parameter 'contextID' must not be null or zero-length"; //$NON-NLS-1$
		this.contextId = contextID;
	}

	/**
	 * Change the identifier of the space in which the event occurs.
	 *
	 * @param spaceID - the serialized space identifier
	 */
	public void setSpaceId(byte[] spaceID) {
		assert (spaceID != null && spaceID.length > 0) : "Parameter 'spaceID' must not be null or zero-length"; //$NON-NLS-1$
		this.spaceId = spaceID;
	}

	/**
	 * Change the scope of the event.
	 *
	 * @param scope - the serialized scope
	 */
	public void setScope(byte[] scope) {
		assert (scope != null && scope.length > 0) : "Parameter 'scope' must not be null or zero-length"; //$NON-NLS-1$
		this.scope = scope;
	}

	@Override
	public String toString() {
		return "EventEnvelope {\n  context=" //$NON-NLS-1$
				+ Arrays.toString(this.contextId) + "\n  scope=" //$NON-NLS-1$
				+ Arrays.toString(this.scope) + ",\n  spaceID=" //$NON-NLS-1$
				+ Arrays.toString(this.spaceId) + ",\n  headers=" //$NON-NLS-1$
				+ Arrays.toString(this.customHeaders) + ",\n  body=" //$NON-NLS-1$
				+ Arrays.toString(this.body) + "\n}"; //$NON-NLS-1$

	}

}
