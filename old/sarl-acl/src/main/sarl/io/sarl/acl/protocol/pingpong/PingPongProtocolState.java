/* 
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2021 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.acl.protocol.pingpong;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.acl.protocol.ProtocolState;

/**
 * This enumeration describes all available states of the Ping-Pong Protocol.
 *
 * <center><img src="./doc-files/pingpong.png"></center>
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public enum PingPongProtocolState implements ProtocolState {

	/** Protocol not started.
	 */
	NOT_STARTED,

	/** Canceled.
	 */
	CANCELED,
	
	/** Protocol was broken due to an error.
	 */
	BROKEN_PROTOCOL,

	/** The initiator has sent a sarl-ping message, and is waiting for the participant's reply.
	 */
	WAITING_ANSWER,

	/** The answer from the *participant* has been received.
	 */
	PONG_RECEIVED,

	/** The initiator has sent a sarl-ping message, and it is received by the participant.
	 * The initiator is waiting for the participant reply.
	 */
	BUILDING_ANSWER,

	/** The participant has sent the sarl-pong message.
	 */
	PONG_SENT;

	@Override
	@Pure
	public String getName() {
		return name();
	}
	
	@Override
	public boolean isStarted() {
		return this != NOT_STARTED;
	}

	@Override
	public boolean isFinished() {
		return this == PONG_RECEIVED
				|| this == PONG_SENT
				|| isCancelled() || isErrorneous();
	}

	@Override
	public boolean isCancelled() {
		return this == CANCELED;
	}

	@Override
	public boolean isErrorneous() {
		return isBrokenProtocol();
	}

	@Override
	public boolean isBrokenProtocol() {
		return this == BROKEN_PROTOCOL;
	}

}
