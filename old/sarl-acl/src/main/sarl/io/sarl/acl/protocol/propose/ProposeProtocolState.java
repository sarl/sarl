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

package io.sarl.acl.protocol.propose;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.acl.protocol.ProtocolState;

/**
 * This enumeration describes all available states of the Propose Protocol.
 *
 * <center><img src="./doc-files/sequence.png"></center>
 * 
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public enum ProposeProtocolState implements ProtocolState {

	/** Protocol not started.
	 */
	NOT_STARTED,

	/** Canceled.
	 */
	CANCELED,
	
	/** Protocol was broken due to an error.
	 */
	BROKEN_PROTOCOL,

	/** Initiator is waiting for the answer from the participant.
	 */
	WAITING_ANSWER,
	
	/** Participant is building the answer.
	 */
	BUILDING_ANSWER,

	/** Proposal is rejected by the participant.
	 */
	REJECT_PROPOSE,

	/** Proposal is accepted by the participant.
	 */
	ACCEPT_PROPOSE;

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
		return this == ACCEPT_PROPOSE
				|| this == REJECT_PROPOSE
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
