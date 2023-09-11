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

package io.sarl.acl.protocol.requestwhen;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.acl.protocol.ProtocolState;

/**
 * This enumeration describes all available states of the Request-When Protocol.
 *
 * <center><img src="./doc-files/sequence.png"></center>
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public enum RequestWhenProtocolState implements ProtocolState {

	/** Protocol not started.
	 */
	NOT_STARTED,

	/** Canceled.
	 */
	CANCELED,
	
	/** Protocol was broken due to an error.
	 */
	BROKEN_PROTOCOL,

	/** The participant received a request and is deciding if it refuse or accept.
	 */
	WAITING_PARTICIPANT_DECISION,
	
	/** Participant has refused the request.
	 */
	PARTICIPANT_REFUSED,

	/** Participant has accepted the request.
	 */
	PARTICIPANT_AGREED,
	
	/** The participant is doing the requested task.
	 */
	PARTICIPANT_PROCEED,

	/** Task is done by participant without a result.
	 */
	DONE_WITHOUT_RESULT,

	/** Task is done by participant with a result.
	 */
	DONE_WITH_RESULT,

	/** Finished with failure.
	 */
	PARTICIPANT_FAILURE;

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
		return this == DONE_WITH_RESULT || this == DONE_WITHOUT_RESULT
				|| this == PARTICIPANT_REFUSED
				|| isCancelled() || isErrorneous();
	}

	@Override
	public boolean isCancelled() {
		return this == CANCELED;
	}

	@Override
	public boolean isErrorneous() {
		return this == PARTICIPANT_FAILURE || isBrokenProtocol();
	}

	@Override
	public boolean isBrokenProtocol() {
		return this == BROKEN_PROTOCOL;
	}

}
