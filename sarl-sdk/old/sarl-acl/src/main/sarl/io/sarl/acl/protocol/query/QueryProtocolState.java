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

package io.sarl.acl.protocol.query;

import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.acl.protocol.ProtocolState;

/**
 * This enumeration describes all available states of the Query Protocol.
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
public enum QueryProtocolState implements ProtocolState {

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

	/** Participant is deciding if it will accept or refuse the query-if.
	 */
	WAITING_IF_DECISION,

	/** Participant is deciding if it will accept or refuse the query-ref.
	 */
	WAITING_REF_DECISION,

	/** Participant is executing the query-if.
	 */
	QUERY_IF_EXECUTION,

	/** Participant is executing the query-ref.
	 */
	QUERY_REF_EXECUTION,

	/** Query is refused.
	 */
	QUERY_REFUSED,

	/** Query was agreed by participant.
	 */
	QUERY_AGREED,

	/** Failure when executing the query.
	 */
	FAILURE,

	/** Success, results are available.
	 */
	SUCCESS;

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
		return this == SUCCESS
				|| this == QUERY_REFUSED
				|| isCancelled() || isErrorneous();
	}

	@Override
	public boolean isCancelled() {
		return this == CANCELED;
	}

	@Override
	public boolean isErrorneous() {
		return this == FAILURE || isBrokenProtocol();
	}

	@Override
	public boolean isBrokenProtocol() {
		return this == BROKEN_PROTOCOL;
	}

}
