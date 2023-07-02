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

package io.sarl.acl.protocol;

import org.eclipse.xtext.xbase.lib.Pure;

/** 
 * This interface describes any protocol state.
 * 
 * @see AbstractFipaProtocol#getState()
 * 
 * @author $Author: sgalland$
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $Groupid$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public interface ProtocolState {

	/** Replies if the the protocol was started.
	 *
	 * @return {@code true} if the protocol was started.
	 */
	@Pure
	boolean isStarted();

	/** Replies if the protocol is finished with an expected termination.
	 *
	 * @return {@code true} if the protocol is finished.
	 */
	@Pure
	boolean isFinished();

	/** Replies if the protocol is finished with  a cancellation from the initiator.
	 *
	 * @return {@code true} if the protocol was canceled.
	 */
	@Pure
	boolean isCancelled();

	/** Replies if the protocol is finished with an error in the protocol sequence.
	 *
	 * @return {@code true} if the protocol has encountered an error.
	 */
	@Pure
	boolean isErrorneous();

	/** Replies if the protocol is finished with a broken protocol.
	 *
	 * @return {@code true} if the protocol is broken.
	 */
	@Pure
	boolean isBrokenProtocol();

	/** Replies the name of the state.
	 *
	 * @return the name of the protocol.
	 */
	@Pure
	String getName();

}
