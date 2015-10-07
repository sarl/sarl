/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.core;

import java.util.UUID;

/** Exception thrown when an agent context is unknown.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class UnknownContextException extends RuntimeException {

	private static final long serialVersionUID = 6092777787192871658L;

	private final UUID unknownContextID;

	/**
	 * @param unknownContextID - ID of the context that is unkown.
	 */
	public UnknownContextException(UUID unknownContextID) {
		super();
		this.unknownContextID = unknownContextID;
	}

	/** Replies the identifier that is used to reference the unknown context.
	 *
	 * @return the unknownContextID
	 */
	public UUID getUnknownContextID() {
		return this.unknownContextID;
	}

}
