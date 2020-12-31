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
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.eclipse.runtime;

/**
 * Exception in a SRE operation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SREException extends RuntimeException {

	private static final long serialVersionUID = 4435195594095001001L;

	/** Constructor.
	 * @param message error message.
	 */
	public SREException(String message) {
		super(message);
	}

	/** Constructor.
	 * @parma installation - the source of the exception.
	 * @param message error message.
	 * @param cause the cause of the error.
	 */
	public SREException(String message, Throwable cause) {
		super(message, cause);
	}

	/** Constructor.
	 * @parma installation - the source of the exception.
	 * @param cause the cause of the error.
	 */
	public SREException(Throwable cause) {
		super(cause);
	}

}
