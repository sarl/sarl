/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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


package io.sarl.docs.validator;

import java.text.MessageFormat;

/** Exception that is generated if an Xtext resource is missed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public class NoXtextResourceException extends RuntimeException {

	private static final long serialVersionUID = -4801744735186228615L;

	/** Constructor.
	 *
	 * @param lineno the line number at which the error occurred.
	 */
	public NoXtextResourceException(int lineno) {
		super(MessageFormat.format(Messages.NoXtextResourceException_0, Integer.valueOf(lineno)));
	}
	
}
