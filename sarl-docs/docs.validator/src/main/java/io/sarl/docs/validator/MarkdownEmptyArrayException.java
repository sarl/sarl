/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

/** Exception that is generated when an array is empty.
 * An ampty array causes a not rendering of the markdown table. 
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.validator 0.13.0 20230919-093059
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.validator
 * @since 0.12
 */
public class MarkdownEmptyArrayException extends RuntimeException {

	private static final long serialVersionUID = -4801744735186228615L;

	/** Constructor.
	 */
	public MarkdownEmptyArrayException() {
		super(Messages.MarkdownEmptyArrayException_0);
	}
	
}
