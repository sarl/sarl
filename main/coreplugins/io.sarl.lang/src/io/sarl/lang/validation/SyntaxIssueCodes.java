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

package io.sarl.lang.validation;

/** List of issue codes for the parser.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class SyntaxIssueCodes {

	/** Prefix related to SARL for the issue codes.
	 */
	protected static final String ISSUE_CODE_PREFIX = "io.sarl.lang.parser.SyntaxIssueCodes."; //$NON-NLS-1$

	/** Identifier of the issue related to the invalid used of a reserved keyword.
	 */
	public static final String USED_RESERVED_KEYWORD = ISSUE_CODE_PREFIX + "used_reserved_keyword"; //$NON-NLS-1$

	private SyntaxIssueCodes() {
		//
	}

}
