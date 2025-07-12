/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.bspl.lang.validation;

/**
 * List of issues codes related to SARL BSPL constructs.
 *
 * @author $Author: sgalland$
 * @author $Author: stedeschi$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 * @see org.eclipse.xtend.core.validation.IssueCodes
 */
public final class IssueCodes {

	/** Prefix related to SARL for the issue codes.
	 */
	protected static final String ISSUE_CODE_PREFIX = "io.sarl.bspl.lang.validation.IssueCodes."; //$NON-NLS-1$

	/** A protocol is defined multiple times.
	 */
	public static final String DUPLICATE_PROTOCOL_NAME =
			ISSUE_CODE_PREFIX + "duplicate_protocol_name"; //$NON-NLS-1$

	/** A protocol role is missed.
	 */
	public static final String MISSED_PROTOCOL_ROLE =
			ISSUE_CODE_PREFIX + "missed_protocol_role"; //$NON-NLS-1$

	/** A protocol message is missed.
	 */
	public static final String MISSED_PROTOCOL_MESSAGE =
			ISSUE_CODE_PREFIX + "missed_protocol_message"; //$NON-NLS-1$

	/** A protocol is empty.
	 */
	public static final String EMPTY_PROTOCOL =
			ISSUE_CODE_PREFIX + "empty_protocol"; //$NON-NLS-1$

	/** A protocol message is defined multiple times in the same protocol.
	 */
	public static final String DUPLICATE_PROTOCOL_MESSAGE =
			ISSUE_CODE_PREFIX + "duplicate_protocol_message"; //$NON-NLS-1$

	/** A protocol parameter is defined multiple times in the same protocol.
	 */
	public static final String DUPLICATE_PROTOCOL_PARAMETER =
			ISSUE_CODE_PREFIX + "duplicate_protocol_parameter"; //$NON-NLS-1$

	/** A protocol argument in a message is defined multiple times in the same protocol.
	 */
	public static final String DUPLICATE_PROTOCOL_ARGUMENT =
			ISSUE_CODE_PREFIX + "duplicate_protocol_argument"; //$NON-NLS-1$

	/** A protocol role is defined multiple times in the same protocol.
	 */
	public static final String DUPLICATE_PROTOCOL_ROLE =
			ISSUE_CODE_PREFIX + "duplicate_protocol_role"; //$NON-NLS-1$

	/** A protocol role is undefined.
	 */
	public static final String UNDEFINED_PROTOCOL_ROLE =
			ISSUE_CODE_PREFIX + "undefined_protocol_role"; //$NON-NLS-1$

	/** A protocol parameter is undefined.
	 */
	public static final String UNDEFINED_PROTOCOL_PARAMETER =
			ISSUE_CODE_PREFIX + "undefined_protocol_parameter"; //$NON-NLS-1$

	/** A type for a parameter is missed.
	 */
	public static final String MISSED_PARAMETER_TYPE =
			ISSUE_CODE_PREFIX + "missed_parameter_type"; //$NON-NLS-1$

	/** A package declaration is recommended.
	 */
	public static final String EMPTY_PACKAGE_DECLARATION =
			ISSUE_CODE_PREFIX + "empty_package_declaration"; //$NON-NLS-1$

	/** A package declaration is not defined with the expected value.
	 */
	public static final String UNEXPECTED_PACKAGE_DECLARATION =
			ISSUE_CODE_PREFIX + "unexpected_package_declaration"; //$NON-NLS-1$

	/** Unnecessary cardinality value for a role.
	 */
	public static final String UNNECESSARY_ROLE_CARDINALITY =
			ISSUE_CODE_PREFIX + "unnecessary_role_cardinality"; //$NON-NLS-1$

	/** The order of the min-max cardinalities of a role is not correct.
	 */
	public static final String INVALID_ROLE_CARDINALITY_ORDER =
			ISSUE_CODE_PREFIX + "invalid_role_cardinality_order"; //$NON-NLS-1$

	private IssueCodes() {
		//
	}

}
