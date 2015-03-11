/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

/**
 * List of issues codes related to SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see org.eclipse.xtend.core.validation.IssueCodes
 */
public final class IssueCodes {

	/** Prefix related to SARL for the issue codes.
	 */
	protected static final String ISSUE_CODE_PREFIX = "io.sarl.lang.validation.IssueCodes."; //$NON-NLS-1$

	/**
	 * An interface is implemented, but it is already implemented by the super type,
	 * or inherited by another interface.
	 */
	public static final String REDUNDANT_INTERFACE_IMPLEMENTATION =
			ISSUE_CODE_PREFIX + "redundant_interface_implementation"; //$NON-NLS-1$

	/**
	 * An action must be implemented.
	 * <p>
	 * The following code causes a warning:<pre><code>
	 * capacity C1 {
	 *    def myaction
	 * }
	 * skill S2 implements C1 {
	 * }
	 * </code></pre>
	 */
	public static final String MISSING_METHOD_IMPLEMENTATION =
			ISSUE_CODE_PREFIX + "missing_method_implementation"; //$NON-NLS-1$

	/**
	 * It is discouraged to have a true/false constant as conditions in guards, if...
	 * <p>
	 * The following code causes a warning:<pre><code>
	 * event E1
	 * agent A1 {
	 *    on E1 [true] { }
	 * }
	 * </code></pre>
	 */
	public static final String DISCOURAGED_BOOLEAN_EXPRESSION =
			ISSUE_CODE_PREFIX + "discouraged_boolean_expression"; //$NON-NLS-1$

	/** A capacity was defined in a way that is discouraged.
	 * The message associated to this issue code explains the details.
	 */
	public static final String DISCOURAGED_CAPACITY_DEFINITION =
			ISSUE_CODE_PREFIX + "discouraged_capacity_definition"; //$NON-NLS-1$

	/** A behavior unit will be never executed due to its guard.
	 */
	public static final String UNREACHABLE_BEHAVIOR_UNIT =
			ISSUE_CODE_PREFIX + "unreachable_behavior_unit"; //$NON-NLS-1$

	/** A capacity type is mandatory after the "uses" and "requires" keyword.
	 */
	public static final String INVALID_CAPACITY_TYPE =
			ISSUE_CODE_PREFIX + "invalid_capacity_type"; //$NON-NLS-1$

	/** A event type is mandatory after the "fires" keyword.
	 */
	public static final String INVALID_FIRING_EVENT_TYPE =
			ISSUE_CODE_PREFIX + "invalid_firing_event_type"; //$NON-NLS-1$

	/** A type is invalid after the "implements" keyword.
	 */
	public static final String INVALID_IMPLEMENTED_TYPE =
			ISSUE_CODE_PREFIX + "invalid_implemented_type"; //$NON-NLS-1$

	/** A type is invalid after the "extends" keyword.
	 */
	public static final String INVALID_EXTENDED_TYPE =
			ISSUE_CODE_PREFIX + "invalid_extended_type"; //$NON-NLS-1$

	/** A capacity was not used in the local context.
	 */
	public static final String UNUSED_AGENT_CAPACITY =
			ISSUE_CODE_PREFIX + "unused_agent_capacity"; //$NON-NLS-1$

	/**
	 * A capacity is used, but it is already used by the current type.
	 */
	public static final String REDUNDANT_CAPACITY_USE =
			ISSUE_CODE_PREFIX + "redundant_capacity_use"; //$NON-NLS-1$

	private IssueCodes() {
		//
	}

}
