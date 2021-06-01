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
	 * It is discouraged to have a true/false constant as conditions in guards, if...
	 *
	 * <p>The following code causes a warning:<pre><code>
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

	/** A function was defined with a name that is discouraged.
	 */
	public static final String DISCOURAGED_FUNCTION_NAME =
			ISSUE_CODE_PREFIX + "discouraged_function_name"; //$NON-NLS-1$

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

	/** A capacity was not used in the local context.
	 */
	public static final String UNUSED_AGENT_CAPACITY =
			ISSUE_CODE_PREFIX + "unused_agent_capacity"; //$NON-NLS-1$

	/**
	 * A capacity is used, but it is already used by the current type.
	 */
	public static final String REDUNDANT_CAPACITY_USE =
			ISSUE_CODE_PREFIX + "redundant_capacity_use"; //$NON-NLS-1$

	/**
	 * A supertype is not a subtype of the expected type.
	 */
	public static final String INVALID_EXTENDED_TYPE =
			ISSUE_CODE_PREFIX + "invalid_extended_type"; //$NON-NLS-1$

	/**
	 * An element cannot be defined as a nested element.
	 */
	public static final String INVALID_NESTED_DEFINITION =
			ISSUE_CODE_PREFIX + "invalid_nested_definition"; //$NON-NLS-1$

	/**
	 * The specification of the return type is recommended.
	 */
	public static final String RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED =
			ISSUE_CODE_PREFIX + "return_type_specification_is_recommended"; //$NON-NLS-1$

	/**
	 * The SARL library is not on the classpath.
	 */
	public static final String SARL_LIB_NOT_ON_CLASSPATH =
			ISSUE_CODE_PREFIX + "sarl_lib_not_on_classpath"; //$NON-NLS-1$

	/**
	 * The SARL library found on the classpath is not compatible with the compiler's version.
	 */
	public static final String INVALID_SARL_LIB_ON_CLASSPATH =
			ISSUE_CODE_PREFIX + "invalid_sarl_lib_on_classpath"; //$NON-NLS-1$

	/**
	 * A SARL annotation is used. They are supposed to never be in the SARL code directly.
	 */
	public static final String USED_RESERVED_SARL_ANNOTATION =
			ISSUE_CODE_PREFIX + "use_reserved_sarl_annotation"; //$NON-NLS-1$

	/**
	 * A SARL annotation is used. They are supposed to never be in the SARL code directly.
	 * @since 0.12
	 */
	public static final String PROGRAMMATIC_ISSUE_ANNOTATION =
			ISSUE_CODE_PREFIX + "programmatic_issue_annotation"; //$NON-NLS-1$

	/**
	 * The occurrence use is invalid because the occurrence is a read-only variable.
	 * @since 0.5
	 */
	public static final String INVALID_OCCURRENCE_READONLY_USE =
			ISSUE_CODE_PREFIX + "invalid_occurrence_readonly_use"; //$NON-NLS-1$

	/**
	 * The occurrence use is invalid because the occurrence is a read-only variable.
	 * @since 0.5
	 */
	public static final String DISCOURAGED_OCCURRENCE_READONLY_USE =
			ISSUE_CODE_PREFIX + "discouraged_occurrence_readonly_use"; //$NON-NLS-1$

	/**
	 * The use of the {@code @Inline} annotation inside the SARL code may cause errornous side-effects.
	 * @since 0.5
	 */
	public static final String MANUAL_INLINE_DEFINITION =
			ISSUE_CODE_PREFIX + "manual_inline_definition"; //$NON-NLS-1$

	/**
	 * The use of the "break" or "continue" statement is discouraged at this location.
	 * @since 0.5
	 */
	public static final String DISCOURAGED_LOOP_BREAKING_KEYWORD_USE =
			ISSUE_CODE_PREFIX + "discouraged_loop_breaking_keyword_use"; //$NON-NLS-1$

	/**
	 * Invalid use of the "break" or "continue" keyword.
	 * @since 0.5
	 */
	public static final String INVALID_USE_OF_LOOP_BREAKING_KEYWORD =
			ISSUE_CODE_PREFIX + "invalid_use_of_loop_breaking_keyword"; //$NON-NLS-1$

	/**
	 * Invalid extra-language generation.
	 * @since 0.6
	 */
	public static final String INVALID_EXTRA_LANGUAGE_GENERATION =
			ISSUE_CODE_PREFIX + "invalid_extra_language_generation"; //$NON-NLS-1$

	/**
	 * The formal parameter is not expected at this location.
	 * @since 0.6
	 */
	public static final String UNEXPECTED_FORMAL_PARAMETER =
			ISSUE_CODE_PREFIX + "unexpected_formal_parameter"; //$NON-NLS-1$

	/**
	 * The exception throw is not expected at this location.
	 * @since 0.6
	 */
	public static final String UNEXPECTED_EXCEPTION_THROW =
			ISSUE_CODE_PREFIX + "unexpected_exception_throw"; //$NON-NLS-1$

	/**
	 * A body is missed.
	 * @since 0.6
	 */
	public static final String MISSING_BODY =
			ISSUE_CODE_PREFIX + "missing_body"; //$NON-NLS-1$

	/**
	 * The {@code @DefaultSkill} annotation has an improper value.
	 * @since 0.7
	 */
	public static final String INVALID_DEFAULT_SKILL_ANNOTATION =
			ISSUE_CODE_PREFIX + "invalid_default_skill_annotation"; //$NON-NLS-1$

	/**
	 * A synchronization problem may be encountered on the field.
	 * @since 0.7
	 */
	public static final String POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM =
			ISSUE_CODE_PREFIX + "potential_field_synchronization_problem"; //$NON-NLS-1$

	/**
	 * Internal error.
	 * @since 0.8
	 */
	public static final String INTERNAL_ERROR =
			ISSUE_CODE_PREFIX + "internal_error"; //$NON-NLS-1$

	/**
	 * The cast operator is linked to a method for converting a value that may be inefficient.
	 * @since 0.9
	 */
	public static final String POTENTIAL_INEFFICIENT_VALUE_CONVERSION =
			ISSUE_CODE_PREFIX + "potential_inefficient_value_conversion"; //$NON-NLS-1$

	/**
	 * A generic type name is shadowing another generic type name.
	 * @since 0.12
	 */
	public static final String GENERIC_TYPE_NAME_SHADOWING =
			ISSUE_CODE_PREFIX + "generic_type_name_shadowing"; //$NON-NLS-1$

	/**
	 * The value of a default parameter is inherited and locally redefined.
	 * @since 0.12
	 */
	public static final String PARAMETER_DEFAULT_VALUE_REDFINITION =
			ISSUE_CODE_PREFIX + "parameter_default_value_redefinition"; //$NON-NLS-1$

	/**
	 * The value of a default parameter does not correspond to the value of the inherited parameter.
	 * @since 0.12
	 */
	public static final String ILLEGAL_PARAMETER_DEFAULT_VALUE_REDEFINITION =
			ISSUE_CODE_PREFIX + "illegal_parameter_default_value_redefinition"; //$NON-NLS-1$

	/**
	 * A static field may enable to share data between components outside the control of the agent.
	 * It brokes the agent autonomy principle.
	 *
	 * @since 0.7
	 */
	public static final String POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL =
			ISSUE_CODE_PREFIX + "potential_memory_sharing_outside_agent_control"; //$NON-NLS-1$

	private IssueCodes() {
		//
	}

}
