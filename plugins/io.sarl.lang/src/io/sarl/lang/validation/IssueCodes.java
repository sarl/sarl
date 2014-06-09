/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
 */
public class IssueCodes {

	/**
	 */
	protected static final String ISSUE_CODE_PREFIX = "io.sarl.lang.validation.IssueCodes."; //$NON-NLS-1$

	/**
	 * A valid JDK version is not found on the classpath.
	 */
	public static final String JDK_NOT_ON_CLASSPATH = ISSUE_CODE_PREFIX+"jdk.not.on.classpath"; //$NON-NLS-1$
	
	/**
	 * A valid Xtext version is not found on the classpath.
	 */
	public static final String XBASE_LIB_NOT_ON_CLASSPATH = ISSUE_CODE_PREFIX+"xbase.lib.not.on.classpath"; //$NON-NLS-1$
	
	/**
	 * A variadic function cannot have a default value for its variadic parameter.
	 * <p>
	 * The following code is avoid:<pre><code>
	 * def myaction(a : int = 45...) {}
	 * </code></pre>
	 */
	public static final String INVALID_USE_OF_VAR_ARG = ISSUE_CODE_PREFIX + "invalid_use_of_varArg"; //$NON-NLS-1$
	
	/**
	 * The definitions of two actions are conflicting.
	 * <p>
	 * The following code is avoid:<pre><code>
	 * {
	 *    def myaction(a : int, b : int, c : int)
	 *    def myaction(a : int, b : int, c : int)
	 * }
	 * </code></pre>
	 */
	public static final String DUPLICATE_METHOD = ISSUE_CODE_PREFIX + "duplicate_method"; //$NON-NLS-1$

	/**
	 * The definitions of two fields are conflicting.
	 * <p>
	 * The following code is avoid:<pre><code>
	 * {
	 *    val myfield
	 *    val myfield
	 * }
	 * </code></pre>
	 */
	public static final String DUPLICATE_FIELD = ISSUE_CODE_PREFIX + "duplicate_field"; //$NON-NLS-1$

	/**
	 * Some names for actions are prohibited, eg. the action names starting with
	 * "_handle_" are restricted to the event handlers that are generated.
	 * <p>
	 * The following code is avoid:<pre><code>
	 * behavior B1 {
	 *    def _handle_myaction(a : int, b : int=4, c : int)
	 * }
	 * </code></pre>
	 * </code></pre>
	 * <p>
	 * Some names for attributes are prohibited, eg. the attribute names starting with
	 * "___FORMAL_PARAMETER_DEFAULT_VALUE_" are restricted to the default values for
	 * the formal parameters of the actions.
	 * <p>
	 * The following code is avoid:<pre><code>
	 * behavior B1 {
	 *    var ___FORMAL_PARAMETER_DEFAULT_VALUE_MYFIELD = 3
	 * }
	 * </code></pre>
	 */
	public static final String INVALID_MEMBER_NAME = ISSUE_CODE_PREFIX + "invalid_member_name"; //$NON-NLS-1$

	/**
	 * An interface is implemented, but it is already implemented by the super type,
	 * or inherited by another interface.
	 */
	public static final String REDUNDANT_INTERFACE_IMPLEMENTATION = ISSUE_CODE_PREFIX+"redundant_interface_implementation"; //$NON-NLS-1$

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
	public static final String MISSING_METHOD_IMPLEMENTATION = ISSUE_CODE_PREFIX+"missing_method_implementation"; //$NON-NLS-1$

	/**
	 * It is not allowed to override a final type.
	 * It is not allowed to override an inherited final operation.
	 */
	public static final String OVERRIDDEN_FINAL = ISSUE_CODE_PREFIX + "overridden_final"; //$NON-NLS-1$

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
	public static final String DISCOURAGED_BOOLEAN_EXPRESSION = ISSUE_CODE_PREFIX+"discouraged_boolean_expression"; //$NON-NLS-1$

	/**
	 * The name of the package should corresponds the real directory where
	 * the file is stored.
	 */
	public static final String WRONG_PACKAGE = ISSUE_CODE_PREFIX + "wrong_package"; //$NON-NLS-1$

	/** It is not allowed to duplicate the types.
	 */
	public static final String DUPLICATE_TYPE_NAME = ISSUE_CODE_PREFIX + "duplicate_class"; //$NON-NLS-1$
	
	/** The type hierarchy is inconsistent.
	 */
	public static final String INCONSISTENT_TYPE_HIERARCHY = ISSUE_CODE_PREFIX + "inconsistent_type_hierarchy"; //$NON-NLS-1$
}
