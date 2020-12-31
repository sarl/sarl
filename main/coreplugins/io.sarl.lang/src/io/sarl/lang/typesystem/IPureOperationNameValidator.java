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

package io.sarl.lang.typesystem;

import com.google.inject.ImplementedBy;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.xbase.lib.Inline;

/**
 * Test if names are for pure or not pure operations.
 *
 * <p>This implementation assumes that any function
 * with a name starting with "get", "is", "has" is a pure function.
 * It also assumes that "equals", "hashCode", "clone" and "toString" are also pure functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@ImplementedBy(PureOperationNameValidator.class)
public interface IPureOperationNameValidator {

	/** Replies if the given operation has a name which is assumed to be for a pure function by default.
	 *
	 * @param name the simple name of the operation.
	 * @return {@code true} if the operation has a side effects.
	 * @since 0.12
	 */
	boolean isNamePatternForPureOperation(String name);

	/** Replies if the given operation has a name which is assumed to be for a pure function by default.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation has a side effects.
	 * @see #isNamePatternForNotPureOperation(JvmOperation)
	 */
	@Inline("isNamePatternForPureOperation(($1).getSimpleName())")
	default boolean isNamePatternForPureOperation(JvmOperation operation) {
		return isNamePatternForPureOperation(operation.getSimpleName());
	}

	/** Replies if the given operation has a name which is assumed to be for a pure function by default.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation has a side effects.
	 * @see #isNamePatternForNotPureOperation(JvmOperation)
	 */
	@Inline("isNamePatternForPureOperation(($1).getName())")
	default boolean isNamePatternForPureOperation(XtendFunction operation)  {
		return isNamePatternForPureOperation(operation.getName());
	}

	/** Replies if the given expression has a side effect in the context of the given operation.
	 *
	 * @param name the simple name of the operation.
	 * @return {@code true} if the operation has a side effects.
	 * @since 0.12
	 */
	boolean isNamePatternForNotPureOperation(String name);

	/** Replies if the given operation has a name which is assumed to be for a pure function by default.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation has no side effect.
	 * @see #isNamePatternForPureOperation(JvmOperation)
	 */
	@Inline("isNamePatternForNotPureOperation(($1).getSimpleName())")
	default boolean isNamePatternForNotPureOperation(JvmOperation operation) {
		return isNamePatternForNotPureOperation(operation.getSimpleName());
	}

	/** Replies if the given operation has a name which is assumed to be for a pure function by default.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation has no side effect.
	 * @see #isNamePatternForPureOperation(JvmOperation)
	 */
	@Inline("isNamePatternForNotPureOperation(($1).getName())")
	default boolean isNamePatternForNotPureOperation(XtendFunction operation) {
		return isNamePatternForNotPureOperation(operation.getName());
	}

}
