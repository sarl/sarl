/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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
	 * @param operation the operation to test.
	 * @return {@code true} if the operation has a side effects.
	 * @see #isNamePatternForNotPureOperation(JvmOperation)
	 */
	boolean isNamePatternForPureOperation(JvmOperation operation);

	/** Replies if the given operation has a name which is assumed to be for a pure function by default.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation has a side effects.
	 * @see #isNamePatternForNotPureOperation(JvmOperation)
	 */
	boolean isNamePatternForPureOperation(XtendFunction operation);

	/** Replies if the given operation has a name which is assumed to be for a pure function by default.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation has no side effect.
	 * @see #isNamePatternForPureOperation(JvmOperation)
	 */
	boolean isNamePatternForNotPureOperation(JvmOperation operation);

	/** Replies if the given operation has a name which is assumed to be for a pure function by default.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation has no side effect.
	 * @see #isNamePatternForPureOperation(JvmOperation)
	 */
	boolean isNamePatternForNotPureOperation(XtendFunction operation);

}
