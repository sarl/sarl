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
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.util.XExpressionHelper;

import io.sarl.lang.sarl.actionprototype.InferredPrototype;

/**
 * Helper on operations.
 *
 * <p>This implementation extends the Xtend expression helper by assuming that any function
 * with a name starting with "get", "is", "has" is a pure function.
 * It also assumes that "equals", "hashCode", "clone" and "toString" are also pure functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@ImplementedBy(SARLOperationHelper.class)
public interface IOperationHelper {

	/** Check if the given operation could be annoted with "@Pure".
	 *
	 * <p>This function is usually used by the inferrers for automatically generating the pure function
	 * annotation.
	 *
	 * <p>This function does not consider the <code>@Pure</code> annotation that is associated to the
	 * overridden function declaration. Only the current operation block is considered
	 *
	 * @param operation the operation to test.
	 * @return <code>true</code> if the given operation has not a side effect;
	 *     otherwise <code>false</code>.
	 * @see Pure
	 * @see #isPureOperation(JvmOperation)
	 */
	boolean isPurableOperation(XtendFunction operation);

	/** Check if the given operation is annoted with "@Pure".
	 *
	 * @param operation the operation to test.
	 * @return <code>true</code> if the operation is marked as pure; otherwise <code>false</code>.
	 * @see Pure
	 * @see #isPurableOperation(XtendFunction)
	 */
	boolean isPureOperation(JvmOperation operation);

	/** Replies if the given expression has a side effect in the context of its containing operation.
	 *
	 * <p>This function differs from {@link XExpressionHelper#hasSideEffects(XExpression)} because it explore the
	 * syntax tree for determining if one action has a side effect.
	 *
	 * @param calledOperation the called operation. It is used for detecting recursive calls.
	 * @param expr the expression to test, usually, the body of the expression.
	 * @return <code>true</code> if a side effect was detected into the expression.
	 * @see #getSideEffectCalls(InferredPrototype, XExpression)
	 */
	boolean hasSideEffects(InferredPrototype calledOperation, XExpression expr);

	/** Replies the list of calls that have side effects into the given expression.
	 *
	 * <p>This function differs from {@link XExpressionHelper#hasSideEffects(XExpression)} because it explore the
	 * syntax tree for determining if one action has a side effect.
	 *
	 * @param calledOperation the called operation. It is used for detecting recursive calls.
	 * @param expr the expression to test, usually, the body of the expression.
	 * @return the list of side-effect calls.
	 * @since 0.12
	 * @see #hasSideEffects(InferredPrototype, XExpression)
	 */
	Iterable<XExpression> getSideEffectExpressions(InferredPrototype calledOperation, XExpression expr);

	/** Replies if the given operation could be annotated with {@code @Pure} according
	 * to its associated adapters.
	 *
	 * @param operation the operation to update.
	 * @return {@code true} if the given operation could be annotated.
	 */
	boolean evaluatePureAnnotationAdapters(JvmOperation operation);

	/** Create an adapter for attached an pure annotation adapter to given operation.
	 *
	 * @param operation the operation to update.
	 * @param dynamicCallback the code to run for adapting the operation. This call back replies {@code true}
	 *     if the operation could be annotated with {@code @Pure}.
	 * @see Pure
	 * @see #evaluatePureAnnotationAdapters(JvmOperation)
	 */
	void attachPureAnnotationAdapter(JvmOperation operation,
			Function2<? super JvmOperation, ? super IOperationHelper, ? extends Boolean> dynamicCallback);

}
