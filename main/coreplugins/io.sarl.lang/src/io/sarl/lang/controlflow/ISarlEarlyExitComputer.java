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

package io.sarl.lang.controlflow;

import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.controlflow.IEarlyExitComputer;

import io.sarl.lang.sarl.SarlAction;

/** Compute the early-exit flag for the SARL statements.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ISarlEarlyExitComputer extends IEarlyExitComputer {

	/** Replies if the given event is an event that causes an early exit of the
	 * function was is calling the firing function.
	 *
	 * @param reference the event reference.
	 * @return <code>true</code> if the event may causes early exit of the function,
	 *     otherwise <code>false</code>.
	 */
	boolean isEarlyExitEvent(JvmTypeReference reference);

	/** Replies if the given statement is annotated with the "early-exit" annotation.
	 *
	 * @param element the element to test.
	 * @return <code>true</code> if the given element is annotated with the "early-flag"
	 *     annotation, otherwise <code>false</code>.
	 */
	boolean isEarlyExitAnnotatedElement(Object element);

	/** Replies if the given expression causes an early exist from a loop.
	 *
	 * @param expression the expression.
	 * @return <code>true</code> if the expression causes early exit of the loop statement,
	 *     otherwise <code>false</code>.
	 * @since 0.5
	 */
	boolean isEarlyExitLoop(XExpression expression);

	/** Replies if the given operation causes an early exist within its caller.
	 *
	 * @param operation the operation.
	 * @return <code>true</code> if the operation causes early exit of the caller,
	 *     otherwise <code>false</code>.
	 * @since 0.7
	 */
	boolean isEarlyExitOperation(SarlAction operation);

	/**
	 * An expression is considered to be left early if all branches end with an explicit
	 * termination, e.g. a return or throw expression.
	 * This functions take care only of the Java and Xbase early exit, not the SARL-specific
	 * early exist expressions.
	 *
	 * @param expression the expression to test.
	 * @return <code>true</code> if the given expression will definitely exit early in Java or Xbase.
	 * @since 0.8
	 */
	boolean isEarlyExitInJava(XExpression expression);

}

