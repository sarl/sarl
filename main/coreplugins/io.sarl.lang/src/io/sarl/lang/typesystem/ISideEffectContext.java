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

import java.util.List;
import java.util.Map;

import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.sarl.actionprototype.InferredPrototype;

/**
 * Context for side effect detection.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public interface ISideEffectContext {

	/** Replies the operation that are being called.
	 *
	 * @return the called operations.
	 */
	List<InferredPrototype> getCalledOperations();

	/** Create a buffer for storing the variable assignments.
	 *
	 * <p>The replied buffer may be passed to {@link #branch(Map)}.
	 *
	 * @return the buffer.
	 */
	Map<String, List<XExpression>> createVariableAssignmentBufferForBranch();

	/** Merge the given variable assignment.
	 *
	 * <p>This function assumes that any previously defined value is the default value.
	 * If one of the given buffer does not redefine the variable, this default value stay in the list of
	 * the possible values for the variable.
	 *
	 * @param buffers the buffers.
	 */
	void mergeBranchVariableAssignments(List<Map<String, List<XExpression>>> buffers);

	/** Create a context that enables multiple assignments to a variable.
	 *
	 * @return the context.
	 */
	@Inline("branch(null)")
	default ISideEffectContext branch() {
		return branch(null);
	}

	/** Create a context that enables multiple assignments to a variable.
	 *
	 * @param buffer the variable assignment buffer.
	 * @return the context.
	 */
	ISideEffectContext branch(Map<String, List<XExpression>> buffer);

	/** Open a declaration context.
	 */
	void open();

	/** Close a declaration context.
	 *
	 */
	void close();

	/** Declare a local variable.
	 *
	 * @param id the identifier of the variable.
	 * @param expression initialization expression.
	 */
	void declareVariable(String id, XExpression expression);

	/** Assign a local variable.
	 *
	 * @param id the identifier of the variable.
	 * @param expression the new value.
	 */
	void assignVariable(String id, XExpression expression);

	/** Replies the values of the variable with the given nbame.
	 *
	 * @param name the name of the variable.
	 * @return the values.
	 */
	List<? extends XExpression> getVariableValues(String name);

	/** Replies if the side-effect exploration should stop at the first discovered side effect.
	 *
	 * @return {@code true} if the algorithm stops at first side-effect occurrence.
	 * @since 0.12
	 */
	@Pure
	boolean isStoppingAtFirstSideEffect();

	/** Register the given call to have a side effect.
	 *
	 * @param expression the side-effect expression.
	 * @since 0.12
	 */
	void registerSideEffect(XExpression expression);

	/** Replies the detected side effect expressions.
	 *
	 * @return the side-effect expressions.
	 * @since 0.12
	 */
	@Pure
	Iterable<XExpression> getSideEffectExpressions();

}
