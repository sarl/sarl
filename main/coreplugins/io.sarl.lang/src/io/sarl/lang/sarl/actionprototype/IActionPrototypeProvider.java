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

package io.sarl.lang.sarl.actionprototype;

import java.util.List;

import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.sarl.SarlFormalParameter;

/**
 * Provides additional function signatures according the semantic
 * associated to the parameter's default values.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface IActionPrototypeProvider {

	/** Build an identifier based on the function's name and a container name.
	 *
	 * @param container the container of the function.
	 * @param functionName the name of the function.
	 * @return the qualified name.
	 */
	QualifiedActionName createQualifiedActionName(JvmIdentifiableElement container, String functionName);

	/** Build an identifier for a constructor in the given container.
	 *
	 * @param container the element for which a constructor ID must be created.
	 * @return the qualified name.
	 */
	QualifiedActionName createConstructorQualifiedName(JvmIdentifiableElement container);

	/** Build an identifier for an action that has no formal parameter.
	 *
	 * @return the list of the parameters' types.
	 */
	ActionParameterTypes createParameterTypesForVoid();

	/** Build an identifier with the given parameters.
	 *
	 * @param isVarargs indicates if the signature has a variadic parameter.
	 * @param parameters the list of the formal parameter to put in the signature key.
	 * @return the list of the parameters' types.
	 */
	ActionParameterTypes createParameterTypesFromSarlModel(boolean isVarargs, List<? extends SarlFormalParameter> parameters);

	/** Build an identifier with the given parameters.
	 *
	 * @param isVarargs indicates if the signature has a variadic parameter.
	 * @param parameters the list of the formal parameter to put in the signature key.
	 * @return the list of the parameters' types.
	 */
	ActionParameterTypes createParameterTypesFromJvmModel(boolean isVarargs, List<JvmFormalParameter> parameters);

	/** Build an identifier with the given parameters.
	 *
	 * @param isVarargs indicates if the signature has a variadic parameter.
	 * @param provider the provider of the formal parameters.
	 * @return the list of the parameters' types.
	 */
	ActionParameterTypes createParameterTypes(boolean isVarargs, FormalParameterProvider provider);

	/** Build an identifier with the given parameters.
	 *
	 * <p>The given parameter must following the format of the value given
	 * by {@link ActionParameterTypes#toString()}.
	 *
	 * @param parameters the string representation of the parameters.
	 * @return the list of the parameters' types.
	 */
	ActionParameterTypes createParameterTypesFromString(String parameters);

	/** Build a prototype of an action.
	 *
	 * @param actionName the name of the action.
	 * @param parameters the ID of the parameters.
	 * @return the prototype.
	 */
	ActionPrototype createActionPrototype(String actionName, ActionParameterTypes parameters);

	/** Replies the name of the function that should store the default value associated to the parameter with the given id.
	 *
	 * @param id the parameter's identifier.
	 * @return the field's name.
	 * @since 0.12
	 */
	String createFunctionNameForDefaultValueID(String id);

	/** Qualify the default value identifier with the given container's qualified name if the ID is not qualified.
	 *
	 * @param containerQualifiedName the qualified name.
	 * @param id the parameter's identifier.
	 * @return the qualified name.
	 */
	String qualifyDefaultValueID(String containerQualifiedName, String id);

	/** Replies the calling agument associated to the parameter with the given id.
	 *
	 * @param callerQualifiedName qualified name of the type where the argument is used.
	 * @param argumentSpecification the parameter's identifier.
	 * @return the calling argument for the formal parameter.
	 */
	String toJavaArgument(String callerQualifiedName, String argumentSpecification);

	/** Replies the default value of the given formal parameter.
	 *
	 * <p>This function replies the string representation of the default value.
	 *
	 * @param parameter the parameter for which the default value should be extracted.
	 * @return the default value, or {@code null} if none.
	 */
	@Pure
	String extractDefaultValueString(JvmFormalParameter parameter);

	/** Reset all the prototypes associated to the given container.
	 *
	 * @param container the element for which the prototype store must be reset.
	 * @deprecated since 0.10, see {@link #createContext()}
	 */
	@Deprecated
	default void clear(JvmIdentifiableElement container) {
		// Do nothing
	}

	/** Reset all the prototypes.
	 *
	 * @deprecated since 0.10, see {@link #createContext()}
	 */
	@Deprecated
	default void clear() {
		// Do nothing.
	}

	/** Replies the inferred action signatures for the element with
	 * the given ID.
	 *
	 * @param context the context in which the prototype should be created.
	 * @param id the ID of the action.
	 * @return the signature, never {@code null}.
	 * @since 0.10
	 * @see #createContext()
	 */
	Iterable<InferredPrototype> getPrototypes(IActionPrototypeContext context, QualifiedActionName id);

	/** Replies the inferred action signatures for the element with
	 * the given ID.
	 *
	 * @param id the ID of the action.
	 * @return the signature, never {@code null}.
	 * @deprecated since 0.10, see {@link #getPrototypes(IActionPrototypeContext, QualifiedActionName)}
	 */
	@Deprecated
	default Iterable<InferredPrototype> getPrototypes(QualifiedActionName id) {
		final IActionPrototypeContext ctx = createContext();
		try {
			return getPrototypes(ctx, id);
		} finally {
			ctx.release();
		}
	}

	/** Replies the inferred action signature for the given IDs.
	 *
	 * @param context the context in which the prototype should be created.
	 * @param actionID the ID of the action.
	 * @param signatureID ID of the signature.
	 * @return the signature or {@code null} if none.
	 * @since 0.10
	 * @see #createContext()
	 */
	InferredPrototype getPrototypes(IActionPrototypeContext context, QualifiedActionName actionID, ActionParameterTypes signatureID);

	/** Replies the inferred action signature for the given IDs.
	 *
	 * @param actionID the ID of the action.
	 * @param signatureID ID of the signature.
	 * @return the signature or {@code null} if none.
	 * @deprecated since 0.10, see {@link #getPrototypes(IActionPrototypeContext, QualifiedActionName, ActionParameterTypes)}
	 */
	@Deprecated
	default InferredPrototype getPrototypes(QualifiedActionName actionID, ActionParameterTypes signatureID) {
		final IActionPrototypeContext ctx = createContext();
		try {
			return getPrototypes(ctx, actionID, signatureID);
		} finally {
			ctx.release();
		}
	}

	/** Build and replies the inferred action signature for the element with
	 * the given ID. This function creates the different signatures according
	 * to the definition, or not, of default values for the formal parameters.
	 *
	 * @param context the context in which the prototype should be created.
	 * @param id identifier of the function.
	 * @param isVarargs indicates if the signature has a variadic parameter.
	 * @param parameters list of the formal parameters of the function.
	 * @return the signature or {@code null} if none.
	 * @since 0.10
	 * @see #createContext()
	 */
	InferredPrototype createPrototypeFromSarlModel(IActionPrototypeContext context, QualifiedActionName id, boolean isVarargs,
			List<? extends XtendParameter> parameters);

	/** Build and replies the inferred action signature for the element with
	 * the given ID. This function creates the different signatures according
	 * to the definition, or not, of default values for the formal parameters.
	 *
	 * @param id identifier of the function.
	 * @param isVarargs indicates if the signature has a variadic parameter.
	 * @param parameters list of the formal parameters of the function.
	 * @return the signature or {@code null} if none.
	 * @deprecated since 0.10, see {@link #createPrototypeFromSarlModel(IActionPrototypeContext, QualifiedActionName, boolean, List)}
	 */
	@Deprecated
	default InferredPrototype createPrototypeFromSarlModel(QualifiedActionName id, boolean isVarargs,
			List<? extends XtendParameter> parameters) {
		final IActionPrototypeContext ctx = createContext();
		try {
			return createPrototypeFromSarlModel(ctx, id, isVarargs, parameters);
		} finally {
			ctx.release();
		}
	}

	/** Build and replies the inferred action signature for the element with
	 * the given ID. This function creates the different signatures according
	 * to the definition, or not, of default values for the formal parameters.
	 *
	 * @param context the context in which the prototype should be created.
	 * @param id identifier of the function.
	 * @param isVarargs indicates if the signature has a variadic parameter.
	 * @param parameters list of the formal parameters of the function.
	 * @return the signature or {@code null} if none.
	 * @since 0.10
	 * @see #createContext()
	 */
	InferredPrototype createPrototypeFromJvmModel(IActionPrototypeContext context, QualifiedActionName id,
			boolean isVarargs, List<JvmFormalParameter> parameters);

	/** Build and replies the inferred action signature for the element with
	 * the given ID. This function creates the different signatures according
	 * to the definition, or not, of default values for the formal parameters.
	 *
	 * @param id identifier of the function.
	 * @param isVarargs indicates if the signature has a variadic parameter.
	 * @param parameters list of the formal parameters of the function.
	 * @return the signature or {@code null} if none.
	 * @deprecated since 0.10, see {@link #createPrototypeFromJvmModel(IActionPrototypeContext, QualifiedActionName, boolean, List)}
	 */
	@Deprecated
	default InferredPrototype createPrototypeFromJvmModel(QualifiedActionName id, boolean isVarargs, List<JvmFormalParameter> parameters) {
		final IActionPrototypeContext ctx = createContext();
		try {
			return createPrototypeFromJvmModel(ctx, id, isVarargs, parameters);
		} finally {
			ctx.release();
		}
	}

	/** Create an empty context.
	 *
	 * @return the context.
	 * @since 0.10
	 */
	IActionPrototypeContext createContext();

}
