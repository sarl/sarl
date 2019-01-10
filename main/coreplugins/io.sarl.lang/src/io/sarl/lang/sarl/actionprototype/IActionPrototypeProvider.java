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

package io.sarl.lang.sarl.actionprototype;

import java.util.List;

import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;

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
	 * @param isVarargs indicates if the signature has a variatic parameter.
	 * @param parameters the list of the formal parameter to put in the signature key.
	 * @return the list of the parameters' types.
	 */
	ActionParameterTypes createParameterTypesFromSarlModel(boolean isVarargs, List<? extends SarlFormalParameter> parameters);

	/** Build an identifier with the given parameters.
	 *
	 * @param isVarargs indicates if the signature has a variatic parameter.
	 * @param parameters the list of the formal parameter to put in the signature key.
	 * @return the list of the parameters' types.
	 */
	ActionParameterTypes createParameterTypesFromJvmModel(boolean isVarargs, List<JvmFormalParameter> parameters);

	/** Build an identifier with the given parameters.
	 *
	 * @param isVarargs indicates if the signature has a variatic parameter.
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

	/** Reset all the prototypes associated to the given container.
	 *
	 * @param container the element for which the prototype store must be reset.
	 */
	void clear(JvmIdentifiableElement container);

	/** Reset all the prototypes.
	 */
	void clear();

	/** Build and replies the inferred action signature for the element with
	 * the given ID. This function creates the different signatures according
	 * to the definition, or not, of default values for the formal parameters.
	 *
	 * @param id identifier of the function.
	 * @param isVarargs indicates if the signature has a variatic parameter.
	 * @param parameters list of the formal parameters of the function.
	 * @return the signature or <code>null</code> if none.
	 */
	InferredPrototype createPrototypeFromSarlModel(QualifiedActionName id, boolean isVarargs,
			List<? extends XtendParameter> parameters);

	/** Build and replies the inferred action signature for the element with
	 * the given ID. This function creates the different signatures according
	 * to the definition, or not, of default values for the formal parameters.
	 *
	 * @param id identifier of the function.
	 * @param isVarargs indicates if the signature has a variatic parameter.
	 * @param parameters list of the formal parameters of the function.
	 * @return the signature or <code>null</code> if none.
	 */
	InferredPrototype createPrototypeFromJvmModel(QualifiedActionName id, boolean isVarargs, List<JvmFormalParameter> parameters);

	/** Replies the inferred action signatures for the element with
	 * the given ID.
	 *
	 * @param id the ID of the action.
	 * @return the signature, never <code>null</code>.
	 */
	Iterable<InferredPrototype> getPrototypes(QualifiedActionName id);

	/** Replies the inferred action signature for the given IDs.
	 *
	 * @param actionID the ID of the action.
	 * @param signatureID ID of the signature.
	 * @return the signature or <code>null</code> if none.
	 */
	InferredPrototype getPrototypes(QualifiedActionName actionID, ActionParameterTypes signatureID);

	/** Replies the name of the field that should store the default value associated to the parameter with the given id.
	 *
	 * @param id the parameter's identifier.
	 * @return the field's name.
	 */
	String createFieldNameForDefaultValueID(String id);

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
	 * @return the default value, or <code>null</code> if none.
	 */
	String extractDefaultValueString(JvmFormalParameter parameter);

}
