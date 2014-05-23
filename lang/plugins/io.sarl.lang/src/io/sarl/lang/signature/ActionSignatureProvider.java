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
package io.sarl.lang.signature;

import io.sarl.lang.sarl.FormalParameter;

import org.eclipse.emf.common.util.EList;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;

/**
 * Provides additional function signatures according the semantic
 * associated to the parameter's default values.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ActionSignatureProvider {

	/** Build an identifier by on the function's name when it
	 * is located in the given container.
	 * 
	 * @param container
	 * @param functionName
	 * @return the id.
	 */
	public ActionNameKey createFunctionID(JvmIdentifiableElement container, String functionName);
	
	/** Build an identifier for a constructor of the given container.
	 * 
	 * @param container
	 * @return the id.
	 */
	public ActionNameKey createConstructorID(JvmIdentifiableElement container);
	
	/** Build an identifier for the given parameters.
	 * 
	 * @param parameters
	 * @return the id.
	 */
	public SignatureKey createSignatureIDFromSarlModel(EList<FormalParameter> parameters);

	/** Build an identifier for the given parameters.
	 * 
	 * @param parameters
	 * @return the id.
	 */
	public SignatureKey createSignatureIDFromJvmModel(EList<JvmFormalParameter> parameters);

	/** Build an identifier for the given function.
	 * 
	 * @param actionName
	 * @param parameters
	 * @return the id.
	 */
	public ActionKey createActionID(String actionName, SignatureKey parameters);

	/** Reset all the signatures associated to the given container
	 * 
	 * @param container
	 */
	public void resetSignatures(JvmIdentifiableElement container);

	/** Build and replies the inferred action signature for the element with
	 * the given ID.
	 * 
	 * @param id
	 * @param isVarargs
	 * @param parameters
	 * @return the signature or <code>null</code> if none.
	 */
	public InferredActionSignature createSignature(ActionNameKey id, boolean isVarargs, EList<FormalParameter> parameters);

	/** Replies the inferred action signature for the element with
	 * the given ID.
	 * 
	 * @param id
	 * @return the signature, never <code>null</code>.
	 */
	public Iterable<InferredActionSignature> getSignatures(ActionNameKey id);

	/** Replies the inferred action signature for the given IDs.
	 * 
	 * @param actionID
	 * @param signatureID
	 * @return the signature or <code>null</code> if none.
	 */
	public InferredActionSignature getSignatures(ActionNameKey actionID, SignatureKey signatureID);

}
