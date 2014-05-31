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

import java.util.Map;

import org.eclipse.emf.common.util.EList;

/**
 * Provides additional function signatures according the semantic
 * associated to the parameter's default values.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface InferredActionSignature extends Iterable<EList<InferredStandardParameter>> {

	/** Replies the key used to stored this signature in a {@link ActionSignatureProvider}.
	 * 
	 * @return the key
	 */
	public ActionNameKey getKey();
	
	/** Replies the signatures that were inferred according to the semantic
	 * of the parameter's default value.
	 * 
	 * @return the inferred signatures
	 */
	public Map<SignatureKey,EList<InferredStandardParameter>> getInferredSignatures();
	
	/** Replies an iterator on the original and inferred signature keys.
	 * 
	 * @return all signatures
	 */
	public Iterable<SignatureKey> signatureKeys();

	/** Replies the parameters that are NOT inferred.
	 * 
	 * @return the parameters.
	 */
	public EList<FormalParameter> getFormalParameters();
	
	/** Replies key associated to the parameters.
	 * 
	 * @return the key.
	 */
	public SignatureKey getFormalParameterKey();

	/** Replies if the signature has a vararg.
	 * 
	 * @return <code>true</code> if the last parameter is a vararg, <code>false</code>
	 * otherwise.
	 */
	public boolean isVarargs();
	
	/** Replies the formatted list of parameters.
	 * 
	 * @return the parameter list.
	 */
	@Override
	public String toString();

	/** Replies the formatted list of parameters.
	 * 
	 * @param functionName
	 * @return the parameter list.
	 */
	public String toString(String functionName);

}
