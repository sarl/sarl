/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.signature;

import java.util.List;
import java.util.Map;

import org.eclipse.xtend.core.xtend.XtendParameter;

/**
 * Provides additional function signatures according the semantic
 * associated to the parameter's default values.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface InferredActionSignature extends Iterable<List<InferredStandardParameter>> {

	/** Replies the key used to stored this signature in a {@link ActionSignatureProvider}.
	 *
	 * @return the key.
	 */
	ActionNameKey getKey();

	/** Replies the signatures that were inferred according to the semantic
	 * of the parameter's default value.
	 *
	 * @return the inferred signatures.
	 */
	Map<SignatureKey, List<InferredStandardParameter>> getInferredSignatures();

	/** Replies an iterator on the original and inferred signature keys.
	 *
	 * @return all signatures.
	 */
	Iterable<SignatureKey> signatureKeys();

	/** Replies the parameters that are NOT inferred.
	 *
	 * @return the parameters.
	 */
	List<? extends XtendParameter> getFormalParameters();

	/** Replies key associated to the parameters.
	 *
	 * @return the key.
	 */
	SignatureKey getFormalParameterKey();

	/** Replies if the signature has a vararg.
	 *
	 * @return <code>true</code> if the last parameter is a vararg, <code>false</code>
	 * otherwise.
	 */
	boolean isVarargs();

	/** Replies the formatted list of parameters.
	 *
	 * @return the parameter list.
	 */
	@Override
	String toString();

	/** Replies the formatted list of parameters with an associated function name.
	 *
	 * @param functionName - the name of the function to be put in the replied string.
	 * @return the parameter list.
	 */
	String toString(String functionName);

}
