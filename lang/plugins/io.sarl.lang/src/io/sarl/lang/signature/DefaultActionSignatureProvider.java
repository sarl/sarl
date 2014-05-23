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

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;

import com.google.inject.Singleton;

/**
 * Provides additional function signatures according the semantic
 * associated to the parameter's default values.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class DefaultActionSignatureProvider implements ActionSignatureProvider {

	private final Map<String,Map<String,Map<SignatureKey,InferredActionSignature>>> elements = new TreeMap<String, Map<String,Map<SignatureKey,InferredActionSignature>>>();

	/**
	 */
	public DefaultActionSignatureProvider() {
		//
	}

	private static Map<SignatureKey,EList<InferredStandardParameter>> buildSignaturesForArgDefaultValues(boolean varargs, List<FormalParameter> params, SignatureKey fillSignatureKeyOutputParameter) {
		Map<SignatureKey,EList<InferredStandardParameter>> signatures = new TreeMap<SignatureKey,EList<InferredStandardParameter>>();
		fillSignatureKeyOutputParameter.clear();
		if (!params.isEmpty()) {
			Map<SignatureKey,EList<InferredStandardParameter>> tmpSignatures;
			final int lastParamIndex = params.size() - 1;
			for(int i=0; i<=lastParamIndex; ++i) {
				final FormalParameter param = params.get(i);
				final boolean isOptional = (param.getDefaultValue()!=null && ((i<lastParamIndex) || (!varargs)));
				String type = param.getParameterType().getIdentifier();
				fillSignatureKeyOutputParameter.add(type);
				tmpSignatures = new TreeMap<SignatureKey,EList<InferredStandardParameter>>();
				if (signatures.isEmpty()) {
					// First parameter
					if (isOptional) {
						SignatureKey key = new SignatureKey();
						EList<InferredStandardParameter> value = ECollections.newBasicEList();
						value.add(new InferredValuedParameter(param));
						tmpSignatures.put(key, value);				
					}
					SignatureKey key = new SignatureKey();
					key.add(type);
					EList<InferredStandardParameter> value = ECollections.newBasicEList();
					value.add(new InferredStandardParameter(param));
					tmpSignatures.put(key, value);
				}
				else {
					// Other parameters
					for(Entry<SignatureKey,EList<InferredStandardParameter>> entry : signatures.entrySet()) {
						if (isOptional) {
							SignatureKey key = entry.getKey().clone();
							EList<InferredStandardParameter> value = ECollections.newBasicEList(entry.getValue());
							value.add(new InferredValuedParameter(param));
							tmpSignatures.put(key, value);
						}
						SignatureKey key = entry.getKey().clone();
						key.add(type);
						entry.getValue().add(new InferredStandardParameter(param));
						tmpSignatures.put(key, entry.getValue());
					}
				}
				signatures = tmpSignatures;
			}
			signatures.remove(fillSignatureKeyOutputParameter);
		}
		return signatures;
	}

	/** {@inheritDoc}
	 */
	public Iterable<InferredActionSignature> getSignatures(ActionNameKey id) {
		Map<String,Map<SignatureKey,InferredActionSignature>> c = this.elements.get(id.getContainerID());
		if (c!=null) {
			Map<SignatureKey,InferredActionSignature> list = c.get(id.getFunctionName());
			if (list!=null) return list.values();
		}
		return ECollections.emptyEList();
	}

	/** {@inheritDoc}
	 */
	public InferredActionSignature getSignatures(ActionNameKey actionID, SignatureKey signatureID) {
		Map<String,Map<SignatureKey,InferredActionSignature>> c = this.elements.get(actionID.getContainerID());
		if (c!=null) {
			Map<SignatureKey,InferredActionSignature> list = c.get(actionID.getFunctionName());
			if (list!=null) {
				return list.get(signatureID);
			}
		}
		return null;
	}

	/** {@inheritDoc}
	 */
	public InferredActionSignature createSignature(ActionNameKey id,
			boolean isVarargs, EList<FormalParameter> parameters) {
		SignatureKey key = new SignatureKey();
		Map<SignatureKey,EList<InferredStandardParameter>> ip = buildSignaturesForArgDefaultValues(isVarargs, parameters, key);
		InferredActionSignature s = new DefaultInferredActionSignature(
				id,
				parameters, 
				key,
				isVarargs,
				ip);
		Map<String,Map<SignatureKey,InferredActionSignature>> c = this.elements.get(id.getContainerID());
		if (c==null) {
			c = new TreeMap<String, Map<SignatureKey,InferredActionSignature>>();
			this.elements.put(id.getContainerID(), c);
		}
		Map<SignatureKey,InferredActionSignature> list = c.get(id.getFunctionName());
		if (list==null) {
			list = new TreeMap<SignatureKey,InferredActionSignature>();
			c.put(id.getFunctionName(), list);
		}
		list.put(key,s);
		return s;
	}

	/** Compute the id for a container.
	 * 
	 * @param container
	 * @return the id.
	 */
	@SuppressWarnings("static-method")
	protected String computeContainerId(JvmIdentifiableElement container) {
		return container.eResource().getURI().toString()
				+"/"+container.getQualifiedName(); //$NON-NLS-1$
	}

	/** {@inheritDoc}
	 */
	public ActionNameKey createFunctionID(JvmIdentifiableElement container,
			String functionName) {
		return new ActionNameKey(
				computeContainerId(container),
				"def_"+functionName); //$NON-NLS-1$
	}

	/** {@inheritDoc}
	 */
	public ActionNameKey createConstructorID(JvmIdentifiableElement container) {
		return new ActionNameKey(
				computeContainerId(container),
				"new"); //$NON-NLS-1$
	}

	/** {@inheritDoc}
	 */
	public SignatureKey createSignatureIDFromSarlModel(EList<FormalParameter> parameters) {
		SignatureKey sig = new SignatureKey();
		for(FormalParameter p : parameters) {
			sig.add(p.getParameterType().getIdentifier());
		}
		return sig;
	}

	/** {@inheritDoc}
	 */
	public SignatureKey createSignatureIDFromJvmModel(EList<JvmFormalParameter> parameters) {
		SignatureKey sig = new SignatureKey();
		for(JvmFormalParameter p : parameters) {
			sig.add(p.getParameterType().getIdentifier());
		}
		return sig;
	}

	/** {@inheritDoc}
	 */
	public ActionKey createActionID(String actionName, SignatureKey parameters) {
		return new ActionKey(actionName, parameters);
	}

	/** {@inheritDoc}
	 */
	public void resetSignatures(JvmIdentifiableElement container) {
		String id = computeContainerId(container);
		this.elements.remove(id);
	}

}
