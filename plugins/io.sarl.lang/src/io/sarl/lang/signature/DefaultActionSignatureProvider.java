/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.util.ModelUtil;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.eclipse.emf.common.util.ECollections;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.XExpression;

import com.google.inject.Inject;
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

	@Inject
	private TypeReferences references;

	private final Map<String, Map<String, Map<SignatureKey, InferredActionSignature>>> elements = new TreeMap<>();

	/**
	 */
	public DefaultActionSignatureProvider() {
		//
	}

	private Map<SignatureKey, List<InferredStandardParameter>> buildParameter(
			XtendParameter param,
			int parameterIndex,
			JvmTypeReference paramType,
			XExpression paramDefaultValue,
			final int lastParamIndex,
			List<? extends XtendParameter> params,
			Map<SignatureKey, List<InferredStandardParameter>> signatures,
			SignatureKey fillSignatureKeyOutputParameter) {
		boolean isOptional = (paramDefaultValue != null
			&& ((parameterIndex < lastParamIndex)
					|| (!fillSignatureKeyOutputParameter.isVarargs())));
		String type;
		if (parameterIndex >= lastParamIndex && fillSignatureKeyOutputParameter.isVarargs()) {
			type = this.references.createArrayType(paramType).getIdentifier();
		} else {
			type = paramType.getIdentifier();
		}
		fillSignatureKeyOutputParameter.add(type);
		Map<SignatureKey, List<InferredStandardParameter>> tmpSignatures = new TreeMap<>();
		if (signatures.isEmpty()) {
			// First parameter
			if (isOptional) {
				SignatureKey key = new SignatureKey(fillSignatureKeyOutputParameter.isVarargs(), params.size());
				List<InferredStandardParameter> value = ECollections.newBasicEList();
				value.add(new InferredValuedParameter(param));
				tmpSignatures.put(key, value);
			}
			SignatureKey key = new SignatureKey(fillSignatureKeyOutputParameter.isVarargs(), params.size());
			key.add(type);
			List<InferredStandardParameter> value = ECollections.newBasicEList();
			value.add(new InferredStandardParameter(param));
			tmpSignatures.put(key, value);
		} else {
			// Other parameters
			for (Entry<SignatureKey, List<InferredStandardParameter>> entry : signatures.entrySet()) {
				if (isOptional) {
					SignatureKey key = entry.getKey().clone();
					List<InferredStandardParameter> value =
							ECollections.newBasicEList(entry.getValue());
					value.add(new InferredValuedParameter(param));
					tmpSignatures.put(key, value);
				}
				SignatureKey key = entry.getKey().clone();
				key.add(type);
				entry.getValue().add(new InferredStandardParameter(param));
				tmpSignatures.put(key, entry.getValue());
			}
		}
		return tmpSignatures;
	}

	private Map<SignatureKey, List<InferredStandardParameter>> buildSignaturesForArgDefaultValues(
						List<? extends XtendParameter> params, SignatureKey fillSignatureKeyOutputParameter) {
		Map<SignatureKey, List<InferredStandardParameter>> signatures = new TreeMap<>();
		fillSignatureKeyOutputParameter.clear();
		if (!params.isEmpty()) {
			final int lastParamIndex = params.size() - 1;
			for (int i = 0; i <= lastParamIndex; ++i) {
				XtendParameter param = params.get(i);

				XExpression paramDefaultValue = null;
				if (param instanceof SarlFormalParameter) {
					paramDefaultValue = ((SarlFormalParameter) param).getDefaultValue();
				}
				JvmTypeReference paramType = param.getParameterType();

				if (paramType != null) {
					signatures = buildParameter(
							param, i, paramType, paramDefaultValue, lastParamIndex,
							params, signatures, fillSignatureKeyOutputParameter);
				}

			}
			signatures.remove(fillSignatureKeyOutputParameter);
		}
		return signatures;
	}

	@Override
	public Iterable<InferredActionSignature> getSignatures(ActionNameKey id) {
		Map<String, Map<SignatureKey, InferredActionSignature>> c = this.elements.get(id.getContainerID());
		if (c != null) {
			Map<SignatureKey, InferredActionSignature> list = c.get(id.getFunctionName());
			if (list != null) {
				return list.values();
			}
		}
		return ECollections.emptyEList();
	}

	@Override
	public InferredActionSignature getSignatures(ActionNameKey actionID, SignatureKey signatureID) {
		Map<String, Map<SignatureKey, InferredActionSignature>> c = this.elements.get(actionID.getContainerID());
		if (c != null) {
			Map<SignatureKey, InferredActionSignature> list = c.get(actionID.getFunctionName());
			if (list != null) {
				return list.get(signatureID);
			}
		}
		return null;
	}

	@Override
	public InferredActionSignature createSignature(ActionNameKey id, List<? extends XtendParameter> parameters) {
		assert (parameters != null);
		SignatureKey key = new SignatureKey(ModelUtil.isVarArg(parameters), parameters.size());
		Map<SignatureKey, List<InferredStandardParameter>> ip = buildSignaturesForArgDefaultValues(parameters, key);
		InferredActionSignature s = new DefaultInferredActionSignature(
				id,
				parameters,
				key,
				ip);
		Map<String, Map<SignatureKey, InferredActionSignature>> c = this.elements.get(id.getContainerID());
		if (c == null) {
			c = new TreeMap<>();
			this.elements.put(id.getContainerID(), c);
		}
		Map<SignatureKey, InferredActionSignature> list = c.get(id.getFunctionName());
		if (list == null) {
			list = new TreeMap<>();
			c.put(id.getFunctionName(), list);
		}
		list.put(key, s);
		return s;
	}

	/** Compute the id for a container.
	 *
	 * @param container - the container for which the ID should be computed.
	 * @return the id.
	 */
	@SuppressWarnings("static-method")
	protected String computeContainerId(JvmIdentifiableElement container) {
		return container.eResource().getURI().toString()
				+ "/" + container.getQualifiedName(); //$NON-NLS-1$
	}

	@Override
	public ActionNameKey createFunctionID(JvmIdentifiableElement container,
			String functionName) {
		return new ActionNameKey(
				computeContainerId(container),
				"def_" + functionName); //$NON-NLS-1$
	}

	@Override
	public ActionNameKey createConstructorID(JvmIdentifiableElement container) {
		return new ActionNameKey(
				computeContainerId(container),
				"new"); //$NON-NLS-1$
	}

	@Override
	public SignatureKey createSignatureIDFromSarlModel(List<? extends XtendParameter> parameters) {
		SignatureKey sig = new SignatureKey(ModelUtil.isVarArg(parameters), parameters.size());
		for (XtendParameter p : parameters) {
			if (p.getParameterType() != null) {
				sig.add(p.getParameterType().getIdentifier());
			}
		}
		return sig;
	}

	@Override
	public SignatureKey createSignatureIDFromString(String parameters) {
		return new SignatureKey(parameters);
	}

	@Override
	public SignatureKey createSignatureIDFromJvmModel(boolean isVarargs, List<JvmFormalParameter> parameters) {
		SignatureKey sig = new SignatureKey(isVarargs, parameters.size());
		for (JvmFormalParameter p : parameters) {
			JvmTypeReference paramType = p.getParameterType();
			if (paramType != null) {
				sig.add(paramType.getIdentifier());
			}
		}
		return sig;
	}

	@Override
	public SignatureKey createSignatureID(boolean isVarargs,
			FormalParameterProvider provider) {
		int count = provider.getFormalParameterCount();
		SignatureKey sig = new SignatureKey(isVarargs, count);
		if (count > 0) {
			if (isVarargs) {
				--count;
				for (int i = 0; i < count; ++i) {
					sig.add(provider.getFormalParameterType(i, false));
				}
				sig.add(provider.getFormalParameterType(count, true));
			} else {
				for (int i = 0; i < count; ++i) {
					sig.add(provider.getFormalParameterType(i, false));
				}
			}
		}
		return sig;
	}

	@Override
	public SignatureKey createSignatureIDForVoid() {
		return new SignatureKey(false, 0);
	}

	@Override
	public ActionKey createActionID(String actionName, SignatureKey parameters) {
		return new ActionKey(actionName, parameters);
	}

	@Override
	public void resetSignatures(JvmIdentifiableElement container) {
		String id = computeContainerId(container);
		this.elements.remove(id);
	}

}
