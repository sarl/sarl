/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.lang.actionprototype;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.google.common.base.Objects;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypeReferences;

import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.util.Utils;

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
public class DefaultActionPrototypeProvider implements ActionPrototypeProvider {

	@Inject
	private TypeReferences references;

	@Inject
	private SARLGrammarAccess grammarAccess;

	private final Map<String, Map<String, Map<ActionParameterTypes, InferredPrototype>>> prototypes = new TreeMap<>();

	/** Construct a provider of action prototypes.
	 */
	public DefaultActionPrototypeProvider() {
		//
	}

	private static Map<ActionParameterTypes, List<InferredStandardParameter>> buildParameter(
			int parameterIndex,
			final int lastParameterIndex,
			String argumentValue,
			FormalParameterProvider params,
			Map<ActionParameterTypes, List<InferredStandardParameter>> signatures,
			ActionParameterTypes fillSignatureKeyOutputParameter) {
		boolean isOptional = (params.hasFormalParameterDefaultValue(parameterIndex)
				&& ((parameterIndex < lastParameterIndex)
				|| (!fillSignatureKeyOutputParameter.isVarArg())));
		boolean isVarArg = (parameterIndex >= lastParameterIndex && fillSignatureKeyOutputParameter.isVarArg());
		String name = params.getFormalParameterName(parameterIndex);
		JvmTypeReference type = params.getFormalParameterTypeReference(parameterIndex, isVarArg);
		fillSignatureKeyOutputParameter.add(type.getIdentifier());
		Map<ActionParameterTypes, List<InferredStandardParameter>> tmpSignatures = new TreeMap<>();
		if (signatures.isEmpty()) {
			// First parameter
			if (isOptional) {
				ActionParameterTypes key = new ActionParameterTypes(isVarArg, 0);
				List<InferredStandardParameter> value = new ArrayList<>();
				value.add(new InferredValuedParameter(
						params.getFormalParameter(parameterIndex),
						name, type,
						argumentValue));
				tmpSignatures.put(key, value);
			}
			ActionParameterTypes key = new ActionParameterTypes(isVarArg, 1);
			key.add(type.getIdentifier());
			List<InferredStandardParameter> value = new ArrayList<>();
			value.add(new InferredStandardParameter(
					params.getFormalParameter(parameterIndex),
					name, type));
			tmpSignatures.put(key, value);
		} else {
			// Other parameters
			for (Entry<ActionParameterTypes, List<InferredStandardParameter>> entry : signatures.entrySet()) {
				if (isOptional) {
					ActionParameterTypes key = new ActionParameterTypes(isVarArg, entry.getKey().size());
					key.addAll(entry.getKey());
					List<InferredStandardParameter> value = new ArrayList<>(entry.getValue());
					value.add(new InferredValuedParameter(
							params.getFormalParameter(parameterIndex),
							name, type,
							argumentValue));
					tmpSignatures.put(key, value);
				}
				ActionParameterTypes key = new ActionParameterTypes(isVarArg, entry.getKey().size() + 1);
				key.addAll(entry.getKey());
				key.add(type.getIdentifier());
				List<InferredStandardParameter> paramList = entry.getValue();
				paramList.add(new InferredStandardParameter(
						params.getFormalParameter(parameterIndex),
						name, type));
				tmpSignatures.put(key, paramList);
			}
		}
		return tmpSignatures;
	}

	private static Map<ActionParameterTypes, List<InferredStandardParameter>> buildSignaturesForArgDefaultValues(
			String containerQualifiedName, String actionId,
			FormalParameterProvider params, ActionParameterTypes fillSignatureKeyOutputParameter) {
		Map<ActionParameterTypes, List<InferredStandardParameter>> signatures = new TreeMap<>();
		fillSignatureKeyOutputParameter.clear();
		if (params.getFormalParameterCount() > 0) {
			final int lastParamIndex = params.getFormalParameterCount() - 1;
			String prefix = containerQualifiedName + "#" + actionId.toUpperCase() + "_"; //$NON-NLS-1$//$NON-NLS-2$
			for (int i = 0; i <= lastParamIndex; ++i) {
				signatures = buildParameter(
						i,
						lastParamIndex,
						prefix + i,
						params,
						signatures,
						fillSignatureKeyOutputParameter);
			}

			List<InferredStandardParameter> parameters = signatures.get(fillSignatureKeyOutputParameter);
			if (parameters != null) {
				for (int i = 0; i < parameters.size(); ++i) {
					parameters.get(i).setDefaultValueAnnotationValue(prefix + i);
				}
			}
		}
		return signatures;
	}

	@Override
	public Iterable<InferredPrototype> getPrototypes(QualifiedActionName id) {
		Map<String, Map<ActionParameterTypes, InferredPrototype>> c = this.prototypes.get(id.getContainerID());
		if (c != null) {
			Map<ActionParameterTypes, InferredPrototype> list = c.get(id.getActionName());
			if (list != null) {
				return list.values();
			}
		}
		return Collections.emptyList();
	}

	@Override
	public InferredPrototype getPrototypes(QualifiedActionName actionID, ActionParameterTypes signatureID) {
		Map<String, Map<ActionParameterTypes, InferredPrototype>> c = this.prototypes.get(actionID.getContainerID());
		if (c != null) {
			Map<ActionParameterTypes, InferredPrototype> list = c.get(actionID.getActionName());
			if (list != null) {
				return list.get(signatureID);
			}
		}
		return null;
	}

	/** Build and replies the inferred action signature for the element with
	 * the given ID. This function creates the different signatures according
	 * to the definition, or not, of default values for the formal parameters.
	 *
	 * @param id - identifier of the function.
	 * @param isVarargs - indicates if the signature has a variatic parameter.
	 * @param parameters - list of the formal parameters of the function.
	 * @return the signature or <code>null</code> if none.
	 */
	protected InferredPrototype createPrototype(QualifiedActionName id,
			boolean isVarargs, FormalParameterProvider parameters) {
		assert (parameters != null);
		ActionParameterTypes key = new ActionParameterTypes(isVarargs, parameters.getFormalParameterCount());
		Map<ActionParameterTypes, List<InferredStandardParameter>> ip = buildSignaturesForArgDefaultValues(
				id.getDeclaringType(),
				key.toActionPrototype(id.getActionName()).toActionId(),
				parameters, key);
		List<InferredStandardParameter> op = ip.remove(key);
		InferredPrototype proto = new DefaultInferredPrototype(
				id,
				parameters,
				key,
				op,
				ip);
		String containerID = id.getContainerID();
		Map<String, Map<ActionParameterTypes, InferredPrototype>> c = this.prototypes.get(containerID);
		if (c == null) {
			c = new TreeMap<>();
			this.prototypes.put(containerID, c);
		}
		Map<ActionParameterTypes, InferredPrototype> list = c.get(id.getActionName());
		if (list == null) {
			list = new TreeMap<>();
			c.put(id.getActionName(), list);
		}
		list.put(key, proto);
		return proto;
	}

	@Override
	public final InferredPrototype createPrototypeFromSarlModel(QualifiedActionName id,
			boolean isVarargs, List<? extends XtendParameter> parameters) {
		return createPrototype(id, isVarargs,
				new SarlFormalParameterProvider(parameters, this.references));
	}

	@Override
	public final InferredPrototype createPrototypeFromJvmModel(QualifiedActionName id,
			boolean isVarargs, List<JvmFormalParameter> parameters) {
		return createPrototype(id, isVarargs, new JvmFormalParameterProvider(parameters));
	}

	@Override
	public QualifiedActionName createQualifiedActionName(JvmIdentifiableElement container,
			String functionName) {
		return new QualifiedActionName(
				container.eResource().getURI().toString(),
				container.getQualifiedName(),
				functionName);
	}

	@Override
	public QualifiedActionName createConstructorQualifiedName(JvmIdentifiableElement container) {
		return new QualifiedActionName(
				container.eResource().getURI().toString(),
				container.getQualifiedName(),
				this.grammarAccess.getConstructorAccess().getNewKeyword_3().getValue());
	}

	@Override
	public ActionParameterTypes createParameterTypesFromSarlModel(boolean isVarargs,
			List<? extends SarlFormalParameter> parameters) {
		ActionParameterTypes sig = new ActionParameterTypes(isVarargs, parameters.size());
		if (!parameters.isEmpty()) {
			int lastIndex = parameters.size() - 1;
			for (int i = 0; i < lastIndex; ++i) {
				SarlFormalParameter param = parameters.get(i);
				if (param.getParameterType() != null) {
					sig.add(param.getParameterType().getIdentifier());
				}
			}
			SarlFormalParameter param = parameters.get(lastIndex);
			if (param.getParameterType() != null) {
				JvmTypeReference type = param.getParameterType();
				if (isVarargs) {
					type = this.references.createArrayType(type);
				}
				sig.add(type.getIdentifier());
			}
		}
		return sig;
	}

	@Override
	public ActionParameterTypes createParameterTypesFromString(String parameters) {
		return new ActionParameterTypes(parameters);
	}

	@Override
	public ActionParameterTypes createParameterTypesFromJvmModel(boolean isVarargs, List<JvmFormalParameter> parameters) {
		ActionParameterTypes sig = new ActionParameterTypes(isVarargs, parameters.size());
		for (JvmFormalParameter p : parameters) {
			JvmTypeReference paramType = p.getParameterType();
			if (paramType != null) {
				sig.add(paramType.getIdentifier());
			}
		}
		return sig;
	}

	@Override
	public ActionParameterTypes createParameterTypes(boolean isVarargs,
			FormalParameterProvider provider) {
		int count = provider.getFormalParameterCount();
		ActionParameterTypes sig = new ActionParameterTypes(isVarargs, count);
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
	public ActionParameterTypes createParameterTypesForVoid() {
		return new ActionParameterTypes(false, 0);
	}

	@Override
	public ActionPrototype createActionPrototype(String actionName, ActionParameterTypes parameters) {
		return new ActionPrototype(actionName, parameters);
	}

	@Override
	public void clear(JvmIdentifiableElement container) {
		QualifiedActionName qn = createQualifiedActionName(container, null);
		this.prototypes.remove(qn.getContainerID());
	}

	@Override
	public void clear() {
		this.prototypes.clear();
	}

	@Override
	public String createFieldNameForDefaultValueID(String id) {
		int index = id.indexOf('#');
		if (index > 0) {
			return Utils.PREFIX_ATTRIBUTE_DEFAULT_VALUE + id.substring(index + 1);
		}
		return Utils.PREFIX_ATTRIBUTE_DEFAULT_VALUE + id;
	}

	@Override
	public String qualifyDefaultValueID(String containerQualifiedName, String id) {
		int index = id.indexOf('#');
		if (index > 0) {
			return id;
		}
		return containerQualifiedName + "#" + id; //$NON-NLS-1$
	}

	@Override
	public String toJavaArgument(String callerQualifiedName, String id) {
		StringBuilder b = new StringBuilder();
		int index = id.indexOf('#');
		if (index > 0) {
			String qn = id.substring(0, index);
			if (!Objects.equal(qn, callerQualifiedName)) {
				b.append(qn);
				b.append("."); //$NON-NLS-1$
			}
			b.append(Utils.PREFIX_ATTRIBUTE_DEFAULT_VALUE);
			b.append(id.substring(index + 1));
		} else {
			b.append(Utils.PREFIX_ATTRIBUTE_DEFAULT_VALUE);
			b.append(id);
		}
		return b.toString();
	}

}
