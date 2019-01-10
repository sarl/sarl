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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.annotation.DefaultValue;
import io.sarl.lang.annotation.SarlSourceCode;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.SARLAnnotationUtil;
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
public class DefaultActionPrototypeProvider implements IActionPrototypeProvider {

	@Inject
	private TypeReferences references;

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	private final Map<String, Map<String, Map<ActionParameterTypes, InferredPrototype>>> prototypes = new TreeMap<>();

	private final Map<String, Map<String, Integer>> defaultValueIDPrefixes = new TreeMap<>();

	@Inject
	private AnnotationLookup annotationFinder;

	@Inject
	private SARLAnnotationUtil annotationUtils;

	/** Construct a provider of action prototypes.
	 */
	public DefaultActionPrototypeProvider() {
		//
	}

	private static Pair<Map<ActionParameterTypes, List<InferredStandardParameter>>, Boolean> buildParameter(
			int parameterIndex,
			final int lastParameterIndex,
			String argumentValue,
			FormalParameterProvider params,
			Map<ActionParameterTypes, List<InferredStandardParameter>> signatures,
			ActionParameterTypes fillSignatureKeyOutputParameter) {
		final boolean isOptional = params.hasFormalParameterDefaultValue(parameterIndex)
				&& ((parameterIndex < lastParameterIndex)
				|| (!fillSignatureKeyOutputParameter.isVarArg()));
		final boolean isVarArg = parameterIndex >= lastParameterIndex && fillSignatureKeyOutputParameter.isVarArg();
		final String name = params.getFormalParameterName(parameterIndex);
		final JvmTypeReference type = params.getFormalParameterTypeReference(parameterIndex, isVarArg);
		final Map<ActionParameterTypes, List<InferredStandardParameter>> tmpSignatures = new TreeMap<>();
		if (type == null) {
			return new Pair<>(tmpSignatures, isOptional);
		}
		fillSignatureKeyOutputParameter.add(type.getIdentifier());
		if (signatures.isEmpty()) {
			// First parameter
			if (isOptional) {
				final ActionParameterTypes key = new ActionParameterTypes(isVarArg, 0);
				final List<InferredStandardParameter> value = new ArrayList<>();
				value.add(new InferredValuedParameter(
						params.getFormalParameter(parameterIndex),
						name, type,
						argumentValue));
				tmpSignatures.put(key, value);
			}
			final ActionParameterTypes key = new ActionParameterTypes(isVarArg, 1);
			key.add(type.getIdentifier());
			final List<InferredStandardParameter> value = new ArrayList<>();
			value.add(new InferredStandardParameter(
					params.getFormalParameter(parameterIndex),
					name, type));
			tmpSignatures.put(key, value);
		} else {
			// Other parameters
			for (final Entry<ActionParameterTypes, List<InferredStandardParameter>> entry : signatures.entrySet()) {
				if (isOptional) {
					final ActionParameterTypes key = new ActionParameterTypes(isVarArg, entry.getKey().size());
					key.addAll(entry.getKey());
					final List<InferredStandardParameter> value = new ArrayList<>(entry.getValue());
					value.add(new InferredValuedParameter(
							params.getFormalParameter(parameterIndex),
							name, type,
							argumentValue));
					tmpSignatures.put(key, value);
				}
				final ActionParameterTypes key = new ActionParameterTypes(isVarArg, entry.getKey().size() + 1);
				key.addAll(entry.getKey());
				key.add(type.getIdentifier());
				final List<InferredStandardParameter> paramList = entry.getValue();
				paramList.add(new InferredStandardParameter(
						params.getFormalParameter(parameterIndex),
						name, type));
				tmpSignatures.put(key, paramList);
			}
		}
		return new Pair<>(tmpSignatures, isOptional);
	}

	private Map<ActionParameterTypes, List<InferredStandardParameter>> buildSignaturesForArgDefaultValues(
			JvmIdentifiableElement container, String actionId,
			FormalParameterProvider params, ActionParameterTypes fillSignatureKeyOutputParameter) {
		Map<ActionParameterTypes, List<InferredStandardParameter>> signatures = new TreeMap<>();
		fillSignatureKeyOutputParameter.clear();
		if (params.getFormalParameterCount() > 0) {
			final int lastParamIndex = params.getFormalParameterCount() - 1;

			final String containerFullyQualifiedName = createQualifiedActionName(container, null).getContainerID();
			Map<String, Integer> indexes = this.defaultValueIDPrefixes.get(containerFullyQualifiedName);
			if (indexes == null) {
				indexes = new TreeMap<>();
				this.defaultValueIDPrefixes.put(containerFullyQualifiedName, indexes);
			}
			final Integer lastIndex = indexes.get(actionId);
			int defaultValueIndex;
			if (lastIndex == null) {
				defaultValueIndex = 0;
			} else {
				defaultValueIndex = lastIndex.intValue();
			}

			final String[] annotationValues = new String[params.getFormalParameterCount()];
			final String prefix = container.getQualifiedName() + "#" //$NON-NLS-1$
					+ actionId.toUpperCase() + "_"; //$NON-NLS-1$
			for (int i = 0; i <= lastParamIndex; ++i) {
				final Pair<Map<ActionParameterTypes, List<InferredStandardParameter>>, Boolean> pair = buildParameter(
						i,
						lastParamIndex,
						prefix + defaultValueIndex,
						params,
						signatures,
						fillSignatureKeyOutputParameter);
				signatures = pair.getKey();
				if (pair.getValue()) {
					annotationValues[i] = prefix + defaultValueIndex;
					++defaultValueIndex;
				}
			}

			indexes.put(actionId, defaultValueIndex);

			final List<InferredStandardParameter> parameters = signatures.get(fillSignatureKeyOutputParameter);
			if (parameters != null) {
				for (int i = 0; i < parameters.size(); ++i) {
					if (!Strings.isNullOrEmpty(annotationValues[i])) {
						parameters.get(i).setDefaultValueAnnotationValue(annotationValues[i],
								annotationValues[i].substring(annotationValues[i].lastIndexOf("#") + 1)); //$NON-NLS-1$
					}
				}
			}
		}
		return signatures;
	}

	@Override
	public Iterable<InferredPrototype> getPrototypes(QualifiedActionName id) {
		final Map<String, Map<ActionParameterTypes, InferredPrototype>> c = this.prototypes.get(id.getContainerID());
		if (c != null) {
			final Map<ActionParameterTypes, InferredPrototype> list = c.get(id.getActionName());
			if (list != null) {
				return list.values();
			}
		}
		return Collections.emptyList();
	}

	@Override
	public InferredPrototype getPrototypes(QualifiedActionName actionID, ActionParameterTypes signatureID) {
		final Map<String, Map<ActionParameterTypes, InferredPrototype>> c = this.prototypes.get(actionID.getContainerID());
		if (c != null) {
			final Map<ActionParameterTypes, InferredPrototype> list = c.get(actionID.getActionName());
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
	 * @param id identifier of the function.
	 * @param isVarargs indicates if the signature has a variatic parameter.
	 * @param parameters list of the formal parameters of the function.
	 * @return the signature or <code>null</code> if none.
	 */
	protected InferredPrototype createPrototype(QualifiedActionName id,
			boolean isVarargs, FormalParameterProvider parameters) {
		assert parameters != null;
		final ActionParameterTypes key = new ActionParameterTypes(isVarargs, parameters.getFormalParameterCount());
		final Map<ActionParameterTypes, List<InferredStandardParameter>> ip = buildSignaturesForArgDefaultValues(
				id.getDeclaringType(),
				key.toActionPrototype(id.getActionName()).toActionId(),
				parameters, key);
		final List<InferredStandardParameter> op = ip.remove(key);
		final InferredPrototype proto = new DefaultInferredPrototype(
				id,
				parameters,
				key,
				op,
				ip);
		final String containerID = id.getContainerID();
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
		return createPrototype(id, isVarargs,
				new JvmFormalParameterProvider(parameters, this.annotationFinder, this));
	}

	@Override
	public QualifiedActionName createQualifiedActionName(JvmIdentifiableElement container,
			String functionName) {
		return new QualifiedActionName(
				container.eResource().getURI().toString(),
				container,
				functionName);
	}

	@Override
	public QualifiedActionName createConstructorQualifiedName(JvmIdentifiableElement container) {
		return new QualifiedActionName(
				container.eResource().getURI().toString(),
				container,
				this.grammarAccess.getNewKeyword());
	}

	@Override
	public ActionParameterTypes createParameterTypesFromSarlModel(boolean isVarargs,
			List<? extends SarlFormalParameter> parameters) {
		final ActionParameterTypes sig = new ActionParameterTypes(isVarargs, parameters.size());
		if (!parameters.isEmpty()) {
			final int lastIndex = parameters.size() - 1;
			for (int i = 0; i < lastIndex; ++i) {
				final SarlFormalParameter param = parameters.get(i);
				if (param != null && param.getParameterType() != null) {
					sig.add(param.getParameterType().getIdentifier());
				}
			}
			final SarlFormalParameter param = parameters.get(lastIndex);
			if (param != null && param.getParameterType() != null) {
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
		final ActionParameterTypes sig = new ActionParameterTypes(isVarargs, parameters.size());
		for (final JvmFormalParameter p : parameters) {
			final JvmTypeReference paramType = p.getParameterType();
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
		final ActionParameterTypes sig = new ActionParameterTypes(isVarargs, count);
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
		return new ActionPrototype(actionName, parameters, false);
	}

	@Override
	public void clear(JvmIdentifiableElement container) {
		final QualifiedActionName qn = createQualifiedActionName(container, null);
		final String fqn = qn.getContainerID();
		this.prototypes.remove(fqn);
		this.defaultValueIDPrefixes.remove(fqn);
	}

	@Override
	public void clear() {
		this.prototypes.clear();
		this.defaultValueIDPrefixes.clear();
	}

	@Override
	public String createFieldNameForDefaultValueID(String id) {
		final int index = id.indexOf('#');
		if (index > 0) {
			return Utils.createNameForHiddenDefaultValueAttribute(id.substring(index + 1));
		}
		return Utils.createNameForHiddenDefaultValueAttribute(id);
	}

	@Override
	public String qualifyDefaultValueID(String containerQualifiedName, String id) {
		final int index = id.indexOf('#');
		if (index > 0) {
			return id;
		}
		return containerQualifiedName + "#" + id; //$NON-NLS-1$
	}

	@Override
	public String toJavaArgument(String callerQualifiedName, String id) {
		final StringBuilder b = new StringBuilder();
		final int index = id.indexOf('#');
		if (index > 0) {
			final String qn = id.substring(0, index);
			if (!Objects.equal(qn, callerQualifiedName)) {
				b.append(qn);
				b.append("."); //$NON-NLS-1$
			}
			b.append(Utils.createNameForHiddenDefaultValueAttribute(id.substring(index + 1)));
		} else {
			b.append(Utils.createNameForHiddenDefaultValueAttribute(id));
		}
		return b.toString();
	}

	@Override
	public String extractDefaultValueString(JvmFormalParameter parameter) {
		final String fieldId = this.annotationUtils.findStringValue(parameter, DefaultValue.class);
		if (!Strings.isNullOrEmpty(fieldId)) {
			final JvmDeclaredType container = EcoreUtil2.getContainerOfType(parameter, JvmDeclaredType.class);
			if (container != null) {
				final int index = fieldId.indexOf('#');
				final JvmDeclaredType target;
				final String fieldName;
				if (index > 0) {
					final JvmType type = this.references.findDeclaredType(fieldId.substring(0, index), container);
					if (type instanceof JvmDeclaredType) {
						target = (JvmDeclaredType) type;
					} else {
						target = container;
					}
					fieldName = Utils.createNameForHiddenDefaultValueAttribute(fieldId.substring(index + 1));
				} else {
					target = container;
					fieldName = Utils.createNameForHiddenDefaultValueAttribute(fieldId);
				}

				final JvmField field = Iterables.find(target.getDeclaredFields(),
						it -> Objects.equal(it.getSimpleName(), fieldName),
						null);
				if (field != null) {
					final String value = this.annotationUtils.findStringValue(field, SarlSourceCode.class);
					if (!Strings.isNullOrEmpty(fieldId)) {
						return value;
					}
				}
			}
		}
		return null;
	}

}
