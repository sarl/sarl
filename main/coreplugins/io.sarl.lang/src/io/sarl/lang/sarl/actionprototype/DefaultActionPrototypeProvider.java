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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.TreeMap;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import javax.inject.Inject;

import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.references.FunctionTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

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
 * <p>This implementation is thread-safe.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DefaultActionPrototypeProvider implements IActionPrototypeProvider {

	@Inject
	private SARLAnnotationUtil annotationUtils;

	@Inject
	private TypeReferences references;

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	@Inject
	private AnnotationLookup annotationFinder;

	@Inject
	private CommonTypeComputationServices services;

	/** Construct a provider of action prototypes.
	 */
	public DefaultActionPrototypeProvider() {
		//
	}

	/** Unify the type representation for the given JVM type.
	 * This function supports the lambda type.
	 *
	 * @param reference the type.
	 * @return the unified type.
	 * @since 0.12
	 */
	protected LightweightTypeReference unifiesType(JvmTypeReference reference) {
		final LightweightTypeReference lreference = Utils.toLightweightTypeReference(reference, this.services);
		if (lreference.isFunctionType()) {
			final FunctionTypeReference functionReference = lreference.getAsFunctionTypeReference();
			if (functionReference != null) {
				return functionReference;
			}
		}
		return lreference;
	}

	/** Generate a generic identifier for the given type reference.
	 *
	 * @param reference the type.
	 * @return the identifier.
	 * @since 0.12
	 */
	protected String toIdentifier(LightweightTypeReference reference) {
		if (reference.isFunctionType()) {
			return reference.getIdentifier();
		}
		return reference.getRawTypeReference().getIdentifier();
	}

	@Override
	public Iterable<InferredPrototype> getPrototypes(IActionPrototypeContext context, QualifiedActionName id) {
		final Context ctx = (Context) context;
		final InnerMap<String, InnerMap<ActionParameterTypes, InferredPrototype>> c;
		ctx.getPrototypes().getLock().readLock().lock();
		try {
			c = ctx.getPrototypes().get(id.getContainerID());
		} finally {
			ctx.getPrototypes().getLock().readLock().unlock();
		}
		if (c != null) {
			final InnerMap<ActionParameterTypes, InferredPrototype> list;
			c.getLock().readLock().lock();
			try {
				list = c.get(id.getActionName());
			} finally {
				c.getLock().readLock().unlock();
			}
			if (list != null) {
				list.getLock().readLock().lock();
				try {
					return Collections.unmodifiableCollection(list.values());
				} finally {
					list.getLock().readLock().unlock();
				}
			}
		}
		return Collections.emptyList();
	}

	@Override
	public InferredPrototype getPrototypes(IActionPrototypeContext context, QualifiedActionName actionID, ActionParameterTypes signatureID) {
		final Context ctx = (Context) context;
		final InnerMap<String, InnerMap<ActionParameterTypes, InferredPrototype>> c;
		ctx.getPrototypes().getLock().readLock().lock();
		try {
			c = ctx.getPrototypes().get(actionID.getContainerID());
		} finally {
			ctx.getPrototypes().getLock().readLock().unlock();
		}
		if (c != null) {
			final InnerMap<ActionParameterTypes, InferredPrototype> list;
			c.getLock().readLock().lock();
			try {
				list = c.get(actionID.getActionName());
			} finally {
				c.getLock().readLock().unlock();
			}
			if (list != null) {
				list.getLock().readLock().lock();
				try {
					return list.get(signatureID);
				} finally {
					list.getLock().readLock().unlock();
				}
			}
		}
		return null;
	}

	private Pair<InnerMap<ActionParameterTypes, List<InferredStandardParameter>>, Boolean> buildParameter(
			int parameterIndex,
			final int lastParameterIndex,
			DynamicArgumentName argumentValue,
			FormalParameterProvider params,
			InnerMap<ActionParameterTypes, List<InferredStandardParameter>> signatures,
			ActionParameterTypes fillSignatureKeyOutputParameter) {
		final boolean isOptional = params.hasFormalParameterDefaultValue(parameterIndex)
				&& ((parameterIndex < lastParameterIndex)
						|| (!fillSignatureKeyOutputParameter.isVarArg()));
		final boolean isVarArg = parameterIndex >= lastParameterIndex && fillSignatureKeyOutputParameter.isVarArg();
		final String name = params.getFormalParameterName(parameterIndex);
		final JvmTypeReference type = params.getFormalParameterTypeReference(parameterIndex, isVarArg);
		final InnerMap<ActionParameterTypes, List<InferredStandardParameter>> tmpSignatures = new InnerMap<>();
		if (type == null) {
			return new Pair<>(tmpSignatures, isOptional);
		}
		final LightweightTypeReference ltype = unifiesType(type);
		fillSignatureKeyOutputParameter.add(toIdentifier(ltype));
		if (signatures.isEmpty()) {
			// First parameter
			if (isOptional) {
				final ActionParameterTypes key = new ActionParameterTypes(isVarArg, 0);
				final List<InferredStandardParameter> value = new ArrayList<>();
				value.add(new InferredValuedParameter(
						params.getFormalParameter(parameterIndex),
						name, ltype,
						argumentValue));
				tmpSignatures.put(key, value);
			}
			final ActionParameterTypes key = new ActionParameterTypes(isVarArg, 1);
			key.add(toIdentifier(ltype));
			final List<InferredStandardParameter> value = new ArrayList<>();
			value.add(new InferredStandardParameter(
					params.getFormalParameter(parameterIndex),
					name, ltype, argumentValue));
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
							name, ltype,
							argumentValue));
					tmpSignatures.put(key, value);
				}
				final ActionParameterTypes key = new ActionParameterTypes(isVarArg, entry.getKey().size() + 1);
				key.addAll(entry.getKey());
				key.add(toIdentifier(ltype));
				final List<InferredStandardParameter> paramList = entry.getValue();
				paramList.add(new InferredStandardParameter(
						params.getFormalParameter(parameterIndex),
						name, ltype, argumentValue));
				tmpSignatures.put(key, paramList);
			}
		}
		return new Pair<>(tmpSignatures, isOptional);
	}

	private InnerMap<ActionParameterTypes, List<InferredStandardParameter>> buildSignaturesForArgDefaultValues(
			Context context,
			JvmIdentifiableElement container, String actionId,
			FormalParameterProvider params, ActionParameterTypes fillSignatureKeyOutputParameter) {
		InnerMap<ActionParameterTypes, List<InferredStandardParameter>> signatures = new InnerMap<>();
		fillSignatureKeyOutputParameter.clear();
		if (params.getFormalParameterCount() > 0) {
			final int lastParamIndex = params.getFormalParameterCount() - 1;

			final String containerFullyQualifiedName = createQualifiedActionName(container, null).getContainerID();
			InnerMap<String, Integer> indexes;
			context.getDefaultValueIDPrefixes().getLock().readLock().lock();
			try {
				indexes = context.getDefaultValueIDPrefixes().get(containerFullyQualifiedName);
			} finally {
				context.getDefaultValueIDPrefixes().getLock().readLock().unlock();
			}
			if (indexes == null) {
				context.getDefaultValueIDPrefixes().getLock().writeLock().lock();
				try {
					indexes = context.getDefaultValueIDPrefixes().get(containerFullyQualifiedName);
					if (indexes == null) {
						indexes = new InnerMap<>();
						context.getDefaultValueIDPrefixes().put(containerFullyQualifiedName, indexes);
					}
				} finally {
					context.getDefaultValueIDPrefixes().getLock().writeLock().unlock();
				}
			}
			final Integer lastIndex;
			indexes.getLock().readLock().lock();
			try {
				lastIndex = indexes.get(actionId);
			} finally {
				indexes.getLock().readLock().unlock();
			}
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
				final DynamicArgumentName argumentName = new DynamicArgumentName(prefix + defaultValueIndex);
				final Pair<InnerMap<ActionParameterTypes, List<InferredStandardParameter>>, Boolean> pair = buildParameter(
						i,
						lastParamIndex,
						argumentName,
						params,
						signatures,
						fillSignatureKeyOutputParameter);
				signatures = pair.getKey();
				if (pair.getValue()) {
					annotationValues[i] = prefix + defaultValueIndex;
					++defaultValueIndex;
				}
			}

			indexes.getLock().writeLock().lock();
			try {
				indexes.put(actionId, defaultValueIndex);
			} finally {
				indexes.getLock().writeLock().unlock();
			}

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

	/** Build and replies the inferred action signature for the element with
	 * the given ID. This function creates the different signatures according
	 * to the definition, or not, of default values for the formal parameters.
	 *
	 * @param context the context in which the prototype should be created.
	 * @param id identifier of the function.
	 * @param isVarargs indicates if the signature has a variadic parameter.
	 * @param parameters list of the formal parameters of the function.
	 * @return the signature or {@code null} if none.
	 */
	protected InferredPrototype createPrototype(Context context, QualifiedActionName id,
			boolean isVarargs, FormalParameterProvider parameters) {
		assert parameters != null;
		final ActionParameterTypes key = new ActionParameterTypes(isVarargs, parameters.getFormalParameterCount());
		final Map<ActionParameterTypes, List<InferredStandardParameter>> ip = buildSignaturesForArgDefaultValues(
				context,
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
		InnerMap<String, InnerMap<ActionParameterTypes, InferredPrototype>> c;
		context.getPrototypes().getLock().readLock().lock();
		try {
			c = context.getPrototypes().get(containerID);
		} finally {
			context.getPrototypes().getLock().readLock().unlock();
		}
		if (c == null) {
			context.getPrototypes().getLock().writeLock().lock();
			try {
				c = context.getPrototypes().get(containerID);
				if (c == null) {
					c = new InnerMap<>();
					context.getPrototypes().put(containerID, c);
				}
			} finally {
				context.getPrototypes().getLock().writeLock().unlock();
			}
		}

		InnerMap<ActionParameterTypes, InferredPrototype> list;
		c.getLock().readLock().lock();
		try {
			list = c.get(id.getActionName());
		} finally {
			c.getLock().readLock().unlock();
		}
		if (list == null) {
			c.getLock().writeLock().lock();
			try {
				list = c.get(id.getActionName());
				if (list == null) {
					list = new InnerMap<>();
					c.put(id.getActionName(), list);
				}
			} finally {
				c.getLock().writeLock().unlock();
			}
		}
		list.getLock().writeLock().lock();
		try {
			list.put(key, proto);
		} finally {
			list.getLock().writeLock().unlock();
		}
		return proto;
	}

	@Override
	public final InferredPrototype createPrototypeFromSarlModel(IActionPrototypeContext context, QualifiedActionName id,
			boolean isVarargs, List<? extends XtendParameter> parameters) {
		return createPrototype((Context) context, id, isVarargs,
				new SarlFormalParameterProvider(parameters, this.references));
	}

	@Override
	public final InferredPrototype createPrototypeFromJvmModel(IActionPrototypeContext context, QualifiedActionName id,
			boolean isVarargs, List<JvmFormalParameter> parameters) {
		return createPrototype((Context) context, id, isVarargs,
				new JvmFormalParameterProvider(parameters, this.annotationFinder, this));
	}

	@Override
	public IActionPrototypeContext createContext() {
		return new Context();
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
					sig.add(toIdentifier(unifiesType(param.getParameterType())));
				}
			}
			final SarlFormalParameter param = parameters.get(lastIndex);
			if (param != null && param.getParameterType() != null) {
				JvmTypeReference type = param.getParameterType();
				if (isVarargs) {
					type = this.references.createArrayType(type);
				}
				sig.add(toIdentifier(unifiesType(type)));
			}
		}
		return sig;
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
	public ActionParameterTypes createParameterTypesFromString(String parameters) {
		return new ActionParameterTypes(parameters);
	}

	@Override
	public ActionParameterTypes createParameterTypesFromJvmModel(boolean isVarargs, List<JvmFormalParameter> parameters) {
		final ActionParameterTypes sig = new ActionParameterTypes(isVarargs, parameters.size());
		for (final JvmFormalParameter p : parameters) {
			final JvmTypeReference paramType = p.getParameterType();
			if (paramType != null) {
				sig.add(toIdentifier(unifiesType(paramType)));
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
	public String createFunctionNameForDefaultValueID(String id) {
		final int index = id.indexOf('#');
		if (index > 0) {
			return Utils.createNameForHiddenDefaultValueFunction(id.substring(index + 1));
		}
		return Utils.createNameForHiddenDefaultValueFunction(id);
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
			if (!Objects.equals(qn, callerQualifiedName)) {
				b.append(qn);
				b.append("."); //$NON-NLS-1$
			}
			b.append(Utils.createNameForHiddenDefaultValueFunction(id.substring(index + 1)));
		} else {
			b.append(Utils.createNameForHiddenDefaultValueFunction(id));
		}
		b.append("()");
		return b.toString();
	}

	@Pure
	@Override
	public String extractDefaultValueString(JvmFormalParameter parameter) {
		final String fieldId = this.annotationUtils.findStringValue(parameter, DefaultValue.class);
		if (!Strings.isNullOrEmpty(fieldId)) {
			final JvmDeclaredType container = EcoreUtil2.getContainerOfType(parameter, JvmDeclaredType.class);
			if (container != null) {
				final int index = fieldId.indexOf('#');
				final JvmDeclaredType target;
				final String methodName;
				if (index > 0) {
					final JvmType type = this.references.findDeclaredType(fieldId.substring(0, index), container);
					if (type instanceof JvmDeclaredType) {
						target = (JvmDeclaredType) type;
					} else {
						target = container;
					}
					methodName = Utils.createNameForHiddenDefaultValueFunction(fieldId.substring(index + 1));
				} else {
					target = container;
					methodName = Utils.createNameForHiddenDefaultValueFunction(fieldId);
				}

				// Since SARL 0.12, default values are stored into functions
				final JvmOperation operation = Iterables.find(target.getDeclaredOperations(), it -> Objects.equals(it.getSimpleName(), methodName),
						null);
				if (operation != null) {
					final String value = this.annotationUtils.findStringValue(operation, SarlSourceCode.class);
					if (!Strings.isNullOrEmpty(fieldId)) {
						return value;
					}
				}

				// Backward compatibility: read default values from fields
				// TODO: Remove when the backward compatibility is not any more needed.
				final JvmField field = Iterables.find(target.getDeclaredFields(), it -> Objects.equals(it.getSimpleName(), methodName),
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

	/**
	 * Context for a {@code DefaultActionPrototypeProvider}.
	 *
	 * @param <K> the type of the keys.
	 * @param <V> the type of the values.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class InnerMap<K, V> extends TreeMap<K, V> {
		// It is important to have a SortedMap as the super type in
		// order to preserve the order of the prototypes into the
		// map. This order is assumed to be present in all the unit tests.

		private static final long serialVersionUID = -7858620757455956027L;

		private final ReadWriteLock lock = new ReentrantReadWriteLock();

		InnerMap() {
			//
		}

		/** Replies the lock.
		 *
		 * @return never {@code null}.
		 */
		public ReadWriteLock getLock() {
			return this.lock;
		}

	}

	/**
	 * Context for a {@code DefaultActionPrototypeProvider}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class Context implements IActionPrototypeContext {

		private final InnerMap<String, InnerMap<String, InnerMap<ActionParameterTypes, InferredPrototype>>> prototypes = new InnerMap<>();

		private final InnerMap<String, InnerMap<String, Integer>> defaultValueIDPrefixes = new InnerMap<>();

		Context() {
			//
		}

		@Override
		public void release() {
			this.prototypes.getLock().writeLock().lock();
			try {
				this.prototypes.clear();
			} finally {
				this.prototypes.getLock().writeLock().unlock();
			}
			this.defaultValueIDPrefixes.getLock().writeLock().lock();
			try {
				this.defaultValueIDPrefixes.clear();
			} finally {
				this.defaultValueIDPrefixes.getLock().writeLock().unlock();
			}
		}

		public InnerMap<String, InnerMap<String, Integer>> getDefaultValueIDPrefixes() {
			return this.defaultValueIDPrefixes;
		}

		public InnerMap<String, InnerMap<String, InnerMap<ActionParameterTypes, InferredPrototype>>> getPrototypes() {
			return this.prototypes;
		}

	}

}
