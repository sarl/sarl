/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.jvmmodel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.common.types.JvmTypeParameterDeclarator;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import com.google.common.base.Strings;
import com.google.common.collect.Collections2;
import com.google.inject.Inject;

import io.sarl.lang.compiler.GeneratorConfig2;
import io.sarl.lang.compiler.IGeneratorConfigProvider2;
import io.sarl.lang.core.annotation.Injectable;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeContext;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.util.Utils;

/** Describe generation context.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class GenerationContext {

	private static final String MAIN_FUNCTION_PARAMETER_TYPE = String.class.getName() + "[]"; //$NON-NLS-1$
	
	private JvmDeclaredType target;

	/** Indicate if the object is injectable.
	 */
	private boolean isInjectable;

	/** Compute serial number for serializable objects.
	 */
	private long serial = 1L;

	/** Index of the late generated action.
	 */
	private int actionIndex;

	/** Index of the late generated behavior unit.
	 */
	private int behaviorUnitIndex;

	/** Index of the late generated localType.
	 */
	private int localTypeIndex;

	/** Set of capacities for which a capacuty-use field was generated.
	 */
	private final Set<String> generatedCapacityUseFields = CollectionLiterals.<String>newHashSet();

	/** collection of the generated constructors.
	 */
	private final Map<ActionParameterTypes, JvmConstructor> generatedConstructors = CollectionLiterals.newTreeMap(null);

	/** Collection of the inherited final operations.
	 */
	private final Map<ActionPrototype, JvmOperation> finalOperations = CollectionLiterals.newTreeMap(null);

	/** Collection of the inherited overridable operations.
	 */
	private final Map<ActionPrototype, JvmOperation>  overridableOperations = CollectionLiterals.newTreeMap(null);

	/** Collection of the inherited operations that have not been implemented.
	 */
	private final Map<ActionPrototype, JvmOperation>  operationsToImplement = CollectionLiterals.newTreeMap(null);

	/** Collection of the local operations.
	 * @since 0.12
	 */
	private final Map<ActionPrototype, JvmOperation>  localOperations = CollectionLiterals.newTreeMap(null);

	/** List of elements that must be generated at the end of the generation process.
	 */
	private final List<Runnable> preFinalization = CollectionLiterals.newLinkedList();

	/** List of elements that must be generated after the generation process.
	 */
	private final List<Runnable> postFinalization = CollectionLiterals.newLinkedList();

	/** Guard evaluators to generate. The keys are the event identifiers. The values are the code snipsets for
	 * evaluating guards and returning the event handler runnables.
	 */
	private final Map<String, BehaviorUnitGuardEvaluators> guardEvaluators = new TreeMap<>();

	/** The context object.
	 */
	private final EObject contextObject;

	/** The provider of generation configuration.
	 */
	@Inject
	private IGeneratorConfigProvider generatorConfigProvider;

	/** The provider of generation configuration v2.
	 */
	@Inject
	private IGeneratorConfigProvider2 generatorConfigProvider2;

	/** Buffering the current generator configuration.
	 */
	private GeneratorConfig generatorConfig;

	/** Buffering the current generator configuration v2.
	 */
	private GeneratorConfig2 generatorConfig2;

	/** Parent context.
	 */
	private GenerationContext parent;

	private IActionPrototypeContext actionPrototypeContext;

	@Inject
	private AnnotationLookup annotationFinder;

	private Map<String, List<Object>> userData = new TreeMap<>();

	@Inject
	private CommonTypeComputationServices services;

	private Boolean isMainFunctionManuallyDefined;

	/** Construct a information about the generation.
	 *
	 * @param owner the object for which the context is created.
	 * @param target the target type for which the context is opened.
	 */
	GenerationContext(EObject owner, JvmDeclaredType target) {
		assert owner != null;
		this.target = target;
		this.contextObject = owner;
	}

	@Override
	public String toString() {
		return "Generation context for: " + getTypeIdentifier(); //$NON-NLS-1$
	}

	/** Replies the parent context if any.
	 *
	 * @return the parent context or {@code null}.
	 * @since 0.5
	 */
	public GenerationContext getParentContext() {
		return this.parent;
	}

	/** Change the parent context if any.
	 *
	 * @param parent the parent context or {@code null}.
	 * @since 0.5
	 */
	public void setParentContext(GenerationContext parent) {
		this.parent = parent;
	}

	/** Replies the identifier of the associated type.
	 *
	 * @return the identifier of the context's type.
	 */
	public String getTypeIdentifier() {
		return this.target == null ? null : this.target.getIdentifier();
	}

	/** Replies the associated type.
	 *
	 * @return the context's type.
	 * @since 0.8.6
	 */
	public JvmDeclaredType getType() {
		return this.target;
	}

	/** Replies the generator configuration.
	 *
	 * @return the configuration.
	 */
	public GeneratorConfig getGeneratorConfig() {
		if (this.generatorConfig == null) {
			this.generatorConfig = this.generatorConfigProvider.get(
					EcoreUtil.getRootContainer(this.contextObject));
		}
		return this.generatorConfig;
	}

	/** Replies the generator configuration v2.
	 *
	 * @return the configuration.
	 */
	public GeneratorConfig2 getGeneratorConfig2() {
		if (this.generatorConfig2 == null) {
			this.generatorConfig2 = this.generatorConfigProvider2.get(
					EcoreUtil.getRootContainer(this.contextObject));
		}
		return this.generatorConfig2;
	}

	/** Replies the guard evaluation code for the given event.
	 *
	 * @return the guard evaluators.
	 */
	public Collection<BehaviorUnitGuardEvaluators> getGuardEvaluationCodes() {
		return this.guardEvaluators.values();
	}

	/** Create and replies the guard evaluation code for the given event.
	 *
	 * @param source the source of the guard evaluation.
	 * @param typeReferences the tool for creating type references that may be needed in this function.
	 * @return the guard evaluators.
	 */
	public Collection<Procedure1<? super ITreeAppendable>> ensureGuardEvaluationCodeFor(SarlBehaviorUnit source,
			TypeReferences typeReferences) {
		assert source != null;
		var eventType = source.getName();
		// Ensure that the event type has a canonical name, i.e., if there is no type parameter specified,
		// there are implicitly replaced by wildcards.
		if (eventType.getArguments().isEmpty()
				&& eventType.getType() instanceof JvmTypeParameterDeclarator cvalue
				&& !cvalue.getTypeParameters().isEmpty()) {
			final var max = cvalue.getTypeParameters().size();
			final var tab = new JvmTypeReference[max];
			for (var i = 0; i < max; ++i) {
				tab[i] = typeReferences.wildCard();
			}
			eventType = typeReferences.createTypeRef(eventType.getType(), tab);
		}
		final var id = Utils.createBehaviorUnitEventId(eventType);
		final var finalEventType = eventType;
		final var evaluators = this.guardEvaluators.computeIfAbsent(id, key -> {
			return new BehaviorUnitGuardEvaluators(source, finalEventType, new ArrayList<>());
		});
		assert evaluators != null;
		return evaluators.evaluators();
	}

	/** Replies the computed serial number.
	 *
	 * @return the serial number.
	 */
	public long getSerial() {
		return this.serial;
	}

	/** Increment the serial number by the given ammount.
	 *
	 * @param value the value to add to the serial number.
	 */
	public void incrementSerial(long value) {
		this.serial += value;
	}

	/** Replies if a constructor is generated.
	 *
	 * @return {@code true} if the constructor is generated; {@code false} if created.
	 */
	public boolean hasConstructor() {
		return !this.generatedConstructors.isEmpty();
	}

	/** Add a generated constructor into the context.
	 *
	 * @param parameters the specification of the parameters of the constructor.
	 * @param jvmElement the generated element.
	 */
	public void addGeneratedConstructor(ActionParameterTypes parameters, JvmConstructor jvmElement) {
		this.generatedConstructors.put(parameters, jvmElement);
	}

	/** Replies the collection of the generated constructor.
	 *
	 * @return the original collection of constructors.
	 */
	public Map<ActionParameterTypes, JvmConstructor> getGeneratedConstructors() {
		return this.generatedConstructors;
	}

	/** Add a capacity for which a capacity-use field is generated.
	 *
	 * @param capacity the identifier of the capacity.
	 */
	public void addGeneratedCapacityUseField(String capacity) {
		this.generatedCapacityUseFields.add(capacity);
	}

	/** Replies the capacities for which capacity-use fields are generated.
	 *
	 * @return the capacity identifiers.
	 */
	public Set<String> getGeneratedCapacityUseFields() {
		return this.generatedCapacityUseFields;
	}

	/** Replies the collection of the inherited final operations.
	 *
	 * @return the original collection of operations.
	 */
	public Map<ActionPrototype, JvmOperation> getInheritedFinalOperations() {
		return this.finalOperations;
	}

	/** Replies the collection of the inherited overridable operations.
	 *
	 * @return the original collection of operations.
	 */
	public Map<ActionPrototype, JvmOperation> getInheritedOverridableOperations() {
		return this.overridableOperations;
	}

	/** Replies the collection of the inherited operations that are not yet implemented.
	 *
	 * @return the original collection of operations.
	 */
	public Map<ActionPrototype, JvmOperation> getInheritedOperationsToImplement() {
		return this.operationsToImplement;
	}

	/** Replies the operation with the given prototype that is locally defined.
	 * This function does not search in inherited operations.
	 *
	 * @param prototype the prototype to search for.
	 * @return the locally defined operation.
	 * @since 0.12
	 */
	public JvmOperation getLocalOperation(ActionPrototype prototype) {
		return this.localOperations.get(prototype);
	}

	/** Register a locally defined operation.
	 *
	 * @param prototype the prototype of the operation.
	 * @param operation the JVM operation.
	 * @since 0.12
	 */
	public void doLocalOperationDefinition(ActionPrototype prototype, JvmOperation operation) {
		getInheritedOperationsToImplement().remove(prototype);
		getInheritedOverridableOperations().remove(prototype);
		this.localOperations.put(prototype, operation);
	}

	/** Replies if a main function (with the standard prototype) is defined in the source code.
	 *
	 * @return {@code true} if a main function is defined.
	 * @since 0.15
	 */
	public boolean isMainFunctionManuallyDefined() {
		if (this.isMainFunctionManuallyDefined == null) {
			if (getContextObject() instanceof SarlClass sarlClass) {
				this.isMainFunctionManuallyDefined = Boolean.valueOf(sarlClass.getMembers().stream().anyMatch(it -> {
					return it instanceof XtendFunction action && Utils.isMainFunctionDeclaration(action, this.services);
				}));
			} else {
				this.isMainFunctionManuallyDefined = Boolean.FALSE;
			}
		}
		return this.isMainFunctionManuallyDefined.booleanValue();
	}

	/** Replies if a main function (with the standard prototype) is generated from the source code or as synthetic.
	 *
	 * @return {@code true} if a main function is generated.
	 * @since 0.15
	 */
	public boolean isMainFunctionGenerated() {
		var parameters = new ActionParameterTypes(false, 1);
		parameters.add(MAIN_FUNCTION_PARAMETER_TYPE);
		var prototype = new ActionPrototype(Utils.getNameForJavaMainFunction(), parameters, true);
		if (this.localOperations.containsKey(prototype)) {
			return true;
		}
		parameters = new ActionParameterTypes(true, 1);
		parameters.add(MAIN_FUNCTION_PARAMETER_TYPE);
		prototype = new ActionPrototype(Utils.getNameForJavaMainFunction(), parameters, true);
		return this.localOperations.containsKey(prototype);
	}

	/** Replies the inherited operation with the given prototype.
	 * The operation may be implemented or not into the super types.
	 *
	 * @param prototype the prototype to search for.
	 * @return the operation or {@code null}.
	 * @since 0.12
	 */
	public JvmOperation getInheritedOperation(ActionPrototype prototype) {
		var inheritedOperation = getInheritedImplementedOperation(prototype);
		if (inheritedOperation == null) {
			inheritedOperation = getInheritedOperationsToImplement().get(prototype);
		}
		return inheritedOperation;
	}

	/** Replies the inherited and implemented operation with the given prototype.
	 *
	 * @param prototype the prototype to search for.
	 * @return the operation or {@code null}.
	 * @since 0.12
	 */
	public JvmOperation getInheritedImplementedOperation(ActionPrototype prototype) {
		var inheritedOperation = getInheritedFinalOperations().get(prototype);
		if (inheritedOperation == null) {
			inheritedOperation = getInheritedOverridableOperations().get(prototype);
		}
		return inheritedOperation;
	}

	/** Replies the locally or inherited operation.
	 *
	 * @param prototype the prototype to search for.
	 * @return the operation or {@code null}.
	 */
	public JvmOperation getDefinedOperation(ActionPrototype prototype) {
		final var localOperation = this.localOperations.get(prototype);
		if (localOperation != null) {
			return localOperation;
		}
		return getInheritedOperation(prototype);
	}

	/** Replies the collection of the elements that must be generated at the end of
	 * the generation process.
	 *
	 * <p>The differed generation element are the element's components that should be
	 * created after all the elements from the SARL input. The runnable codes
	 * are run at the end of the JVM element generation.
	 *
	 * @return the original collection of elements.
	 * @since 0.5
	 */
	public List<Runnable> getPreFinalizationElements() {
		return this.preFinalization;
	}

	/** Replies the collection of the elements that must be generated after
	 * the generation process of the current SARL element.
	 *
	 * <p>The differed generation element are the element's components that could be
	 * generated after the complete JVM type is generated. They are extended the
	 * JVM type definition with additional elements (annotations...)
	 *
	 * @return the original collection of elements.
	 * @since 0.5
	 */
	public List<Runnable> getPostFinalizationElements() {
		final var prt = getParentContext();
		if (prt != null) {
			return prt.getPostFinalizationElements();
		}
		return this.postFinalization;
	}

	/** Replies the index of the late created action.
	 *
	 * @return the index.
	 */
	public int getActionIndex() {
		return this.actionIndex;
	}

	/** Set the index of the late created action.
	 *
	 * @param index the index.
	 */
	public void setActionIndex(int index) {
		this.actionIndex = index;
	}

	/** Replies the index of the last created behavior unit.
	 *
	 * @return the index
	 */
	public int getBehaviorUnitIndex() {
		return this.behaviorUnitIndex;
	}

	/** Replies the index of the last created behavior unit.
	 *
	 * @param index the index.
	 */
	public void setBehaviorUnitIndex(int index) {
		this.behaviorUnitIndex = index;
	}

	/** Replies the index of the late created local type.
	 *
	 * @return the index.
	 */
	public int getLocalTypeIndex() {
		return this.localTypeIndex;
	}

	/** Set the index of the late created local type.
	 *
	 * @param index the index.
	 */
	public void setLocalTypeIndex(int index) {
		this.localTypeIndex = index;
	}

	/** Replies if the given member is supported in the current context.
	 *
	 * @param member the member to test.
	 * @return {@code true} if the member is supported, {@code false} for ignoring it.
	 */
	public abstract boolean isSupportedMember(XtendMember member);

	/** Get the context for the action prototype provider.
	 *
	 * @param provider the provider for creating the context if it was not created.
	 * @return the context
	 */
	public IActionPrototypeContext getActionPrototypeContext(IActionPrototypeProvider provider) {
		if (this.actionPrototypeContext == null) {
			this.actionPrototypeContext = provider.createContext();
		}
		return this.actionPrototypeContext;
	}

	/** Replies if this context is released.
	 *
	 * @return {@code true} if the context was released, otherwise {@code false}.
	 * @since 0.13
	 */
	public synchronized boolean isRelease() {
		return this.target == null || this.contextObject == null;
	}

	/** Release any allocated resource.
	 */
	public synchronized void release() {
		if (this.actionPrototypeContext != null) {
			this.actionPrototypeContext.release();
			this.actionPrototypeContext = null;
		}
		this.target = null;
		this.generatorConfig = null;
		this.generatorConfig2 = null;
		this.finalOperations.clear();
		this.generatedCapacityUseFields.clear();
		this.generatedConstructors.clear();
		this.finalOperations.clear();
		this.overridableOperations.clear();
		this.operationsToImplement.clear();
		this.preFinalization.clear();
		this.postFinalization.clear();
		this.guardEvaluators.clear();
	}

	/** Replies the context object.
	 *
	 * @return the context object.
	 * @since 0.12
	 */
	@Pure
	public EObject getContextObject() {
		return this.contextObject;
	}

	/** Replies if the generated type is injectable.
	 *
	 * @return {@code true} if the generated type is injectable.
	 * @since 0.12
	 */
	@Pure
	public boolean isInjectable() {
		return this.isInjectable;
	}

	/** Change the flag that indicates if the generated type is injectable.
	 *
	 * @param injectable {@code true} if the generated type is injectable.
	 * @since 0.12
	 */
	public void setInjectable(boolean injectable) {
		this.isInjectable = injectable;
	}

	/** Replies if the given target has the {@code Inject} annotation.
	 *
	 * @param target the target to analyze.
	 * @return {@code true} if the inject annotation is attached to the target.
	 * @since 0.14
	 */
	protected static boolean hasInjectAnnotation(JvmAnnotationTarget target) {
		return target.getAnnotations().stream()
				.anyMatch(it -> {
					final var annotationType = it.getAnnotation();
					if (annotationType != null) {
						final var name = annotationType.getQualifiedName();
						if (name != null && name.endsWith(".Inject")) { //$NON-NLS-1$
							if (name.startsWith("jakarta.inject.") //$NON-NLS-1$
									|| name.startsWith("com.google.inject.") //$NON-NLS-1$
									|| name.startsWith("javax.inject.")) { //$NON-NLS-1$
								return true;
							}
						}
					}
					return false;
				});
	}

	/** Change the flag that indicates if the given element is injected.
	 *
	 * @param element the element to test.
	 * @since 0.12
	 */
	public void setInjectable(JvmAnnotationTarget element) {
		if (element != null && hasInjectAnnotation(element)) {
			setInjectable(true);
		}
	}

	/** Change the flag that indicates if the given element is injected.
	 *
	 * @param element the element to test.
	 * @since 0.12
	 */
	public void setInjectable(JvmTypeReference element) {
		if (element != null) {
			final var type = element.getType();
			if (type instanceof JvmAnnotationTarget cvalue
					&& this.annotationFinder.findAnnotation(cvalue, Injectable.class) != null) {
				setInjectable(true);
			}
		}
	}

	/** Add a user data associated to the given id.
	 *
	 * @param id the identifier of the user data.
	 * @param value the value to put into the list of user data with the given id.
	 * @since 0.12
	 */
	public void addUserObject(String id, Object value) {
		if (!Strings.isNullOrEmpty(id) && value != null) {
			final var usrData = this.userData.computeIfAbsent(id, it -> {
				return new ArrayList<>();
			});
			usrData.add(value);
		}
	}

	/** Consume the user data associated to the given id.
	 *
	 * @param id the identifier of the user data.
	 * @return the value to put into the list of user data with the given id.
	 * @since 0.12
	 */
	public Iterable<Object> consumeUserObject(String id) {
		if (!Strings.isNullOrEmpty(id)) {
			final var usrData = this.userData.remove(id);
			if (usrData != null) {
				return usrData;
			}
		}
		return Collections.emptyList();
	}

	/** Consume the user data associated to the given id.
	 *
	 * @param <T> the type of the data.
	 * @param id the identifier of the user data.
	 * @param dataType the type of the data.
	 * @return the value to put into the list of user data with the given id.
	 * @since 0.12
	 */
	public <T> Iterable<T> consumeUserObject(String id, Class<T> dataType) {
		if (!Strings.isNullOrEmpty(id)) {
			final var usrData = this.userData.remove(id);
			if (usrData != null) {
				final var output = Collections2.transform(usrData, it -> {
					return dataType.cast(it);
				});
				return output;
			}
		}
		return Collections.emptyList();
	}

	/** Define a group of guard evaluators.
	 *
	 * @param source the behavior unit.
	 * @param eventType the event type associated to the guard evaluators.
	 * @param evaluators the codes for evaluating the guards.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.14
	 */
	public record BehaviorUnitGuardEvaluators(
			SarlBehaviorUnit source,
			JvmParameterizedTypeReference eventType,
			Collection<Procedure1<? super ITreeAppendable>> evaluators) {
		//
	}

}
