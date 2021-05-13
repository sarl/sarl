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

package io.sarl.lang.jvmmodel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import javax.inject.Inject;

import com.google.common.base.Strings;
import com.google.common.collect.Collections2;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.annotation.Injectable;
import io.sarl.lang.compiler.GeneratorConfig2;
import io.sarl.lang.compiler.IGeneratorConfigProvider2;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeContext;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;

/** Describe generation context.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
abstract class GenerationContext {

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
	private final Map<String, Pair<SarlBehaviorUnit, Collection<Procedure1<? super ITreeAppendable>>>> guardEvaluators
			= CollectionLiterals.newHashMap();

	/** The context object.
	 */
	private EObject contextObject;

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

	/** Construct a information about the generation.
	 *
	 * @param owner the object for which the context is created.
	 * @param target the target type for which the context is opened.
	 */
	GenerationContext(EObject owner, JvmDeclaredType target) {
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
		return this.target.getIdentifier();
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
	public Collection<Pair<SarlBehaviorUnit, Collection<Procedure1<? super ITreeAppendable>>>>
			getGuardEvaluationCodes() {
		return this.guardEvaluators.values();
	}

	/** Replies the guard evaluation code for the given event.
	 *
	 * @param source the source of the guard evaluation.
	 * @return the guard evaluators.
	 */
	public Collection<Procedure1<? super ITreeAppendable>> getGuardEvalationCodeFor(SarlBehaviorUnit source) {
		assert source != null;
		final String id = source.getName().getIdentifier();
		final Collection<Procedure1<? super ITreeAppendable>> evaluators;
		final Pair<SarlBehaviorUnit, Collection<Procedure1<? super ITreeAppendable>>> pair = this.guardEvaluators.get(id);
		if (pair == null) {
			evaluators = new ArrayList<>();
			this.guardEvaluators.put(id, new Pair<>(source, evaluators));
		} else {
			evaluators = pair.getValue();
			assert evaluators != null;
		}
		return evaluators;
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
	 * @return <code>true</code> if the constructor is generated; <code>false</code> if created.
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

	/** Replies the inherited operation with the given prototype.
	 * The operation may be implemented or not into the super types.
	 *
	 * @param prototype the prototype to search for.
	 * @return the operation or {@code null}.
	 * @since 0.12
	 */
	public JvmOperation getInheritedOperation(ActionPrototype prototype) {
		JvmOperation inheritedOperation = getInheritedImplementedOperation(prototype);
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
		JvmOperation inheritedOperation = getInheritedFinalOperations().get(prototype);
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
		final JvmOperation localOperation = this.localOperations.get(prototype);
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
		final GenerationContext prt = getParentContext();
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
	 * @return <code>true</code> if the member is supported, <code>false</code> for ignoring it.
	 */
	public abstract boolean isSupportedMember(XtendMember member);

	/** Replies if the compiler is using Java8 or higher.
	 *
	 * @return <code>true</code> if the compiler uses Java8 or higher. Otherwise <code>false</code>.
	 */
	public boolean isAtLeastJava8() {
		return getGeneratorConfig().getJavaSourceVersion().isAtLeast(JavaVersion.JAVA8);
	}

	/** Replies if the compiler is using Java11 or higher.
	 *
	 * @return <code>true</code> if the compiler uses Java8 or higher. Otherwise <code>false</code>.
	 * @since 0.10
	 */
	public boolean isAtLeastJava11() {
		return getGeneratorConfig().getJavaSourceVersion().isAtLeast(JavaVersion.JAVA11);
	}

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

	/** Release any allocated resource.
	 */
	public void release() {
		if (this.actionPrototypeContext != null) {
			this.actionPrototypeContext.release();
			this.actionPrototypeContext = null;
		}
		this.target = null;
		this.contextObject = null;
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

	/** Change the flag that indicates if the given element is injected.
	 *
	 * @param element the element to test.
	 * @since 0.12
	 */
	public void setInjectable(JvmAnnotationTarget element) {
		if (element != null
			&& (this.annotationFinder.findAnnotation(element, Inject.class) != null
			   || this.annotationFinder.findAnnotation(element, com.google.inject.Inject.class) != null)) {
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
			final JvmType type = element.getType();
			if (type instanceof JvmAnnotationTarget
				&& this.annotationFinder.findAnnotation((JvmAnnotationTarget) type, Injectable.class) != null) {
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
			final List<Object> usrData = this.userData.computeIfAbsent(id, it -> {
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
			final List<Object> usrData = this.userData.remove(id);
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
			final List<Object> usrData = this.userData.remove(id);
			if (usrData != null) {
				final Collection<T> output = Collections2.transform(usrData, it -> {
					return dataType.cast(it);
				});
				return output;
			}
		}
		return Collections.emptyList();
	}

}
