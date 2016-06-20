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

package io.sarl.lang.jvmmodel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.actionprototype.ActionParameterTypes;
import io.sarl.lang.actionprototype.ActionPrototype;
import io.sarl.lang.sarl.SarlBehaviorUnit;

/** Describe generation context.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
abstract class GenerationContext {

	/** Compute serial number for serializable objects.
	 */
	private long serial = 1L;

	/** Index of the late generated action.
	 */
	private int actionIndex;

	/** Index of the late generated behavior unit.
	 */
	private int behaviorUnitIndex;

	/** collection of the generated constructors.
	 */
	@SuppressWarnings("unchecked")
	private final Map<ActionParameterTypes, JvmConstructor> generatedConstructors = CollectionLiterals.newTreeMap(null);

	/** Collection of the inherited final operations.
	 */
	@SuppressWarnings("unchecked")
	private final Map<ActionPrototype, JvmOperation> finalOperations = CollectionLiterals.newTreeMap(null);

	/** Collection of the inherited overridable operations.
	 */
	@SuppressWarnings("unchecked")
	private final Map<ActionPrototype, JvmOperation>  overridableOperations = CollectionLiterals.newTreeMap(null);

	/** Collection of the inherited operations that have not been implemented.
	 */
	@SuppressWarnings("unchecked")
	private final Map<ActionPrototype, JvmOperation>  operationsToImplement = CollectionLiterals.newTreeMap(null);

	/** List of elements that must be generated at the end of the generation process.
	 */
	private final List<Runnable> differedCodeGeneration = CollectionLiterals.newLinkedList();

	/** Guard evaluators to generate. The keys are the event identifiers. The values are the code snipsets for
	 * evaluating guards and returning the event handler runnables.
	 */
	@SuppressWarnings("unchecked")
	private final Map<JvmTypeReference, Pair<SarlBehaviorUnit, Collection<String>>> guardEvaluators
			= CollectionLiterals.newHashMap();

	/** Construct a information about the generation.
	 */
	GenerationContext() {
		//
	}

	/** Replies the guard evaluation code for the given event.
	 *
	 * @return the guard evaluators.
	 */
	public Collection<Entry<JvmTypeReference, Pair<SarlBehaviorUnit, Collection<String>>>> getGuardEvalationCodes() {
		return this.guardEvaluators.entrySet();
	}

	/** Replies the guard evaluation code for the given event.
	 *
	 * @param source the source of the guard evaluation.
	 * @return the guard evaluators.
	 */
	public Collection<String> getGuardEvalationCodeFor(SarlBehaviorUnit source) {
		assert (source != null);
		final JvmTypeReference id = source.getName();
		Collection<String> evaluators;
		Pair<SarlBehaviorUnit, Collection<String>> pair = this.guardEvaluators.get(id);
		if (pair == null) {
			evaluators = new ArrayList<>();
			this.guardEvaluators.put(id, new Pair<>(source, evaluators));
		} else {
			evaluators = pair.getValue();
			assert (evaluators != null);
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

	/** Add an element that must be generated at the end of the generation process.
	 *
	 * @param element the element to generate at the end.
	 */
	public void addDifferedGenerationElement(Runnable element) {
		this.differedCodeGeneration.add(element);
	}

	/** Replies the collection of the elements that must be generated at the end of
	 * the generation process.
	 *
	 * @return the original collection of elements.
	 */
	public List<Runnable> getDifferedGenerationElements() {
		return this.differedCodeGeneration;
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

	/** Replies if the given member is supported in the current context.
	 *
	 * @param member the member to test.
	 * @return <code>true</code> if the member is supported, <code>false</code> for ignoring it.
	 */
	public abstract boolean isSupportedMember(XtendMember member);

}