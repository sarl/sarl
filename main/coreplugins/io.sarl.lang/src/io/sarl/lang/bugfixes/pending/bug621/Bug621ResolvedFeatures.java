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

package io.sarl.lang.bugfixes.pending.bug621;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.LinkedHashMultiset;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;
import com.google.common.collect.Sets;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypesSwitch;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.typesystem.override.AbstractResolvedOperation;
import org.eclipse.xtext.xbase.typesystem.override.BottomResolvedOperation;
import org.eclipse.xtext.xbase.typesystem.override.ConflictingDefaultOperation;
import org.eclipse.xtext.xbase.typesystem.override.IOverrideCheckResult;
import org.eclipse.xtext.xbase.typesystem.override.IOverrideCheckResult.OverrideCheckDetails;
import org.eclipse.xtext.xbase.typesystem.override.IResolvedOperation;
import org.eclipse.xtext.xbase.typesystem.override.OverrideTester;
import org.eclipse.xtext.xbase.typesystem.override.ResolvedFeatures;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/**
 * Fixing the SARL issue 621: Error on multiple function inheritance.
 *
 * <p>Issue is due to Xtend issue 191 (https://github.com/eclipse/xtext-xtend/pull/191),
 * and the associated PR 192 (https://github.com/eclipse/xtext-xtend/pull/192)
 *
 * <p>Search for "START CHANGE" comment for finding the specific fixes of this class.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/621"
 * @see "https://github.com/eclipse/xtext-xtend/pull/191"
 * @see "https://github.com/eclipse/xtext-xtend/pull/192"
 */
@SuppressWarnings("all")
public class Bug621ResolvedFeatures extends ResolvedFeatures {

	private JavaVersion targetVersion = JavaVersion.JAVA5;
	
	public Bug621ResolvedFeatures(LightweightTypeReference type, OverrideTester overrideTester, JavaVersion targetVersion) {
		super(type, overrideTester, targetVersion);
		this.targetVersion = targetVersion;
	}
	
	public Bug621ResolvedFeatures(LightweightTypeReference type, OverrideTester overrideTester) {
		super(type, overrideTester);
	}
	
	public Bug621ResolvedFeatures(LightweightTypeReference type) {
		super(type);
	}

	protected void computeAllOperationsFromSortedSuperTypes(JvmDeclaredType rootType,
			final Multimap<String, AbstractResolvedOperation> processedOperations) {
		class SuperTypes extends TypesSwitch<Boolean> {

			private Multiset<JvmType> interfaces = LinkedHashMultiset.create();
			private Set<JvmType> notInterfaces = Sets.newLinkedHashSet();
			
			public SuperTypes(JvmDeclaredType rootType) {
				doSwitch(rootType);
			}

			@Override
			public Boolean doSwitch(EObject theEObject) {
				if (theEObject == null)
					return Boolean.FALSE;
				return super.doSwitch(theEObject);
			}
			
			@Override
			public Boolean caseJvmTypeReference(JvmTypeReference object) {
				return doSwitch(object.getType());
			}
			
			@Override
			public Boolean caseJvmType(JvmType object) {
				return notInterfaces.add(object);
			}
			
			@Override
			public Boolean caseJvmDeclaredType(JvmDeclaredType object) {
				if (notInterfaces.add(object)) {
					for (JvmTypeReference superType : object.getSuperTypes()) {
						doSwitch(superType);
					}
					return Boolean.TRUE;
				}
				return Boolean.FALSE;
			}
			
			@Override
			public Boolean caseJvmGenericType(JvmGenericType object) {
				boolean traverseSuperTypes = false;
				if (object.isInterface()) {
					traverseSuperTypes = interfaces.add(object, 1) == 0;
				} else {
					traverseSuperTypes = notInterfaces.add(object);
				}
				if (traverseSuperTypes) {
					for (JvmTypeReference superType : object.getSuperTypes()) {
						doSwitch(superType);
					}
					return Boolean.TRUE;
				}
				return Boolean.FALSE;
			}
			
			public Collection<JvmType> getSuperTypesNoInterfaces() {
				return notInterfaces;
			}
			
			public int consumeInterfaceOccurrence(JvmGenericType intf) {
				return interfaces.remove(intf, 1);
			}
			
		}
		final SuperTypes superTypes = new SuperTypes(rootType);
		for(JvmType superClass: superTypes.getSuperTypesNoInterfaces()) {
			if (superClass instanceof JvmDeclaredType) {
				computeAllOperations((JvmDeclaredType) superClass, processedOperations);
			}
		}
		
		class SuperInterfaceConsumer extends TypesSwitch<Boolean> {

			private Set<JvmType> seen = Sets.newHashSet();
			
			private boolean isSuperclassBranch;
			private Multimap<String, AbstractResolvedOperation> superclassBranchOperations;
			
			@Override
			public Boolean doSwitch(EObject theEObject) {
				if (theEObject == null)
					return Boolean.FALSE;
				return super.doSwitch(theEObject);
			}
			
			@Override
			public Boolean defaultCase(EObject object) {
				return Boolean.FALSE;
			}
			
			@Override
			public Boolean caseJvmTypeReference(JvmTypeReference object) {
				return doSwitch(object.getType());
			}
			
			@Override
			public Boolean caseJvmDeclaredType(JvmDeclaredType object) {
				if (seen.add(object)) {
					for (JvmTypeReference superType : object.getSuperTypes()) {
						doSwitch(superType);
					}
					return Boolean.TRUE;
				}
				return Boolean.FALSE;
			}
			
			@Override
			public Boolean caseJvmGenericType(JvmGenericType object) {
				if (object.isInterface()) {
					int was = superTypes.consumeInterfaceOccurrence(object);
					if (was == 0) {
						return Boolean.FALSE;
					}
					if (was == 1) {
						computeAllOperations(this.isSuperclassBranch, this.superclassBranchOperations, object, processedOperations);
					}
					for (JvmTypeReference superType : object.getSuperTypes()) {
						doSwitch(superType);
					}
					return was > 1;
				} else if (seen.add(object)) {
					boolean saved = this.isSuperclassBranch;
					this.isSuperclassBranch = object != rootType;
					for (JvmTypeReference superType : object.getSuperTypes()) {
						doSwitch(superType);
					}
					this.isSuperclassBranch = saved;
					return Boolean.TRUE;
				}
				return Boolean.FALSE;
			}
			
			public void consume(JvmType rootType) {
				this.isSuperclassBranch = false;
				this.superclassBranchOperations = LinkedHashMultimap.create();
				doSwitch(rootType);
			}
			
		}
		new SuperInterfaceConsumer().consume(rootType);
	}

	private OverrideTester _getOverrideTester(AbstractResolvedOperation op) {
		try {
			Method meth = AbstractResolvedOperation.class.getDeclaredMethod("getOverrideTester");
			meth.setAccessible(true);
			return (OverrideTester) meth.invoke(op);
		} catch (Throwable exception) {
			throw new Error(exception);
		}
	}

	private boolean _handleOverridesAndConflicts(JvmOperation operation, Multimap<String, AbstractResolvedOperation> processedOperations) {
		try {
			Method meth = ResolvedFeatures.class.getDeclaredMethod("handleOverridesAndConflicts", JvmOperation.class, Multimap.class);
			meth.setAccessible(true);
			return ((Boolean) meth.invoke(this, operation, processedOperations)).booleanValue();
		} catch (Throwable exception) {
			throw new Error(exception);
		}
	}

	protected void computeAllOperations(boolean isSuperClassBranch, Multimap<String, AbstractResolvedOperation> superClassBranchOperations,
			JvmDeclaredType type, Multimap<String, AbstractResolvedOperation> processedOperations) {
		for (JvmOperation operation: type.getDeclaredOperations()) {
			boolean addToResult = true;
			if (targetVersion.isAtLeast(JavaVersion.JAVA8)) {
				addToResult = handleOverridesAndConflicts(isSuperClassBranch, operation, processedOperations, superClassBranchOperations);
			} else {
				String simpleName = operation.getSimpleName();
				if (processedOperations.containsKey(simpleName)) {
					addToResult = !isOverridden(operation, processedOperations.get(simpleName));
				}
			}
			if (addToResult) {
				BottomResolvedOperation resolvedOperation = createResolvedOperation(operation);
				processedOperations.put(operation.getSimpleName(), resolvedOperation);
				if (isSuperClassBranch) {
					superClassBranchOperations.put(operation.getSimpleName(), resolvedOperation);
				}
			}
		}
	}

	private boolean handleOverridesAndConflicts(boolean isSuperClassBranch,
			JvmOperation operation, Multimap<String, AbstractResolvedOperation> processedOperations,
			Multimap<String, AbstractResolvedOperation> superClassBranchOperations) {
		String simpleName = operation.getSimpleName();
		if (!processedOperations.containsKey(simpleName)) {
			return true;
		}
		List<AbstractResolvedOperation> conflictingOperations = null;
		Iterator<AbstractResolvedOperation> iterator = processedOperations.get(simpleName).iterator();
		while (iterator.hasNext()) {
			AbstractResolvedOperation candidate = iterator.next();
			OverrideTester overrideTester = _getOverrideTester(candidate);
			IOverrideCheckResult checkResult = overrideTester.isSubsignature(candidate, operation, false);
			if (checkResult.getDetails().contains(OverrideCheckDetails.DEFAULT_IMPL_CONFLICT)) {
				if (!isSuperClassBranch && superClassBranchOperations.containsKey(simpleName)) {
					iterator.remove();
				} else {
					// The current operation conflicts with the candidate
					if (conflictingOperations == null)
						conflictingOperations = Lists.newLinkedList();
					conflictingOperations.add(candidate);
				}
			} else if (checkResult.isOverridingOrImplementing()) {
				return false;
			}
		}
		if (conflictingOperations != null) {
			if (conflictingOperations.size() == 1 && conflictingOperations.get(0) instanceof ConflictingDefaultOperation) {
				// The current operation contributes to the already existing conflict
				ConflictingDefaultOperation conflictingDefaultOperation = (ConflictingDefaultOperation) conflictingOperations.get(0);
				boolean isOverridden = false;
				for (IResolvedOperation conflictingOp : conflictingDefaultOperation.getConflictingOperations()) {
					if (conflictingOp.getResolvedDeclarator().isSubtypeOf(operation.getDeclaringType())) {
						isOverridden = true;
						break;
					}
				}
				if (!isOverridden)
					conflictingDefaultOperation.getConflictingOperations().add(createResolvedOperation(operation));
				return false;
			}
			// A new conflict of default implementations was found
			if (operation.isAbstract()) {
				ConflictingDefaultOperation resolvedOperation = createConflictingOperation(conflictingOperations.get(0).getDeclaration());
				resolvedOperation.getConflictingOperations().add(createResolvedOperation(operation));
				for (AbstractResolvedOperation conflictingOp : conflictingOperations) {
					processedOperations.remove(simpleName, conflictingOp);
					if (conflictingOp.getDeclaration() != resolvedOperation.getDeclaration()) {
						resolvedOperation.getConflictingOperations().add(conflictingOp);
					}
				}
				processedOperations.put(simpleName, resolvedOperation);
			} else {
				ConflictingDefaultOperation resolvedOperation = createConflictingOperation(operation);
				for (AbstractResolvedOperation conflictingOp : conflictingOperations) {
					processedOperations.remove(simpleName, conflictingOp);
					resolvedOperation.getConflictingOperations().add(conflictingOp);
				}
				processedOperations.put(simpleName, resolvedOperation);
			}
			return false;
		}
		return true;
	}

}
