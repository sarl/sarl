/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import static org.eclipse.xtend.core.validation.IssueCodes.CONFLICTING_DEFAULT_METHODS;
import static org.eclipse.xtend.core.validation.IssueCodes.DUPLICATE_METHOD;

import java.util.List;
import java.util.Set;

import javax.inject.Inject;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.xbase.typesystem.override.ConflictingDefaultOperation;
import org.eclipse.xtext.xbase.typesystem.override.IResolvedOperation;
import org.eclipse.xtext.xbase.typesystem.override.ResolvedFeatures;
import org.eclipse.xtext.xbase.typesystem.util.ContextualVisibilityHelper;
import org.eclipse.xtext.xbase.typesystem.util.IVisibilityHelper;
import org.eclipse.xtext.xbase.typesystem.util.RecursionGuard;

import io.sarl.lang.validation.SARLValidator;

/**
 * Fixing the SARL issue 621: Error on multiple function inheritance.
 *
 * <p>Issue is due to Xtend issue 191 (https://github.com/eclipse/xtext-xtend/pull/191),
 * and the associated PR 192 (https://github.com/eclipse/xtext-xtend/pull/192)
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
public class Bug621SARLValidator extends SARLValidator {

	@Inject
	private IVisibilityHelper visibilityHelper;

	private boolean contributesToConflict(JvmGenericType rootType, ConflictingDefaultOperation conflictingDefaultOperation) {
		Set<JvmDeclaredType> involvedInterfaces = Sets.newHashSet();
		involvedInterfaces.add(conflictingDefaultOperation.getDeclaration().getDeclaringType());
		for (IResolvedOperation conflictingOperation : conflictingDefaultOperation.getConflictingOperations()) {
			involvedInterfaces.add(conflictingOperation.getDeclaration().getDeclaringType());
		}
		RecursionGuard<JvmDeclaredType> recursionGuard = new RecursionGuard<JvmDeclaredType>();
		if (rootType.isInterface()) {
			int contributingCount = 0;
			for (JvmTypeReference typeRef : rootType.getExtendedInterfaces()) {
				JvmType rawType = typeRef.getType();
				if (rawType instanceof JvmDeclaredType && contributesToConflict((JvmDeclaredType) rawType, involvedInterfaces, recursionGuard)) {
					contributingCount++;
				}
			}
			return contributingCount >= 2;
		} else {
			return contributesToConflict(rootType, involvedInterfaces, recursionGuard);
		}
	}
	
	private boolean contributesToConflict(JvmDeclaredType type, Set<JvmDeclaredType> involvedInterfaces,
			RecursionGuard<JvmDeclaredType> guard) {
		if (!guard.tryNext(type)) {
			return false;
		}
		if (involvedInterfaces.contains(type)) {
			return true;
		}
		for (JvmTypeReference typeRef : type.getExtendedInterfaces()) {
			JvmType rawType = typeRef.getType();
			if (rawType instanceof JvmDeclaredType && contributesToConflict((JvmDeclaredType) rawType, involvedInterfaces, guard)) {
				return true;
			}
		}
		return false;
	}

	@Override
	protected void doCheckOverriddenMethods(XtendTypeDeclaration xtendType, JvmGenericType inferredType,
			ResolvedFeatures resolvedFeatures, Set<EObject> flaggedOperations) {
		List<IResolvedOperation> operationsMissingImplementation = null;
		boolean doCheckAbstract = !inferredType.isAbstract();
		if (doCheckAbstract) {
			operationsMissingImplementation = Lists.newArrayList();
		}
		IVisibilityHelper visibilityHelper = new ContextualVisibilityHelper(this.visibilityHelper, resolvedFeatures.getType());
		boolean flaggedType = false;
		for (IResolvedOperation operation : resolvedFeatures.getAllOperations()) {
			JvmDeclaredType operationDeclaringType = operation.getDeclaration().getDeclaringType();
			if (operationDeclaringType != inferredType) {
				if (operationsMissingImplementation != null && operation.getDeclaration().isAbstract()) {
					operationsMissingImplementation.add(operation);
				}
				if (visibilityHelper.isVisible(operation.getDeclaration())) {
					String erasureSignature = operation.getResolvedErasureSignature();
					List<IResolvedOperation> declaredOperationsWithSameErasure = 
							resolvedFeatures.getDeclaredOperations(erasureSignature);
					for (IResolvedOperation localOperation: declaredOperationsWithSameErasure) {
						if (!localOperation.isOverridingOrImplementing(operation.getDeclaration()).isOverridingOrImplementing()) {
							EObject source = findPrimarySourceElement(localOperation);
							if (flaggedOperations.add(source)) {
								if (operation.getDeclaration().isStatic() && !localOperation.getDeclaration().isStatic()) {
									error("The instance method "
											+ localOperation.getSimpleSignature()
											+ " cannot override the static method "
											+ operation.getSimpleSignature() + " of type "
											+ getDeclaratorName(operation.getDeclaration()) + ".",
											source, nameFeature(source), DUPLICATE_METHOD);
								} else {
									error("Name clash: The method "
											+ localOperation.getSimpleSignature() + " of type "
											+ inferredType.getSimpleName()
											+ " has the same erasure as "
											+
											// use source with other operations parameters to avoid confusion
											// due to name transformations in JVM model inference
											operation.getSimpleSignature() + " of type "
											+ getDeclaratorName(operation.getDeclaration()) + " but does not override it.",
											source, nameFeature(source), DUPLICATE_METHOD);
								}
							}
						}
					}
					if (operation instanceof ConflictingDefaultOperation
							&& contributesToConflict(inferredType, (ConflictingDefaultOperation) operation)
							&& !flaggedType) {
						IResolvedOperation conflictingOperation = ((ConflictingDefaultOperation) operation).getConflictingOperations().get(0);
						// Include the declaring class in the issue code in order to give better quick fixes
						String[] uris = new String[] {
								getDeclaratorName(operation.getDeclaration()) + "|"
										+ EcoreUtil.getURI(operation.getDeclaration()).toString(),
								getDeclaratorName(conflictingOperation.getDeclaration()) + "|"
										+ EcoreUtil.getURI(conflictingOperation.getDeclaration()).toString()
							};
						if (!operation.getDeclaration().isAbstract() && !operation.getDeclaration().isDefault()
							&& !conflictingOperation.getDeclaration().isAbstract() && !conflictingOperation.getDeclaration().isDefault()) {
							error("The type " + inferredType.getSimpleName()
									+ " inherits multiple implementations of the method " + conflictingOperation.getSimpleSignature()
									+ " from " + getDeclaratorName(conflictingOperation.getDeclaration())
									+ " and " + getDeclaratorName(operation.getDeclaration()) + ".",
									xtendType, XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME,
									CONFLICTING_DEFAULT_METHODS, uris);
						} else if (!operation.getDeclaration().isDefault() && !conflictingOperation.getDeclaration().isDefault()) {
							// At least one of the operations is non-abstract
							IResolvedOperation abstractOp, nonabstractOp;
							if (operation.getDeclaration().isAbstract()) {
								abstractOp = operation;
								nonabstractOp = conflictingOperation;
							} else {
								abstractOp = conflictingOperation;
								nonabstractOp = operation;
							}
							error("The non-abstract method " + nonabstractOp.getSimpleSignature()
									+ " inherited from " + getDeclaratorName(nonabstractOp.getDeclaration())
									+ " conflicts with the method " + abstractOp.getSimpleSignature()
									+ " inherited from " + getDeclaratorName(abstractOp.getDeclaration()) + ".",
									xtendType, XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME,
									CONFLICTING_DEFAULT_METHODS, uris);
						}
						flaggedType = true;
					}
				}
			}
		}
		if (operationsMissingImplementation != null && !operationsMissingImplementation.isEmpty() && !flaggedType) {
			reportMissingImplementations(xtendType, inferredType, operationsMissingImplementation);
		}
	}

}
