/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.validation.subvalidators;

import static com.google.common.collect.Iterables.filter;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_AGENT__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_BEHAVIOR__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_CAPACITY__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_EVENT__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_SKILL__EXTENDS;
import static io.sarl.lang.sarl.SarlPackage.Literals.SARL_SKILL__IMPLEMENTS;
import static io.sarl.lang.validation.IssueCodes.INVALID_EXTENDED_TYPE;
import static io.sarl.lang.validation.IssueCodes.INVALID_IMPLEMENTED_TYPE;
import static io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION;
import static io.sarl.lang.validation.IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE;
import static org.eclipse.xtend.core.validation.IssueCodes.OBSOLETE_ANNOTATION_OVERRIDE;
import static org.eclipse.xtend.core.validation.IssueCodes.OBSOLETE_OVERRIDE;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_CLASS__IMPLEMENTS;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FIELD__TYPE;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FUNCTION__NAME;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_INTERFACE__EXTENDS;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_MEMBER__MODIFIERS;
import static org.eclipse.xtext.util.Strings.isEmpty;
import static org.eclipse.xtext.xbase.validation.IssueCodes.CLASS_EXPECTED;
import static org.eclipse.xtext.xbase.validation.IssueCodes.CYCLIC_INHERITANCE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.DUPLICATE_FIELD;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_RETURN_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INTERFACE_EXPECTED;
import static org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_SYNCHRONIZED;
import static org.eclipse.xtext.xbase.validation.IssueCodes.MISSING_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.OVERRIDDEN_FINAL;
import static org.eclipse.xtext.xbase.validation.IssueCodes.OVERRIDE_REDUCES_VISIBILITY;
import static org.eclipse.xtext.xbase.validation.IssueCodes.WILDCARD_IN_SUPERTYPE;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.google.common.base.Objects;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Sets;
import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtend.core.jvmmodel.DispatchHelper;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.typesystem.override.IOverrideCheckResult.OverrideCheckDetails;
import org.eclipse.xtext.xbase.typesystem.override.IResolvedOperation;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.validation.JvmGenericTypeValidator;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.util.Utils;

/**
 * A specialization of {@link JvmGenericTypeValidator} to deal with inheritance mechanism of SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLInheritanceValidator extends AbstractSARLJvmGenericTypeValidator {

	@Inject
	private DispatchHelper dispatchHelper;

	private boolean checkRedundantInterfaceInSameType(
			XtendTypeDeclaration element,
			EReference structuralElement,
			LightweightTypeReference lightweightInterfaceReference,
			List<LightweightTypeReference> knownInterfaces) {
		final var iid = lightweightInterfaceReference.getUniqueIdentifier();
		var index = 1;
		for (final var previousInterface : knownInterfaces) {
			final var pid = previousInterface.getUniqueIdentifier();
			if (Objects.equal(iid, pid)) {
				error(MessageFormat.format(
						Messages.SARLInheritanceValidator_2,
						canonicalName(lightweightInterfaceReference)),
						element,
						structuralElement,
						// The index of the element to highlight in the super-types
						index,
						REDUNDANT_INTERFACE_IMPLEMENTATION,
						canonicalName(lightweightInterfaceReference),
						"pre"); //$NON-NLS-1$
				return true;
			}
			if (!isIgnored(REDUNDANT_INTERFACE_IMPLEMENTATION, element)) {
				if (getParentValidator().memberOfTypeHierarchy(previousInterface, lightweightInterfaceReference)) {
					addIssue(MessageFormat.format(
							Messages.SARLInheritanceValidator_1,
							canonicalName(lightweightInterfaceReference),
							canonicalName(previousInterface)),
							element,
							structuralElement,
							// The index of the element to highlight in the super-types
							index,
							REDUNDANT_INTERFACE_IMPLEMENTATION,
							canonicalName(lightweightInterfaceReference),
							"pre"); //$NON-NLS-1$
					return true;
				}
			}
			++index;
		}
		return false;
	}

	private void checkRedundantInterfaces(
			XtendTypeDeclaration element,
			EReference structuralElement,
			Iterable<? extends JvmTypeReference> interfaces,
			Iterable<? extends JvmTypeReference> superTypes) {
		final var knownInterfaces = CollectionLiterals.<LightweightTypeReference>newArrayList();
		for (final var interfaceRef : interfaces) {
			final var lightweightInterfaceReference = getParentValidator().toLightweightTypeReference(interfaceRef);
			// Detect if an interface is specified two types for the same type.
			if (!checkRedundantInterfaceInSameType(
					element, structuralElement,
					lightweightInterfaceReference,
					knownInterfaces)) {
				// Check the interface against the super-types
				if (superTypes != null && !isIgnored(REDUNDANT_INTERFACE_IMPLEMENTATION, element)) {
					for (final var superType : superTypes) {
						final var lightweightSuperType = getParentValidator().toLightweightTypeReference(superType);
						if (getParentValidator().memberOfTypeHierarchy(lightweightSuperType, lightweightInterfaceReference)) {
							addIssue(MessageFormat.format(
									Messages.SARLInheritanceValidator_1,
									canonicalName(lightweightInterfaceReference),
									canonicalName(lightweightSuperType)),
									element,
									structuralElement,
									// The index of the element to highlight in the super-types
									knownInterfaces.size(),
									REDUNDANT_INTERFACE_IMPLEMENTATION,
									canonicalName(lightweightInterfaceReference),
									"unknow"); //$NON-NLS-1$
						}
					}
				}
			}
			// Prepare next loop
			knownInterfaces.add(lightweightInterfaceReference);
		}
	}

	/** Check if implemented interfaces of a skill are redundant.
	 *
	 * @param skill the skill.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkRedundantImplementedInterfaces(SarlSkill skill) {
		checkRedundantInterfaces(
				skill,
				SARL_SKILL__IMPLEMENTS,
				skill.getImplements(),
				Utils.singletonList(skill.getExtends()));
	}

	/** Check if implemented interfaces of a Xtend Class are redundant.
	 *
	 * @param xtendClass the class.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkRedundantImplementedInterfaces(SarlClass xtendClass) {
		checkRedundantInterfaces(
				xtendClass,
				XTEND_CLASS__IMPLEMENTS,
				xtendClass.getImplements(),
				Utils.singletonList(xtendClass.getExtends()));
	}

	/** Check if implemented interfaces of a Xtend Interface are redundant.
	 *
	 * @param xtendInterface the interface.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkRedundantImplementedInterfaces(SarlInterface xtendInterface) {
		checkRedundantInterfaces(
				xtendInterface,
				XTEND_INTERFACE__EXTENDS,
				xtendInterface.getExtends(),
				Collections.<JvmTypeReference>emptyList());
	}

	@Override
	protected void checkSuperTypes(EObject sourceType, JvmGenericType type) {
		// Since there is specific checks of super types implemented in this class
		// we don't need to support them with the inherited function.
		if (sourceType instanceof SarlAgent agent) {
			checkSuperTypes(
					agent,
					SARL_AGENT__EXTENDS,
					Utils.singletonList(agent.getExtends()),
					Agent.class,
					false);
		} else if (sourceType instanceof SarlCapacity capacity) {
			checkSuperTypes(
					capacity,
					SARL_CAPACITY__EXTENDS,
					capacity.getExtends(),
					Capacity.class,
					false);
		} else if (sourceType instanceof SarlBehavior behavior) {
			checkSuperTypes(
					behavior,
					SARL_BEHAVIOR__EXTENDS,
					Utils.singletonList(behavior.getExtends()),
					Behavior.class,
					false);
		} else if (sourceType instanceof SarlEvent event) {
			checkSuperTypes(
					event,
					SARL_EVENT__EXTENDS,
					Utils.singletonList(event.getExtends()),
					Event.class,
					false);
		} else if (sourceType instanceof SarlSkill skill) {
			final int nbSuperTypes = checkSuperTypes(
					skill,
					SARL_SKILL__EXTENDS,
					Utils.singletonList(skill.getExtends()),
					Skill.class,
					false);
			checkImplementedTypes(
					skill,
					SARL_SKILL__IMPLEMENTS,
					skill.getImplements(),
					Capacity.class,
					nbSuperTypes > 0 ? 0 : 1, true);
		} else {
			// Check the OOP concepts
			super.checkSuperTypes(sourceType, type);

			// Check super types' conformance
			final var superTypes = type.getSuperTypes();
			for (int i = 0; i < superTypes.size(); ++i) {
				final var superType = superTypes.get(i);
				final var associated = getSuperTypeSourceElement(superType);
				if (associated == null) {
					// we still record this supertype for possible duplication checks
					continue; // synthetic superclass (e.g., Object)
				}
				final var eContainingFeature = associated.eContainingFeature();
				final var lighweightSuperType = getParentValidator().toLightweightTypeReference(superType);
				getParentValidator().doCheckValidSuperTypeArgumentDefinition(lighweightSuperType, sourceType,
						eContainingFeature, i, false, getMessageAcceptor());
			}
		}
	}

	/** Check the super type.
	 * 
	 * <p>This function supports the {@code expectedType} that is not
	 * supported by the inherited {@link #checkSuperTypes(EObject, JvmGenericType)}.
	 *
	 * @param element the child type.
	 * @param feature the syntactic feature related to the supertypes.
	 * @param superTypes the current super types.
	 * @param expectedType the expected root type.
	 * @param onlySubTypes if {@code true} only the subtype of the {@code expectedType} are valid;
	 * {@code false} if the {@code expectedType} is allowed.
	 * @return the count of supertypes.
	 */
	private int checkSuperTypes(
			XtendTypeDeclaration element,
			EStructuralFeature sourceFeature,
			List<? extends JvmTypeReference> superTypes,
			Class<?> expectedType,
			boolean onlySubTypes) {
		var nbSuperTypes = 0;
		final var inferredType = getAssociations().getInferredType(element);
		if (inferredType instanceof JvmGenericType) {
			final var inferredSuperTypes = CollectionLiterals.<JvmTypeReference>newLinkedList();
			inferredSuperTypes.addAll(inferredType.getSuperTypes());
			final var isExpectingInterface = expectedType.isInterface();
			var superTypeIndex = 0;
			for (final var superType : superTypes) {
				var success = true;
				if (superType != null) {
					final var feature = sourceFeature == null ? superType.eContainingFeature() : sourceFeature;
					final var jvmSuperType = superType.getType();
					if (jvmSuperType != null) {
						final var inferredSuperType = inferredSuperTypes.isEmpty() ? null : inferredSuperTypes.removeFirst();
						final var lighweightSuperType = getParentValidator().toLightweightTypeReference(superType);
						if (!(jvmSuperType instanceof JvmGenericType cvalue)
								|| (isExpectingInterface != cvalue.isInterface())) {
							if (isExpectingInterface) {
								getMessageAcceptor().acceptError(
										MessageFormat.format(Messages.SARLInheritanceValidator_3, Messages.SARLInheritanceValidator_4),
										element,
										feature,
										superTypeIndex,
										INTERFACE_EXPECTED,
										jvmSuperType.getIdentifier());
							} else {
								getMessageAcceptor().acceptError(
										MessageFormat.format(Messages.SARLInheritanceValidator_3, Messages.SARLInheritanceValidator_5),
										element,
										feature,
										superTypeIndex,
										CLASS_EXPECTED,
										jvmSuperType.getIdentifier());
							}
							success = false;
						} else if (getParentValidator().isFinal(lighweightSuperType)) {
							getMessageAcceptor().acceptError(Messages.SARLInheritanceValidator_6,
									element,
									feature,
									superTypeIndex,
									OVERRIDDEN_FINAL,
									inferredType.getIdentifier(),
									jvmSuperType.getIdentifier());
							success = false;
						} else if (!lighweightSuperType.isSubtypeOf(expectedType)
								|| (onlySubTypes && lighweightSuperType.isType(expectedType))) {
							if (onlySubTypes) {
								getMessageAcceptor().acceptError(MessageFormat.format(Messages.SARLInheritanceValidator_7, expectedType.getName()),
										element,
										feature,
										superTypeIndex,
										INVALID_EXTENDED_TYPE,
										jvmSuperType.getIdentifier());
							} else {
								getMessageAcceptor().acceptError(MessageFormat.format(Messages.SARLInheritanceValidator_8, expectedType.getName()),
										element,
										feature,
										superTypeIndex,
										INVALID_EXTENDED_TYPE,
										jvmSuperType.getIdentifier());
							}
							success = false;
						} else if (inferredSuperType == null
								|| !sameRawType(inferredSuperType.getType(), lighweightSuperType)
								|| sameRawType(inferredType, lighweightSuperType)
								|| hasCycleInHierarchy((JvmGenericType) inferredType, Sets.<JvmGenericType>newHashSet())) {
							getMessageAcceptor().acceptError(MessageFormat.format(Messages.SARLInheritanceValidator_9,
									inferredType.getQualifiedName()),
									element,
									feature,
									superTypeIndex,
									CYCLIC_INHERITANCE,
									jvmSuperType.getIdentifier());
							success = false;
						} else {
							success = getParentValidator().doCheckValidSuperTypeArgumentDefinition(lighweightSuperType,
									element, feature, superTypeIndex, false, getMessageAcceptor());
						}
					} else {
						getMessageAcceptor().acceptError(MessageFormat.format(Messages.SARLInheritanceValidator_9,
								inferredType.getQualifiedName()),
								element,
								feature,
								superTypeIndex,
								CYCLIC_INHERITANCE,
								superType.getIdentifier());
						success = false;
					}
					if (success) {
						checkWildcardSupertype(element, superType, feature, superTypeIndex);
					}
				}
				++superTypeIndex;
				if (success) {
					++nbSuperTypes;
				}
			}
		}
		return nbSuperTypes;
	}

	private static boolean sameRawType(JvmType type0, LightweightTypeReference type1) {
		final var id0 = type0.getIdentifier();
		final var id1 = type1.getRawTypeReference().getIdentifier();
		return Objects.equal(id0, id1);
	}

	/** Check the implemented type.
	 *
	 * @param element the child type.
	 * @param feature the syntactic feature related to the supertypes.
	 * @param implementedTypes the current super types.
	 * @param expectedType the expected root type.
	 * @param mandatoryNumberOfTypes the minimal number of implemented types.
	 * @param onlySubTypes if {@code true} only the subtype of the {@code expectedType} are valid;
	 * {@code false} if the {@code expectedType} is allowed.
	 * @return the count of supertypes.
	 */
	private boolean checkImplementedTypes(
			XtendTypeDeclaration element,
			EReference feature,
			List<? extends JvmTypeReference> implementedTypes,
			Class<?> expectedType,
			int mandatoryNumberOfTypes,
			boolean onlySubTypes) {
		var success = true;
		var nb = 0;
		var index = 0;
		for (final var superType : implementedTypes) {
			final var  ref = getParentValidator().toLightweightTypeReference(superType);
			if (ref != null
					&& (!ref.isInterfaceType() || !ref.isSubtypeOf(expectedType)
							|| (onlySubTypes && ref.isType(expectedType)))) {
				final String msg;
				if (onlySubTypes) {
					msg = Messages.SARLInheritanceValidator_10;
				} else {
					msg = Messages.SARLInheritanceValidator_11;
				}
				error(MessageFormat.format(
						msg,
						superType.getQualifiedName(),
						expectedType.getName(),
						element.getName()),
						element,
						feature,
						index,
						INVALID_IMPLEMENTED_TYPE,
						superType.getSimpleName());
				success = false;
			} else {
				++nb;
			}
			++index;
		}
		if (nb < mandatoryNumberOfTypes) {
			error(MessageFormat.format(
					Messages.SARLInheritanceValidator_12,
					expectedType.getName(),
					element.getName()),
					element,
					feature,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					MISSING_TYPE);
			success = false;
		}
		return success;
	}

	/** Check if the super types is valid and have no wildcard.
	 *
	 * @param xtendType the current type.
	 * @param superTypeReference the super type.
	 * @param feature the syntactic feature related to the supertypes.
	 * @param index the index of the super type in the list of super types.
	 */
	private void checkWildcardSupertype(XtendTypeDeclaration xtendType, JvmTypeReference superTypeReference,
			EStructuralFeature feature, int index) { 
		if (isInvalidWildcard(superTypeReference)) { 
			error(MessageFormat.format(Messages.SARLInheritanceValidator_13, xtendType.getName(), superTypeReference.getIdentifier()),
					feature, index, WILDCARD_IN_SUPERTYPE);
		}
	}

	/** Determine is a cycle is defined in the type inheritance hierarchy.
	 *
	 * @param type the type to test.
	 * @param processedSuperTypes set of types that are been marked are processed.
	 * @return {@code true} if a cycle was detected.
	 * @since 0.14
	 */
	private boolean hasCycleInHierarchy(JvmGenericType type, Set<JvmGenericType> processedSuperTypes) {
		JvmDeclaredType container = type;
		do {
			if (processedSuperTypes.contains(container)) {
				return true;
			}
			container = container.getDeclaringType();
		} while (container != null);
		processedSuperTypes.add(type);
		for (JvmTypeReference superTypeRef : type.getSuperTypes()) {
			if (superTypeRef.getType() instanceof JvmGenericType cvalue) {
				if (hasCycleInHierarchy(cvalue, processedSuperTypes)) {
					return true;
				}
			}
		}
		processedSuperTypes.remove(type);
		return false;
	}

	@Check(CheckType.EXPENSIVE)
	public void checkNoTypeNameShadowing(XtendTypeDeclaration type) {
		final var name = type.getName();
		if (name != null && name.length() > 0) {
			var outer = EcoreUtil2.getContainerOfType(type.eContainer(), XtendTypeDeclaration.class);
			while (outer != null) {
				if (name.equals(outer.getName())) {
					acceptError(MessageFormat.format(Messages.SARLInheritanceValidator_14, name),
							type, XtendPackage.Literals.XTEND_TYPE_DECLARATION__NAME, INSIGNIFICANT_INDEX, INVALID_MEMBER_NAME);
					return;
				}
				outer = EcoreUtil2.getContainerOfType(outer.eContainer(), XtendTypeDeclaration.class);
			}
		}
	}

	@Override
	protected void checkMemberNamesAreUnique(JvmGenericType genericType) {
		super.checkMemberNamesAreUnique(genericType);
		final var primarySourceElement = getPrimarySourceElement(genericType);
		if (primarySourceElement instanceof XtendTypeDeclaration xtendType) {
			final var type2extension = HashMultimap.<JvmType, XtendField>create();
			for (final var member : xtendType.getMembers()) {
				if (member instanceof XtendField field) {
					if (isEmpty(field.getName())) {
						if (field.isExtension()) {
							final var typeReference = field.getType();
							if (typeReference != null) {
								final var type = typeReference.getType();
								if (type != null) {
									type2extension.put(type, field);
								}
							}
						}
					}
				}
			}
			for (final var type: type2extension.keySet()) {
				final var fields = type2extension.get(type);
				if (fields.size() >1) {
					for (final var field: fields) {
						error(Messages.SARLInheritanceValidator_15, field, XTEND_FIELD__TYPE, DUPLICATE_FIELD);
					}
				}
			}
		}
	}

	@Override
	protected void checkFunctionOverrides(IResolvedOperation operation, Set<EObject> flaggedOperations) {
		super.checkFunctionOverrides(operation, flaggedOperations);
		final var sourceElement = findPrimarySourceElement(operation);
		if (sourceElement != null) {
			final var allInherited = operation.getOverriddenAndImplementedMethods();
			if (allInherited.isEmpty()) {
				if (sourceElement instanceof XtendFunction function && flaggedOperations.add(sourceElement)) {
					if (function.isOverride()) {
						error(MessageFormat.format(Messages.SARLInheritanceValidator_16,
								operation.getSimpleSignature(), getDeclaratorName(operation.getDeclaration())), 
								function, XTEND_MEMBER__MODIFIERS, function.getModifiers().indexOf(
										getGrammarAccess().getOverrideKeyword()), OBSOLETE_OVERRIDE);
					} else {
						for (final var anno : function.getAnnotations()) {
							if (anno != null && anno.getAnnotationType() != null && Override.class.getName().equals(anno.getAnnotationType().getIdentifier())) {
								error(Messages.SARLInheritanceValidator_17, anno, null, OBSOLETE_ANNOTATION_OVERRIDE);
							}
						}
					}
				}
			}
		}
	}

	/** This function is overridden to make {@code MISSING_OVERRIDE} a warning and not an error.
	 * The generation of the "RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED" warning is also added.
	 *
	 * {@inheritDoc}
	 */
	@Override
	protected void checkFunctionOverrides(EObject sourceElement, IResolvedOperation resolved,
			List<IResolvedOperation> allInherited) {
		var overrideProblems = false;
		List<IResolvedOperation> exceptionMismatch = null;
		for (final var inherited: allInherited) {
			if (inherited.getOverrideCheckResult().hasProblems()) {
				overrideProblems = true;
				final var details = inherited.getOverrideCheckResult().getDetails();
				if (details.contains(OverrideCheckDetails.IS_FINAL)) {
					error(MessageFormat.format(Messages.SARLInheritanceValidator_21, inherited.getSimpleSignature()), sourceElement,
							getFeatureForIssue(sourceElement), OVERRIDDEN_FINAL);
				} else if (details.contains(OverrideCheckDetails.REDUCED_VISIBILITY)) {
					error(MessageFormat.format(Messages.SARLInheritanceValidator_22, inherited.getSimpleSignature()),
							sourceElement, getFeatureForIssue(sourceElement), OVERRIDE_REDUCES_VISIBILITY);
				} else if (details.contains(OverrideCheckDetails.EXCEPTION_MISMATCH)) {
					if (exceptionMismatch == null) {
						exceptionMismatch = new ArrayList<>(allInherited.size());
					}
					exceptionMismatch.add(inherited);
				} else if (details.contains(OverrideCheckDetails.RETURN_MISMATCH)) {
					error(MessageFormat.format(Messages.SARLInheritanceValidator_23, inherited.getSimpleSignature()), sourceElement,
							returnTypeFeature(sourceElement, resolved), INCOMPATIBLE_RETURN_TYPE);
				} else if (details.contains(OverrideCheckDetails.SYNCHRONIZED_MISMATCH)) {
					warning(Messages.SARLInheritanceValidator_24, sourceElement,
							getFeatureForIssue(sourceElement), MISSING_SYNCHRONIZED);
				}
			} else if (!isIgnored(RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED, sourceElement)
					&& sourceElement instanceof SarlAction function) {
				if (function.getReturnType() == null && !inherited.getResolvedReturnType().isPrimitiveVoid()) {
					addIssue(MessageFormat.format(Messages.SARLInheritanceValidator_20,
							resolved.getResolvedReturnType().getHumanReadableName()),
							sourceElement,
							returnTypeFeature(sourceElement), RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED,
							inherited.getResolvedReturnType().getIdentifier());
				}
			}
		}

		if (exceptionMismatch != null) {
			createExceptionMismatchError(resolved, sourceElement, exceptionMismatch);
		}

		if (sourceElement instanceof XtendFunction function) {
			if (!overrideProblems && !function.isOverride() && !function.isStatic()) {
				addIssue(MessageFormat.format(Messages.SARLInheritanceValidator_18,
						resolved.getSimpleSignature(), getDeclaratorName(resolved)),
						function, XTEND_FUNCTION__NAME, MISSING_OVERRIDE);
			} 
			if (!overrideProblems && function.isOverride() && function.isStatic()) {
				for (final var inherited: allInherited) {
					error(MessageFormat.format(Messages.SARLInheritanceValidator_19,
							resolved.getSimpleSignature(), getDeclaratorName(resolved),
							resolved.getSimpleSignature(), getDeclaratorName(inherited)),
							function, XTEND_FUNCTION__NAME, function.getModifiers().indexOf(
									getGrammarAccess().getOverrideKeyword()), OBSOLETE_OVERRIDE);
				}
			}
			if (function.isOverride()) {
				for (final var anno : function.getAnnotations()) {
					if (anno != null && anno.getAnnotationType() != null && Override.class.getName().equals(anno.getAnnotationType().getIdentifier())) {
						warning(Messages.SARLInheritanceValidator_17, anno, null, OBSOLETE_ANNOTATION_OVERRIDE);
					}
				}
			}
		}
	}

	/** This function is overridden to avoid the output of the {@code MISSING_CONSTRUCTOR} issue because
	 * the constructors from the super type are "inherited".
	 * 
	 * {@inheritDoc}
	 */
	@Override
	protected void checkDefaultSuperConstructor(EObject sourceType, JvmGenericType type) {
		// The inherited check behavior for the constructors is applicable only when the SARL coder has
		// written an explicitly constructor. If no constructor is provided by the SARL code, then
		// the JVM model inferrer is generating automatically constructors with the same signature as
		// the constructor of the super type.
		final var container = filterConstructorContainer(sourceType);
		if (container != null) {
			final var sarlConstructors = filter(container.getMembers(), XtendConstructor.class);
			if (sarlConstructors.iterator().hasNext()) {
				super.checkDefaultSuperConstructor(sourceType, type);
			}
		}
	}

	/** Replies if the given type is an agent-oriented type that could receives a constructor declaration.
	 *
	 * @param type is the type to test.
	 * @return the {@code type} casted to a constructor container type, or {@code null} if the given object is not a constructor container.
	 * @since 0.14
	 */
	@SuppressWarnings("static-method")
	private XtendTypeDeclaration filterConstructorContainer(EObject object) {
		if (object instanceof SarlAgent cvalue) {
			return cvalue;
		}
		if (object instanceof SarlBehavior cvalue) {
			return cvalue;
		}
		if (object instanceof SarlSkill cvalue) {
			return cvalue;
		}
		if (object instanceof SarlEvent cvalue) {
			return cvalue;
		}
		if (object instanceof SarlClass cvalue) {
			return cvalue;
		}
		return null;
	}

	@Override
	protected EStructuralFeature returnTypeFeature(EObject member, IResolvedOperation resolved) {
		var returnTypeFeature = super.returnTypeFeature(member, resolved);
		if (returnTypeFeature == null && member instanceof XtendField) // e.g., for an active annotation like @Accessors
			return XTEND_FIELD__TYPE;
		return returnTypeFeature;
	}

	/**
	 * Dispatcher {@link JvmOperation}s must not be checked by the {@link JvmGenericTypeValidator}.
	 */
	@Override
	protected boolean shouldBeValidated(JvmIdentifiableElement element) {
		if (element instanceof JvmOperation op && this.dispatchHelper.isDispatcherFunction(op)) {
			return false;
		}
		return super.shouldBeValidated(element);
	}

}
