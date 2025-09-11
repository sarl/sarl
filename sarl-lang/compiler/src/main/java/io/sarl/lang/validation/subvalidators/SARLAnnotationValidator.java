/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import static com.google.common.collect.Iterables.any;
import static io.sarl.lang.validation.IssueCodes.INVALID_DEFAULT_SKILL_ANNOTATION;
import static io.sarl.lang.validation.IssueCodes.MANUAL_INLINE_DEFINITION;
import static io.sarl.lang.validation.IssueCodes.PROGRAMMATIC_ISSUE_ANNOTATION;
import static io.sarl.lang.validation.IssueCodes.USED_RESERVED_SARL_ANNOTATION;
import static org.eclipse.xtend.core.validation.IssueCodes.ANNOTATION_MULTIPLE;
import static org.eclipse.xtend.core.validation.IssueCodes.ANNOTATION_WRONG_TARGET;
import static org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE;

import java.lang.annotation.ElementType;
import java.text.MessageFormat;
import java.util.List;
import java.util.Objects;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendAnnotationTarget;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtend.lib.annotations.Delegate;
import org.eclipse.xtend.lib.annotations.EqualsHashCode;
import org.eclipse.xtend.lib.annotations.FinalFieldsConstructor;
import org.eclipse.xtend.lib.annotations.ToString;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmAnnotationValue;
import org.eclipse.xtext.common.types.JvmCustomAnnotationValue;
import org.eclipse.xtext.common.types.JvmStringAnnotationValue;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XStringLiteral;
import org.eclipse.xtext.xbase.XTypeLiteral;
import org.eclipse.xtext.xbase.annotations.typing.XAnnotationUtil;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotationsPackage;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multimaps;
import com.google.inject.Inject;

import io.sarl.lang.core.DefaultSkill;
import io.sarl.lang.core.annotation.EarlyExit;
import io.sarl.lang.core.annotation.ErrorOnCall;
import io.sarl.lang.core.annotation.InfoOnCall;
import io.sarl.lang.core.annotation.WarningOnCall;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlAnnotationType;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlEnumeration;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlFormalParameter;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;

/**
 * An implementation of a validator to deal with annotations.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLAnnotationValidator extends AbstractSARLSubValidatorWithParentLink {

	/** Information about that annotation targets.
	 */
	protected final Multimap<Class<?>, ElementType> targetInfos;
	
	{
		final var result = ImmutableMultimap.<Class<?>, ElementType>builder();
		result.put(XtendClass.class, ElementType.TYPE);
		result.put(XtendInterface.class, ElementType.TYPE);
		result.put(XtendEnum.class, ElementType.TYPE);
		result.putAll(XtendAnnotationType.class, ElementType.ANNOTATION_TYPE, ElementType.TYPE);
		result.put(XtendField.class, ElementType.FIELD);
		result.put(XtendFunction.class, ElementType.METHOD);
		result.put(XtendParameter.class, ElementType.PARAMETER);
		result.put(SarlAgent.class, ElementType.TYPE);
		result.put(SarlCapacity.class, ElementType.TYPE);
		result.put(SarlSkill.class, ElementType.TYPE);
		result.put(SarlEvent.class, ElementType.TYPE);
		result.put(SarlBehavior.class, ElementType.TYPE);
		result.put(SarlSpace.class, ElementType.TYPE);
		result.put(SarlArtifact.class, ElementType.TYPE);
		result.put(SarlClass.class, ElementType.TYPE);
		result.put(SarlInterface.class, ElementType.TYPE);
		result.put(SarlEnumeration.class, ElementType.TYPE);
		result.putAll(SarlAnnotationType.class, ElementType.ANNOTATION_TYPE, ElementType.TYPE);
		result.put(SarlField.class, ElementType.FIELD);
		result.put(SarlAction.class, ElementType.METHOD);
		result.put(SarlFormalParameter.class, ElementType.PARAMETER);
		this.targetInfos = result.build();
	}

	@Inject
	private IQualifiedNameConverter qualifiedNameConverter;

	@Inject
	private XAnnotationUtil annotationUtil;

	@Inject
	private AnnotationLookup annotationLookup;

	/** Check for reserved annotations.
	 *
	 * @param annotationTarget thee target to test.
	 */
	@Check(CheckType.NORMAL)
	public void checkReservedAnnotation(XtendAnnotationTarget annotationTarget) {
		if (!isIgnored(USED_RESERVED_SARL_ANNOTATION)) {
			if (annotationTarget.getAnnotations().isEmpty() || !isRelevantAnnotationTarget(annotationTarget)) {
				return;
			}
			final var reservedPackage = this.qualifiedNameConverter.toQualifiedName(
					EarlyExit.class.getPackage().getName());
			final var earlyExitAnnotation = EarlyExit.class.getName();
			final var errorOnCallAnnotation = ErrorOnCall.class.getName();
			final var warningOnCallAnnotation = WarningOnCall.class.getName();
			final var infoOnCallAnnotation = InfoOnCall.class.getName();
			for (final var annotation : annotationTarget.getAnnotations()) {
				final var type = annotation.getAnnotationType();
				if (type != null && !type.eIsProxy()) {
					if (Objects.equals(type.getIdentifier(), earlyExitAnnotation)) {
						// Special case: EarlyExit is allowed on events for declaring early-exit events
						if (!(annotationTarget instanceof SarlEvent)) {
							addIssue(
									MessageFormat.format(Messages.SARLAnnotationValidator_1, type.getSimpleName()),
									annotation,
									USED_RESERVED_SARL_ANNOTATION);
						}
					} else if (!Objects.equals(type.getIdentifier(), errorOnCallAnnotation)
							&& !Objects.equals(type.getIdentifier(), warningOnCallAnnotation)
							&& !Objects.equals(type.getIdentifier(), infoOnCallAnnotation)) {
						final var annotationName = this.qualifiedNameConverter.toQualifiedName(
								type.getIdentifier());
						if (annotationName.startsWith(reservedPackage)) {
							addIssue(
									MessageFormat.format(Messages.SARLAnnotationValidator_1, type.getSimpleName()),
									annotation,
									USED_RESERVED_SARL_ANNOTATION);
						}
					}
				}
			}
		}
	}

	private boolean isRelevantAnnotationTarget(XtendAnnotationTarget annotationTarget) {
		return any(this.targetInfos.keySet(), new Predicate<Class<?>>() {
			@Override
			public boolean apply(Class<?> input) {
				return input.isInstance(annotationTarget);
			}
		});
	}

	/** Replies if the given annotation is a forbidden active annotation.
	 *
	 * @param annotation the annotation.
	 * @return {@code true} if the annotation is forbidden.
	 */
	@SuppressWarnings("static-method")
	private boolean isForbiddenActiveAnnotation(XAnnotation annotation) {
		final var name = annotation.getAnnotationType().getQualifiedName();
		return Strings.equal(EqualsHashCode.class.getName(), name)
				|| Strings.equal(FinalFieldsConstructor.class.getName(), name);
	}

	/** Check for {@code @Inline} annotation usage.
	 *
	 * @param annotationTarget thee target to test.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkManualInlineDefinition(XtendAnnotationTarget annotationTarget) {
		if (!isIgnored(MANUAL_INLINE_DEFINITION)) {
			if (annotationTarget.getAnnotations().isEmpty() || !isRelevantAnnotationTarget(annotationTarget)) {
				return;
			}
			final var inlineAnnotation = Inline.class.getName();
			for (final var annotation : annotationTarget.getAnnotations()) {
				final var type = annotation.getAnnotationType();
				if (type != null && !type.eIsProxy()) {
					if (Objects.equals(type.getIdentifier(), inlineAnnotation)) {
						addIssue(
								Messages.SARLAnnotationValidator_2,
								annotation,
								MANUAL_INLINE_DEFINITION);
					}
				}
			}
		}
	}

	/** Check the correct usage of the {@link io.sarl.lang.core.DefaultSkill} annotation.
	 *
	 * @param capacity the associated capacity to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkDefaultSkillAnnotation(SarlCapacity capacity) {
		final var annotationId = DefaultSkill.class.getName();
		final var annotation = IterableExtensions.findFirst(capacity.getAnnotations(), it -> {
			return Boolean.valueOf(Strings.equal(annotationId, it.getAnnotationType().getIdentifier()));
		});
		if (annotation != null) {
			final var expr = annotation.getValue();
			if (expr instanceof XTypeLiteral typeLiteral) {
				final var type = typeLiteral.getType();
				if (type != null && !type.eIsProxy()) {
					final var reference = getParentValidator().toLightweightTypeReference(type, capacity);
					// Validating by the annotation value's type.
					if (getInheritanceHelper().isSarlSkill(reference)) {
						final var element = getAssociations().getPrimaryJvmElement(capacity);
						assert element instanceof JvmType;
						if (!reference.isSubtypeOf((JvmType) element)) {
							error(MessageFormat.format(
									Messages.SARLAnnotationValidator_3,
									capacity.getName(), type.getSimpleName()),
									expr,
									null,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									INVALID_DEFAULT_SKILL_ANNOTATION);
						}
						return;
					}
				}
			}
			error(Messages.SARLAnnotationValidator_4,
					expr,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					INVALID_DEFAULT_SKILL_ANNOTATION);
		}
	}

	/** Check if element has an programmatic issue message.
	 *
	 * @param expression the expression.
	 * @since 0.12
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkProgrammaticIssueMessage(XAbstractFeatureCall expression) {
		if (expression != null && expression.getFeature() != null) {
			final var feature = expression.getFeature();
			if (feature instanceof JvmAnnotationTarget target) {
				final var errorAnnoName = ErrorOnCall.class.getName();
				final var warningAnnoName = WarningOnCall.class.getName();
				final var infoAnnoName = InfoOnCall.class.getName();
				for (final var annotation : target.getAnnotations()) {
					final var values = annotation.getValues();
					if (!values.isEmpty()) {
						if (Objects.equals(errorAnnoName, annotation.getAnnotation().getIdentifier())) {
							final var message = parseIssueOnCallAnnotation(values);
							error(
									message,
									expression,
									null,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									PROGRAMMATIC_ISSUE_ANNOTATION);
						} else if (Objects.equals(warningAnnoName, annotation.getAnnotation().getIdentifier())) {
							final var message = parseIssueOnCallAnnotation(values);
							warning(
									message,
									expression,
									null,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									PROGRAMMATIC_ISSUE_ANNOTATION);
						} else if (Objects.equals(infoAnnoName, annotation.getAnnotation().getIdentifier())) {
							final var message = parseIssueOnCallAnnotation(values);
							info(
									message,
									expression,
									null,
									ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
									PROGRAMMATIC_ISSUE_ANNOTATION);
						}
					}
				}
			}
		}
	}

	private static String parseIssueOnCallAnnotation(List<JvmAnnotationValue> values) {
		final var message = new StringBuilder();
		for (final var value : values) {
			if (value instanceof JvmStringAnnotationValue cvalue) {
				message.append(cvalue.getValues());
			} else if (value instanceof JvmCustomAnnotationValue cvalue) {
				for (final var obj : cvalue.getValues()) {
					if (obj instanceof XStringLiteral cvalue0) {
						message.append(cvalue0.getValue());
					}
				}
			}
		}
		return message.toString();
	}

	/** Replies if the given annotation is an active annotation for object-oriented elements.
	 *
	 * @param annotation the annotation.
	 * @return {@code true} if the annotation should be used only for OO elements.
	 * @see #isAOActiveAnnotation(XAnnotation)
	 */
	@SuppressWarnings("static-method")
	protected boolean isOOActiveAnnotation(XAnnotation annotation) {
		final var name = annotation.getAnnotationType().getQualifiedName();
		return Strings.equal(Accessors.class.getName(), name)
				|| Strings.equal(Data.class.getName(), name)
				|| Strings.equal(Delegate.class.getName(), name)
				|| Strings.equal(ToString.class.getName(), name);
	}

	/** Replies if the given container can receive an active annotation.
	 *
	 * @param annotation the annotation.
	 * @return {@code true} if the annotation should be used only for OO elements.
	 * @see #isOOActiveAnnotation(XAnnotation)
	 * @see #isAOActiveAnnotationReceiver(XtendTypeDeclaration)
	 */
	@SuppressWarnings("static-method")
	protected boolean isAOActiveAnnotation(XAnnotation annotation) {
		final var name = annotation.getAnnotationType().getQualifiedName();
		return Strings.equal(Accessors.class.getName(), name);
	}

	/** Replies if the given annotation is an active annotation for agent-oriented elements.
	 *
	 * @param container the container to test.
	 * @return {@code true} if the container could receive an active annotation.
	 * @see #isOOActiveAnnotation(XAnnotation)
	 * @see #isAOActiveAnnotation(XAnnotation)
	 */
	@SuppressWarnings("static-method")
	protected boolean isAOActiveAnnotationReceiver(XtendTypeDeclaration container) {
		return container instanceof SarlAgent
				|| container instanceof SarlBehavior
				|| container instanceof SarlSkill;
	}

	/** Replies if the given element is an object oriented type.
	 *
	 * @param type the type to test.
	 * @return {@code true} if the type is an object oriented type.
	 */
	@SuppressWarnings("static-method")
	protected boolean isOOType(XtendTypeDeclaration type) {
		return type instanceof XtendClass
				|| type instanceof XtendInterface
				|| type instanceof XtendEnum
				|| type instanceof XtendAnnotationType;
	}

	/** Check the annotations for the given annotation target.
	 * 
	 * @param annotation the annotation to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkAnnotationTarget(XAnnotation annotation) {
		final var annotationType = annotation.getAnnotationType();
		if (annotationType == null || annotationType.eIsProxy() || !(annotationType instanceof JvmAnnotationType)) {
			return;
		}
		final var targets = this.annotationUtil.getAnnotationTargets((JvmAnnotationType) annotationType);
		if (targets.isEmpty()) {
			return;
		}
		final var eContainer = getContainingAnnotationTarget(annotation);
		var clazz = eContainer.getClass();
		if (eContainer instanceof XtendField && eContainer.eContainer() instanceof XtendAnnotationType) {
			clazz = XtendFunction.class;
		}
		for (final var  mapping : this.targetInfos.asMap().entrySet()) {
			if (mapping.getKey().isAssignableFrom(clazz)) {
				targets.retainAll(mapping.getValue());
				if (targets.isEmpty()) {
					error(MessageFormat.format(Messages.SARLAnnotationValidator_5,
							annotation.getAnnotationType().getSimpleName()),
							annotation, null, INSIGNIFICANT_INDEX,
							ANNOTATION_WRONG_TARGET);
				}
			}
		}
		if (isForbiddenActiveAnnotation(annotation)) {
			error(Messages.SARLAnnotationValidator_6,
					annotation,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					FORBIDDEN_REFERENCE);
		} else if (isOOActiveAnnotation(annotation) || isAOActiveAnnotation(annotation)) {
			var container = EcoreUtil2.getContainerOfType(annotation.eContainer(), XtendTypeDeclaration.class);
			while (container != null && (container.isAnonymous() || container.getName() == null)) {
				container = EcoreUtil2.getContainerOfType(container.eContainer(), XtendTypeDeclaration.class);
			}
			if (container != null) {
				if (isOOType(container)) {
					if (!isOOActiveAnnotation(annotation)) {
						error(MessageFormat.format(Messages.SARLAnnotationValidator_7, container.getName()),
								annotation,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								FORBIDDEN_REFERENCE);
					}
				} else {
					if (!isAOActiveAnnotation(annotation) || !isAOActiveAnnotationReceiver(container)) {
						error(MessageFormat.format(Messages.SARLAnnotationValidator_8, container.getName()),
								annotation,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								FORBIDDEN_REFERENCE);
					}
				}
			}
		}
	}

	private static EObject getContainingAnnotationTarget(XAnnotation annotation) {
		final EObject eContainer = annotation.eContainer();
		// skip synthetic container
		if (eContainer.eClass() == XtendPackage.Literals.XTEND_MEMBER || eContainer.eClass() == XtendPackage.Literals.XTEND_TYPE_DECLARATION) {
			return eContainer.eContainer();
		}
		return eContainer;
	}

	/** Check if there is multiple annotations. 
	 * 
	 * @param annotationTarget the annotation receiver to check.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkMultipleAnnotations(final XtendAnnotationTarget annotationTarget) {
		if (annotationTarget.getAnnotations().size() <= 1 || !isRelevantAnnotationTarget(annotationTarget)) {
			return;
		}

		final var groupByIdentifier = Multimaps.index(annotationTarget.getAnnotations(),
				input -> input.getAnnotationType().getIdentifier());

		for (final var qName : groupByIdentifier.keySet()) {
			final var sameType = groupByIdentifier.get(qName);
			if (sameType.size() > 1) {
				final var type = sameType.get(0).getAnnotationType();
				if (type instanceof JvmAnnotationType && !type.eIsProxy()
						&& !this.annotationLookup.isRepeatable((JvmAnnotationType) type)) {
					for (XAnnotation xAnnotation : sameType) {
						error(MessageFormat.format(Messages.SARLAnnotationValidator_9,
								xAnnotation.getAnnotationType().getSimpleName()),
								xAnnotation, XAnnotationsPackage.Literals.XANNOTATION__ANNOTATION_TYPE,
								INSIGNIFICANT_INDEX, ANNOTATION_MULTIPLE);
					}
				}
			}
		}
	}

}
