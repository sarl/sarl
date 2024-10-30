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

import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_MEMBER_NAME;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_IDENTIFIER;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Iterables;
import com.google.common.collect.Iterators;
import com.google.inject.Inject;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendAnnotationType;
import org.eclipse.xtend.core.xtend.XtendClass;
import org.eclipse.xtend.core.xtend.XtendEnum;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendInterface;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.validation.AbstractDeclarativeValidator;
import org.eclipse.xtext.validation.EValidatorRegistrar;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XNullLiteral;
import org.eclipse.xtext.xbase.compiler.JavaKeywords;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.computation.SynonymTypesProvider;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.util.XExpressionHelper;
import org.eclipse.xtext.xbase.util.XbaseUsageCrossReferencer;
import org.eclipse.xtext.xbase.validation.FeatureNameValidator;

import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.jvmmodel.SARLReadAndWriteTracking;
import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlArtifact;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlProtocol;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.sarl.SarlSpace;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.typesystem.IImmutableTypeValidator;
import io.sarl.lang.typesystem.InheritanceHelper;
import io.sarl.lang.typesystem.SARLOperationHelper;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IProgrammaticWarningSuppressor;
import io.sarl.lang.validation.SARLFeatureNameValidator;

/**
 * A abstract implementation of {@link AbstractDeclarativeValidator} that provides utility functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public abstract class AbstractSARLSubValidator extends AbstractDeclarativeValidator {

	@Inject
	private SarlJvmModelAssociations associations;

	@Inject
	private SARLReadAndWriteTracking readAndWriteTracking;

	@Inject
	private SARLGrammarKeywordAccess grammarAccess;

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private IBatchTypeResolver typeResolver;

	@Inject
	private XExpressionHelper expressionHelper;

	@Inject
	private IImmutableTypeValidator immutableTypeValidator;

	@Inject
	private FeatureNameValidator featureNameValidator;

	@Inject
	private IActionPrototypeProvider sarlActionSignatures;

	@Inject
	private InheritanceHelper inheritanceHelper;

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private ILogicalContainerProvider logicalContainerProvider;
	
	@Inject
	private SARLOperationHelper operationHelper;

	@Inject 
	private JavaKeywords javaUtils;

	@Inject
	private IProgrammaticWarningSuppressor warningSuppressor;

	private List<String> visibilityModifiers;

	@Override
	public IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject eObject) {
		final var severities = super.getIssueSeverities(context, eObject);
		return this.warningSuppressor.getIssueSeverities(context, eObject, severities);
	}

	/** Replies all the visibility modifiers.
	 *
	 * @return the visibility modifiers.
	 */
	protected List<String> getVisibilityModifiers() {
		if (this.visibilityModifiers == null) {
			this.visibilityModifiers = new ArrayList<>();
			this.visibilityModifiers.add(this.grammarAccess.getPublicKeyword());
			this.visibilityModifiers.add(this.grammarAccess.getProtectedKeyword());
			this.visibilityModifiers.add(this.grammarAccess.getPackageKeyword());
			this.visibilityModifiers.add(this.grammarAccess.getPrivateKeyword());
		}
		return this.visibilityModifiers;
	}

	/** Replies the tools for managing operations.
	 *
	 * @return the tool.
	 */
	protected SARLOperationHelper getOperationHelper() {
		return this.operationHelper;
	}

	/** Replies the logical container provider.
	 *
	 * @return the tool.
	 */
	protected ILogicalContainerProvider getLogicalContainerProvider() {
		return this.logicalContainerProvider;
	}

	/** Replies the tools for using type references.
	 *
	 * @return the tool.
	 */
	protected TypeReferences getTypeReferences() {
		return this.typeReferences;
	}

	/** Replies the tools for checking the inheritance of types.
	 *
	 * @return the tool.
	 */
	protected InheritanceHelper getInheritanceHelper() {
		return this.inheritanceHelper;
	}

	/** Replies the tools for managing the SARL action signatures.
	 *
	 * @return the tool.
	 */
	protected IActionPrototypeProvider getSarlActionSignatures() {
		return this.sarlActionSignatures;
	}

	/** Replies the validator of feture names.
	 *
	 * @return the validator.
	 */
	protected FeatureNameValidator getFeatureNameValidator() {
		return this.featureNameValidator;
	}

	/** Replies the validator for immutable types.
	 *
	 * @return the validator.
	 */
	protected IImmutableTypeValidator getImmutableTypeValidator() {
		return this.immutableTypeValidator;
	}

	/** Replies the expression helper.
	 *
	 * @return the helper.
	 */
	protected XExpressionHelper getExpressionHelper() {
		return this.expressionHelper;
	}

	/** Replies the type computation services.
	 *
	 * @return the associations.
	 */
	protected CommonTypeComputationServices getServices() {
		return this.services;
	}

	/** Replies the associations between SARL and JVM models.
	 *
	 * @return the associations.
	 */
	protected SarlJvmModelAssociations getAssociations() {
		return this.associations;
	}
	
	/** Replies the read-write tracker.
	 *
	 * @return the tracker.
	 */
	protected SARLReadAndWriteTracking getReadAndWriteTracking() {
		return this.readAndWriteTracking;
	}

	/** Replies the accessor to the grammar.
	 *
	 * @return the accessor.
	 */
	protected SARLGrammarKeywordAccess getGrammarAccess() {
		return this.grammarAccess;
	}
	
	@Override
	public void register(EValidatorRegistrar registrar) {
		// library validator is not registered for a specific language
	}

	/** Replies the outer-most type of the given member.
	 *
	 * @return the container of {@code XtendTypeDeclaration} type if it exists, or the direct container.
	 */
	@SuppressWarnings("static-method")
	protected final EObject getOutermostType(XtendMember member) {
		var result = EcoreUtil2.getContainerOfType(member, XtendTypeDeclaration.class);
		if (result == null) {
			return member.eContainer();
		}
		while (!(result.eContainer() instanceof SarlScript)) {
			final var next = EcoreUtil2.getContainerOfType(result.eContainer(), XtendTypeDeclaration.class);
			if (next == null) {
				return result;
			}
			result = next;
		}
		return result;
	}

	/** Replies if the given object is locally assigned.
	 *
	 * <p>An object is locally assigned when it is the left operand of an assignment operation.
	 *
	 * @param target the object to test.
	 * @param containerToFindUsage the container in which the usages should be find.
	 * @return {@code true} if the given object is assigned.
	 * @since 0.7
	 */
	protected boolean isLocallyAssigned(EObject target, EObject containerToFindUsage) {
		if (getReadAndWriteTracking().isAssigned(target)) {
			return true;
		}
		final var usages = XbaseUsageCrossReferencer.find(target, containerToFindUsage);
		// field are assigned when they are not used as the left operand of an assignment operator.
		for (final var usage : usages) {
			final var object = usage.getEObject();
			if (object instanceof XAssignment assignment) {
				if (assignment.getFeature() == target) {
					// Mark the field as assigned in order to be faster during the next assignment test.
					getReadAndWriteTracking().markAssignmentAccess(target);
					return true;
				}
			}
		}
		return false;
	}

	/** Replies if the given issue is ignored for the given object.
	 *
	 * @param issueCode the code if the issue.
	 * @param currentObject the current object.
	 * @return {@code true} if the issue is ignored.
	 * @see #isIgnored(String)
	 */
	protected boolean isIgnored(String issueCode, EObject currentObject) {
		final var severities = getIssueSeverities(getContext(), currentObject);
		return severities.isIgnored(issueCode);
	}

	/** Replies if the given member has a valid EMF name.
	 *
	 * @param member the member to text.
	 * @return
	 */
	protected boolean doCheckValidMemberName(XtendMember member) {
		final var nameAttribute = member.eClass().getEStructuralFeature("name"); //$NON-NLS-1$
		if (nameAttribute != null) {
			final var name = (String) member.eGet(nameAttribute);
			if (name != null) {
				if (name.equals(getGrammarAccess().getThisKeyword())
					|| name.equals(getGrammarAccess().getItKeyword())
					|| name.equals(getGrammarAccess().getOccurrenceKeyword())) { 
					error(MessageFormat.format(Messages.AbstractSARLSubValidator_1, name), nameAttribute, INVALID_MEMBER_NAME);
					return false;
				}
				if (SarlUtils.isHiddenMember(name)) { 
					error(MessageFormat.format(Messages.AbstractSARLSubValidator_2, SarlUtils.HIDDEN_MEMBER_CHARACTER), nameAttribute, INVALID_MEMBER_NAME);
					return false;
				}
			}
		}
		return true;
	}

	/** Replies the canonical name for the given type.
	 *
	 * @param typeRef the type.
	 * @return the canonical name.
	 */
	protected static String canonicalName(LightweightTypeReference typeRef) {
		return (typeRef == null) ? Messages.AbstractSARLSubValidator_4 : typeRef.getHumanReadableName();
	}

	/** Replies the name for the give type.
	 * 
	 * @param expectedType the type.
	 * @return the name.
	 */
	protected String getNameOfTypes(LightweightTypeReference expectedType) {
		final StringBuilder result = new StringBuilder(canonicalName(expectedType));
		getServices().getSynonymTypesProvider().collectSynonymTypes(expectedType, new SynonymTypesProvider.Acceptor() {
			@Override
			protected boolean accept(LightweightTypeReference synonym, int flags) {
				result.append(Messages.AbstractSARLSubValidator_3).append(canonicalName(synonym));
				return true;
			}
		});
		return result.toString();
	}

	/** Replies the type of the given element.
	 *
	 * @param context the type context.
	 * @param element the element for which the type is computed.
	 * @return the type of the element.
	 */
	protected LightweightTypeReference getActualType(EObject context, JvmIdentifiableElement element) {
		return this.typeResolver.resolveTypes(context).getActualType(element);
	}

	/** Replies the type of the given expression.
	 *
	 * @param expression the expression to evaluate.
	 * @return the type of the expression.
	 */
	protected LightweightTypeReference getActualType(XExpression expression) {
		return this.typeResolver.resolveTypes(expression).getActualType(expression);
	}
	
	/** Replies the type of the given identifiable element.
	 *
	 * @param identifiable the identifiable element to evaluate.
	 * @return the type of the element.
	 */
	protected LightweightTypeReference getActualType(JvmIdentifiableElement identifiable) {
		return this.typeResolver.resolveTypes(identifiable).getActualType(identifiable);
	}

	/** Replies the expected type of the given expression.
	 *
	 * @param identifiable the expression to evaluate.
	 * @return the expected type of the expressiont.
	 */
	protected LightweightTypeReference getExpectedType(XExpression expression) {
		return this.typeResolver.resolveTypes(expression).getExpectedType(expression);
	}

	/** Replies the member feature call that is the root of a sequence of member feature calls.
	 *
	 * <p>While the current feature call is the actual receiver of a member feature call, and not an argument,
	 * the sequence is still active. Otherwise, the sequence is stopped.
	 *
	 * @param leaf the expression at the leaf of the feature call.
	 * @param container the top most container that cannot be part of the sequence. Could be {@code null}.
	 * @param feedback the function that is invoked on each discovered member feature call within the sequence. Could be {@code null}.
	 * @return the root of a member feature call sequence.
	 */
	protected static XMemberFeatureCall getRootOfMemberFeatureCallSequence(EObject leaf, EObject container,
			Procedure1<XMemberFeatureCall> feedback) {
		var call = leaf;
		var obj = EcoreUtil2.getContainerOfType(leaf.eContainer(), XExpression.class);
		while (obj != null && (container == null || obj != container)) {
			if (obj instanceof XMemberFeatureCall fcall) {
				final var previous = call;
				call = fcall;
				if (fcall.getActualReceiver() == previous) {
					// Sequence of calls, with the '.' char.
					if (feedback != null) {
						feedback.apply(fcall);
					}
					obj = EcoreUtil2.getContainerOfType(call.eContainer(), XExpression.class);
				} else if (fcall.getActualArguments().contains(previous)) {
					// The sequence is an argument of a function call.
					call = previous;
					obj = null;
				} else {
					obj = null;
				}
			} else {
				obj = null;
			}
		}
		return call instanceof XMemberFeatureCall cvalue ? cvalue : null;
	}

	/** Replies if the given element is an immutable element.
	 *
	 * @param element the element to test.
	 * @return {@code true} if the element is immutable.
	 */
	protected boolean isImmutable(EObject element) {
		LightweightTypeReference calledFeatureType = null;
		if (element instanceof XExpression cvalue) {
			calledFeatureType = getActualType(cvalue);
		} else if (element instanceof JvmIdentifiableElement cvalue) {
			calledFeatureType = getActualType(cvalue);
		}
		if (calledFeatureType != null) {
			return getImmutableTypeValidator().isImmutable(calledFeatureType);
		}
		return false;
	}

	/** Replies the lightweight type reference that is corresponding to the given type reference.
	 *
	 * @param typeRef the source type reference.
	 * @return the lightweight type reference.
	 */
	protected LightweightTypeReference toLightweightTypeReference(JvmTypeReference typeRef) {
		return toLightweightTypeReference(typeRef, false);
	}
	
	/** Replies the lightweight type reference that is corresponding to the given type reference.
	 *
	 * @param typeRef the source type reference.
	 * @param keepUnboundWildcardInformation indicates if the wildcard information are kept in the lightweight reference.
	 * @return the lightweight type reference.
	 */
	protected LightweightTypeReference toLightweightTypeReference(JvmTypeReference typeRef, boolean keepUnboundWildcardInformation) {
		final var owner = new StandardTypeReferenceOwner(getServices(), typeRef);
		final var factory = new LightweightTypeReferenceFactory(owner, keepUnboundWildcardInformation);
		final var reference = factory.toLightweightReference(typeRef);
		return reference;
	}

	/** Retrieve the types of the formal parameters of the given JVM executable object.
	 *
	 * @param jvmExecutable the JVM executable.
	 * @param wrapFromPrimitives indicates if the primitive types must be wrapped to object type equivalent.
	 * @param wrapToPrimitives indicates if the object types must be wrapped to primitive type equivalent.
	 * @return the list of types.
	 * @since 0.8
	 * @see #getParamTypes(JvmOperation, boolean)
	 */
	protected List<LightweightTypeReference> getParamTypeReferences(JvmExecutable jvmExecutable,
			boolean wrapFromPrimitives, boolean wrapToPrimitives) {
		assert (wrapFromPrimitives && !wrapToPrimitives) || (wrapToPrimitives && !wrapFromPrimitives);
		final var types = new ArrayList<LightweightTypeReference>();
		for (final var parameter : jvmExecutable.getParameters()) {
			var typeReference = toLightweightTypeReference(parameter.getParameterType());
			if (wrapFromPrimitives) {
				typeReference = typeReference.getWrapperTypeIfPrimitive();
			} else if (wrapToPrimitives) {
				typeReference = typeReference.getPrimitiveIfWrapperType();
			}
			types.add(typeReference);
		}
		return types;
	}

	/** This functions checks of the given name is disallowed.
	 * The {@link SARLFeatureNameValidator} provides the basic conditions.
	 * It is not detecting the implicit lambda's parameter names. This specific
	 * function does.
	 *
	 * @param name the name to test.
	 * @return {@code true} if the name is really disallowed.
	 */
	protected boolean isReallyDisallowedName(QualifiedName name) {
		if (getFeatureNameValidator().isDisallowedName(name)) {
			return true;
		}
		final var base = name.getLastSegment();
		if (SarlUtils.isHiddenMember(base) && Utils.isImplicitLambdaParameterName(base)) {
			return true;
		}
		return false;
	}

	/** Replies the label for the given type that is composed by the type's label and the type's name.
	 *
	 * @param type the type to analyse.
	 * @return the label for the type.
	 * @since 0.14
	 */
	protected String typeName(XtendTypeDeclaration declaration) {
		if (declaration != null) {
			final var jvmObject = getServices().getJvmModelAssociations().getPrimaryJvmElement(declaration);
			if (jvmObject instanceof JvmType jvmType) {
				final var name = jvmType.getSimpleName();
				final var fullLabel = new StringBuilder();
				if (declaration instanceof SarlAgent) {
					fullLabel.append(getGrammarAccess().getAgentKeyword());
				} else if (declaration instanceof SarlBehavior) {
					fullLabel.append(getGrammarAccess().getBehaviorKeyword());
				} else if (declaration instanceof SarlCapacity) {
					fullLabel.append(getGrammarAccess().getCapacityKeyword());
				} else if (declaration instanceof SarlEvent) {
					fullLabel.append(getGrammarAccess().getEventKeyword());
				} else if (declaration instanceof SarlSkill) {
					fullLabel.append(getGrammarAccess().getSkillKeyword());
				} else if (declaration instanceof XtendClass) {
					fullLabel.append(getGrammarAccess().getClassKeyword());
				} else if (declaration instanceof XtendInterface) {
					fullLabel.append(getGrammarAccess().getInterfaceKeyword());
				} else if (declaration instanceof XtendEnum) {
					fullLabel.append(getGrammarAccess().getEnumKeyword());
				} else if (declaration instanceof XtendAnnotationType) {
					fullLabel.append(getGrammarAccess().getAnnotationKeyword());
				} else if (declaration instanceof SarlSpace) {
					fullLabel.append(getGrammarAccess().getSpaceKeyword());
				} else if (declaration instanceof SarlArtifact) {
					fullLabel.append(getGrammarAccess().getArtifactKeyword());
				} else if (declaration instanceof SarlProtocol) {
					fullLabel.append(getGrammarAccess().getProtocolKeyword());
				} else {
					throw new IllegalStateException();
				}
				fullLabel.append(' ').append(name);
				return fullLabel.toString();
			}
		}
		return null;
	}

	/** Replies the label for the given field.
	 *
	 * @param field the field to analyse.
	 * @return the label for the field.
	 * @since 0.14
	 */
	protected String memberName(XtendField field) {
		String memberName = null;
		if (field != null) {
			// Determine the member name of an extension field where the field name is not explicitly set.
			if (field.isExtension() && field.getName() == null) {
				final var jvmField = getAssociations().getJvmField(field);
				if (jvmField != null) {
					final var type = getActualType(jvmField);
					if (type != null) {
						memberName = MessageFormat.format(Messages.AbstractSARLSubValidator_6, type.getHumanReadableName());
					}
				}
			}
			if (memberName == null) {
				memberName = MessageFormat.format(Messages.AbstractSARLSubValidator_5, field.getName());
			}
		}
		return memberName;
	}

	/** Replies the label for the given function.
	 *
	 * @param function the type to function.
	 * @return the label for the field.
	 * @since 0.14
	 */
	protected String memberName(XtendFunction function) {
		if (function != null) {
			final var container = function.getDeclaringType();
			if (isAOContainer(container)) {
				return MessageFormat.format(Messages.AbstractSARLSubValidator_7, function.getName());
			}
			return MessageFormat.format(Messages.AbstractSARLSubValidator_8, function.getName());
		}
		return null;
	}

	/** Replies the canonical name of the given object.
	 *
	 * @param object the object.
	 * @return the canonical name or {@code null} if it cannot be computed.
	 */
	protected String canonicalName(EObject object) {
		if (object instanceof JvmIdentifiableElement cvalue) {
			return cvalue.getQualifiedName();
		}
		final var jvmElement = this.associations.getPrimaryJvmElement(object);
		if (jvmElement instanceof JvmIdentifiableElement cvalue) {
			return cvalue.getQualifiedName();
		}
		return null;
	}

	/** Replies if the given type is an agent-oriented type that could receives a constructor declaration.
	 *
	 * @param type is the type to test.
	 * @return {@code true} if the given type could contain a constructor declaration.
	 * @since 0.12
	 */
	@SuppressWarnings("static-method")
	protected boolean isAOConstructorContainer(XtendTypeDeclaration type) {
		return type instanceof SarlAgent
				|| type instanceof SarlBehavior
				|| type instanceof SarlSkill
				|| type instanceof SarlEvent;
	}

	/** Replies if the given type is an agent-oriented container type.
	 *
	 * @param type is the type to test.
	 * @return {@code true} if the given type could contain a AOP declaration.
	 * @since 0.12
	 */
	@SuppressWarnings("static-method")
	protected boolean isAOContainer(XtendTypeDeclaration type) {
		return type instanceof SarlAgent
				|| type instanceof SarlBehavior
				|| type instanceof SarlCapacity
				|| type instanceof SarlSkill
				|| type instanceof SarlEvent
				|| type instanceof SarlSpace;
	}

	/** Replies if the given expression has no specific type.
	 *
	 * @param expression the expression to test
	 * @return {@code true} if the given {@code expression} cannot have a specific type.
	 */
	@SuppressWarnings("static-method")
	protected boolean isTypeFreeExpression(XExpression expression) {
		return expression instanceof XNullLiteral;
	}

	/** Replies all the feature calls in the given expression.
	 *
	 * @param expr the expression to analyze.
	 * @return the iterator on the feature calls.
	 */
	@SuppressWarnings("static-method")
	protected Iterator<? extends XAbstractFeatureCall> getAllFeatureCalls(XExpression expr) {
		final Iterator<? extends XAbstractFeatureCall> iter;
		if (expr instanceof XAbstractFeatureCall) {
			final var iter2 = Iterators.filter(expr.eAllContents(), XAbstractFeatureCall.class);
			final var iter3 = Iterators.singletonIterator((XAbstractFeatureCall) expr);
			if (iter2.hasNext()) {
				iter = Iterators.concat(iter3, iter2);
			} else {
				iter = iter3;
			}
		} else {
			iter = Iterators.filter(expr.eAllContents(), XAbstractFeatureCall.class);
		}
		return iter;
	}

	/** Replies the name of declarator for the given member.
	 *
	 * @param feature the member for which the declarated should be determined.
	 * @return the name of the declarator.
	 */
	protected String getDeclaratorName(JvmFeature feature) {
		if (feature != null) {
			final var declarator = feature.getDeclaringType();
			if (declarator.isLocal()) {
				return new StringBuilder()
						.append(getGrammarAccess().getNewKeyword())
						.append(Iterables.getLast(declarator.getSuperTypes()).getType().getSimpleName())
						.toString();
			}
			return declarator.getSimpleName();
		}
		return ""; //$NON-NLS-1$
	}

	/** Check if the attribute for the given object is not a Java keyword.
	 * 
	 * @param obj the object.
	 * @param attribute the attribute.
	 */
	protected void checkNoJavaKeyword(EObject obj, EAttribute attribute) {
		final var name = obj.eGet(attribute);
		if (name != null && this.javaUtils.isJavaKeyword(name.toString())) {
			error(MessageFormat.format(Messages.AbstractSARLSubValidator_9, name),
					obj, attribute, -1, INVALID_IDENTIFIER);
		}
	}

}
