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

package io.sarl.lang.validation;

import static org.eclipse.xtend.core.validation.IssueCodes.FIELD_NOT_INITIALIZED;
import static org.eclipse.xtend.core.xtend.XtendPackage.Literals.XTEND_FIELD__NAME;

import java.text.MessageFormat;
import java.util.Map;

import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import com.google.inject.Singleton;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.RichString;
import org.eclipse.xtend.core.xtend.RichStringForLoop;
import org.eclipse.xtend.core.xtend.XtendAnnotationTarget;
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmAnnotationTarget;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeParameter;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.DeprecationUtil;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.IssueSeverities;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;
import org.eclipse.xtext.xbase.jvmmodel.IJvmModelAssociations;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReferenceFactory;
import org.eclipse.xtext.xbase.typesystem.references.StandardTypeReferenceOwner;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;
import org.eclipse.xtext.xbase.util.XbaseUsageCrossReferencer;
import org.eclipse.xtext.xbase.validation.ReadAndWriteTracking;
import org.eclipse.xtext.xtype.XImportDeclaration;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.subvalidators.SARLCastValidator;

/**
 * Validator for the SARL elements.
 *
 * <p>The check type may be one of:<ul>
 * <li>{@link CheckType#FAST}: is executed after a delay of 500ms after ANY editing action (type, enter, delete);</li>
 * <li>{@link CheckType#NORMAL}: is executed after a build (manual, or automatic);</li>
 * <li>{@link CheckType#EXPENSIVE}: is executed by right clicking ANYWHERE in the editor window and chooseing "Validate".</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://www.eclipse.org/Xtext/documentation/303_runtime_concepts.html#validation"
 */
@Singleton
public class SARLValidator extends AbstractSARLValidator implements ISARLValidator {

	@Inject
	private IGeneratorConfigProvider generatorConfigProvider;

	@Inject
	private SarlJvmModelAssociations associations;

	@Inject
	private IJvmModelAssociations jvmModelAssociations;

	@Inject
	private SARLCastValidator castValidator;

	@Inject
	private IProgrammaticWarningSuppressor warningSuppressor;

	@Inject
	private ReadAndWriteTracking readAndWriteTracking;

	private ValidationMessageAcceptor temporaryMessageAcceptor; 

	@Override
	public boolean isPrimitiveVoid(JvmTypeReference typeReference) {
		return super.isPrimitiveVoid(typeReference);
	}

	@Override
	public boolean isFinal(LightweightTypeReference type) {
		return super.isFinal(type);
	}

	@Override
	public boolean memberOfTypeHierarchy(LightweightTypeReference type, LightweightTypeReference potentialMember) {
		return super.memberOfTypeHierarchy(type, potentialMember);
	}

	@Override
	public CommonTypeComputationServices getServices() {
		return super.getServices();
	}

	@Override
	public LightweightTypeReference toLightweightTypeReference(JvmTypeReference typeRef) {
		return super.toLightweightTypeReference(typeRef);
	}

	@Override
	public LightweightTypeReference toLightweightTypeReference(JvmTypeReference typeRef, boolean keepUnboundWildcardInformation) {
		return super.toLightweightTypeReference(typeRef, keepUnboundWildcardInformation);
	}

	@Override
	public LightweightTypeReference getActualType(XExpression expression) {
		return super.getActualType(expression);
	}

	@Override
	public boolean isIgnored(String issueCode) {
		return super.isIgnored(issueCode);
	}

	@Override
	public boolean isIgnored(String issueCode, EObject currentObject) {
		final var severities = getIssueSeverities(getContext(), currentObject);
		return severities.isIgnored(issueCode);
	}

	@Override
	public void addIssue(String message, EObject source, String issueCode) {
		super.addIssue(message, source, issueCode);
	}
	
	@Override
	public boolean isLocallyUsed(EObject target, EObject containerToFindUsage) {
		final var result = super.isLocallyUsed(target, containerToFindUsage);
		return result;
	}

	@Override
	public boolean isTypeParameterLocallyUsedInEvent(JvmTypeParameter parameter, SarlEvent event) {
		// Type parameters are used when they are referenced in the containerToFindUsage.
		for (final var member : event.getMembers()) {
			if (member instanceof XtendField field) {
				if (field.getType() != null && field.getType().getType().getIdentifier().equals(parameter.getIdentifier())) {
					return true;
				}
				if (field.getInitialValue() != null) {
					final var usages = XbaseUsageCrossReferencer.find(parameter, field.getInitialValue());
					if (!usages.isEmpty()) {
						return true;
					}
				}
			}
		}
		return false;
	}

	@Override
	public LightweightTypeReference toLightweightTypeReference(JvmType type, EObject context) {
		final var owner = new StandardTypeReferenceOwner(getServices(), context);
		final var factory = new LightweightTypeReferenceFactory(owner, false);
		return factory.toLightweightReference(type);
	}

	@Override
	public GeneratorConfig getGeneratorConfig(EObject element) {
		var result = (GeneratorConfig) getContext().get(GeneratorConfig.class);
		if (result == null) {
			result = this.generatorConfigProvider.get(element);
			getContext().put(GeneratorConfig.class, result);
		}
		return result;
	}

	@Override
	protected void checkCast(JvmTypeReference concreteSyntax, LightweightTypeReference toType, LightweightTypeReference fromType) {
		super.checkCast(concreteSyntax, toType, fromType);
		this.castValidator.checkCast(this, concreteSyntax, toType, fromType);
	}
	
	@Override
	@Check(CheckType.NORMAL)
	public void checkCasts(XCastedExpression cast) {
		if (!this.castValidator.checkCasts(this, cast)) {
			super.checkCasts(cast);
		}
	}

	@Override
	protected boolean isValueExpectedRecursive(XExpression expr) {
		final var container = expr.eContainer();
		if (container instanceof SarlBreakExpression) {
			return false;
		}
		if (container instanceof SarlContinueExpression) {
			return false;
		}
		if (container instanceof SarlAssertExpression) {
			return true;
		}
		if (container instanceof RichString 
			|| container instanceof RichStringForLoop
			|| container instanceof XtendField) {
			return true;
		}
		return super.isValueExpectedRecursive(expr);
	}

	@Override
	public IssueSeverities getIssueSeverities(Map<Object, Object> context, EObject eObject) {
		final var severities = super.getIssueSeverities(context, eObject);
		return this.warningSuppressor.getIssueSeverities(context, eObject, severities);
	}

	@Override
	protected boolean isInitialized(JvmField input) {
		if (super.isInitialized(input)) {
			return true;
		}
		// Check initialization into a static constructor.
		final var sarlField = (XtendField) this.associations.getPrimarySourceElement(input);
		if (sarlField == null) {
			return false;
		}
		final var declaringType = sarlField.getDeclaringType();
		if (declaringType == null) {
			return false;
		}
		for (final var staticConstructor : Iterables.filter(Iterables.filter(
				declaringType.getMembers(), XtendConstructor.class), it -> it.isStatic())) {
			if (staticConstructor.getExpression() != null) {
				for (final var assign : EcoreUtil2.getAllContentsOfType(staticConstructor.getExpression(), XAssignment.class)) {
					if (assign.isStatic() && Strings.equal(input.getIdentifier(), assign.getFeature().getIdentifier())) {
						// Mark the field as initialized in order to be faster during the next initialization test.
						this.readAndWriteTracking.markInitialized(input, null);
						return true;
					}
				}
			}
		}
		return false;
	}

	@Override
	protected void checkAssignment(XExpression expression, EStructuralFeature feature, boolean simpleAssignment) {
		if (simpleAssignment && expression instanceof XAbstractFeatureCall cvalue) {
			final var assignmentFeature = cvalue.getFeature();
			if (assignmentFeature instanceof JvmField field) {
				if (!field.isFinal()) {
					return;
				}
				final var container = getLogicalContainerProvider().getNearestLogicalContainer(expression);
				if (container != null && container instanceof JvmOperation operation) {
					if (operation.isStatic() && field.getDeclaringType() == operation.getDeclaringType()
							&& Utils.isStaticConstructorName(operation.getSimpleName())) {
						return;
					}
				}
			}
		}
		super.checkAssignment(expression, feature, simpleAssignment);
	}

	
	@Override
	public void checkFinalFieldInitialization(JvmGenericType type, ValidationMessageAcceptor acceptor) {
		assert type != null;
		this.temporaryMessageAcceptor = acceptor;
		super.checkFinalFieldInitialization(type);
	}

	private ValidationMessageAcceptor getTemporaryMessageAcceptor() {
		var acceptor = this.temporaryMessageAcceptor;
		if (acceptor == null) {
			acceptor = getMessageAcceptor();
		}
		return acceptor;
	}
	
	@Override
	protected void reportUninitializedField(JvmField field) {
		final var acceptor = getTemporaryMessageAcceptor();
		final var element = this.associations.getPrimarySourceElement(field);
		if (element instanceof XtendField) {
			acceptor.acceptError(MessageFormat.format(Messages.SARLValidator_1, field.getSimpleName()),
					element, XTEND_FIELD__NAME, ValidationMessageAcceptor.INSIGNIFICANT_INDEX, FIELD_NOT_INITIALIZED);
		} else {
			acceptor.acceptError(MessageFormat.format(Messages.SARLValidator_2, field.getSimpleName()),
					element, null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX, FIELD_NOT_INITIALIZED);
		}
	}
	
	@Override
	protected void reportUninitializedField(JvmField field, JvmConstructor constructor) {
		final var acceptor = getTemporaryMessageAcceptor();
		final var sourceElement = this.associations.getPrimarySourceElement(constructor);
		if (sourceElement != null) {
			if (this.associations.getXtendField(field) != null) {
				acceptor.acceptError(MessageFormat.format(Messages.SARLValidator_1, field.getSimpleName()),
						sourceElement, null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX, FIELD_NOT_INITIALIZED);
			} else {
				acceptor.acceptError(MessageFormat.format(Messages.SARLValidator_2, field.getSimpleName()),
						sourceElement, null, ValidationMessageAcceptor.INSIGNIFICANT_INDEX, FIELD_NOT_INITIALIZED);
			}
		}
	}
	
	@Override
	protected boolean isLocalClassSemantics(EObject object) {
		return super.isLocalClassSemantics(object) || (object instanceof XtendMember && !(object instanceof AnonymousClass));
	}
	
	@Override
	@Check(CheckType.NORMAL)
	public void checkDeprecated(XImportDeclaration decl) {
		final var file = EcoreUtil2.getContainerOfType(decl, XtendFile.class);
		if (file != null) {
			for (final var t : file.getXtendTypes()) {
				for (final var e : this.jvmModelAssociations.getJvmElements(t)) {
					if  (e instanceof JvmAnnotationTarget cvalue) {
						if (DeprecationUtil.isDeprecated(cvalue)) {
							return;
						}
					}
				}
				
				if (hasAnnotation(t, Deprecated.class)) {
					return;
				}
			}
		}
		super.checkDeprecated(decl);
	}

	private static boolean hasAnnotation(XtendAnnotationTarget source, Class<?> class1) {
		for (final var anno : source.getAnnotations()) {
			if (anno != null && anno.getAnnotationType() != null && class1.getName().equals(anno.getAnnotationType().getIdentifier())) {
				return true;
			}
 		}
		return false;
	}

}
