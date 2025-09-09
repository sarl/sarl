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

import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE;
import static io.sarl.lang.validation.IssueCodes.INVALID_OCCURRENCE_READONLY_USE;
import static io.sarl.lang.validation.IssueCodes.UNREACHABLE_BEHAVIOR_UNIT;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_INNER_EXPRESSION;
import static org.eclipse.xtext.xbase.validation.IssueCodes.TYPE_BOUNDS_MISMATCH;

import java.text.MessageFormat;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBinaryOperation;
import org.eclipse.xtext.xbase.XBooleanLiteral;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XPostfixOperation;
import org.eclipse.xtext.xbase.XUnaryOperation;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.core.util.OutParameter;
import com.google.inject.Inject;

import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.typesystem.SARLOperationHelper;
import io.sarl.lang.util.Utils;

/**
 * A specialized validator to deal with behavior units and event handlers.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.14
 */
public class SARLBehaviorUnitValidator extends AbstractSARLSubValidator {

	@Inject
	private SARLOperationHelper operationHelper;

	/** Check the type of the behavior unit's guard.
	 *
	 * @param behaviorUnit the behavior unit.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkBehaviorUnitGuardType(SarlBehaviorUnit behaviorUnit) {
		final var guard = behaviorUnit.getGuard();
		if (guard != null) {
			if (this.operationHelper.hasSideEffects(null, guard)) {
				error(Messages.SARLBehaviorUnitValidator_1,
						guard,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INVALID_INNER_EXPRESSION);
				return;
			}
			if (guard instanceof XBooleanLiteral booleanLiteral) {
				if (booleanLiteral.isIsTrue()) {
					if (!isIgnored(DISCOURAGED_BOOLEAN_EXPRESSION)) {
						addIssue(Messages.SARLBehaviorUnitValidator_2,
								booleanLiteral,
								null,
								ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
								DISCOURAGED_BOOLEAN_EXPRESSION);
					}
				} else if (!isIgnored(UNREACHABLE_BEHAVIOR_UNIT)) {
					addIssue(Messages.SARLBehaviorUnitValidator_3,
							behaviorUnit,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							UNREACHABLE_BEHAVIOR_UNIT,
							behaviorUnit.getName().getSimpleName());
				}
				return;
			}

			final var fromType = getActualType(guard);
			if (!fromType.isAssignableFrom(Boolean.TYPE)) {
				error(MessageFormat.format(
						Messages.SARLBehaviorUnitValidator_4,
						getNameOfTypes(fromType), boolean.class.getName()),
						guard,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INCOMPATIBLE_TYPES);
			}
		}
	}

	/** Check for usage of the event functions in the behavior units.
	 *
	 * @param unit the unit to analyze.
	 */
	@Check(CheckType.EXPENSIVE)
	public void checkUnmodifiableEventAccess(SarlBehaviorUnit unit) {
		final var enable = !isIgnored(DISCOURAGED_OCCURRENCE_READONLY_USE);
		final var root = unit.getExpression();
		final var occurrenceKw = getGrammarAccess().getOccurrenceKeyword();
		for (final var child : EcoreUtil2.getAllContentsOfType(root, XFeatureCall.class)) {
			if (occurrenceKw.equals(child.getFeature().getIdentifier())) {
				checkUnmodifiableFeatureAccess(enable, child, occurrenceKw);
			}
		}
	}

	private void checkUnmodifiableFeatureAccess(boolean enableWarning, EObject readOnlyExpression, String keywordName) {
		final var container = new OutParameter<EObject>();
		final var directContainerChild = new OutParameter<EObject>();

		final var failure = new OutParameter<>(Boolean.FALSE);
		final var sequence = getRootOfMemberFeatureCallSequence(readOnlyExpression, null, it -> {
			// Function call: if one of the functions called on the read-only keyword is not pure => WARNING
			if (enableWarning && getExpressionHelper().hasSideEffects(it)) {
				addIssue(MessageFormat.format(Messages.SARLBehaviorUnitValidator_5, keywordName),
						it,
						DISCOURAGED_OCCURRENCE_READONLY_USE);
				failure.set(Boolean.TRUE);
			}
		});
		if (failure.get().booleanValue()) {
			return;
		}

		final var expression = sequence == null ? readOnlyExpression : sequence;

		if (Utils.getContainerOfType(expression, container, directContainerChild,
				XAssignment.class, XVariableDeclaration.class)) {
			if (container.get() instanceof XAssignment assignment) {
				// Assignment: occurrence in left side => ERROR
				final var leftMember = directContainerChild.get();
				if (expression == leftMember && leftMember == assignment.getActualReceiver()) {
					error(MessageFormat.format(Messages.SARLBehaviorUnitValidator_6, keywordName),
							readOnlyExpression,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							INVALID_OCCURRENCE_READONLY_USE);
					return;
				}
			} else if (enableWarning && container.get() instanceof XVariableDeclaration) {
				final var directContainerChildInstance = directContainerChild.get();

				// We have validated that the chain of calls on the read-only variable does not contain
				// a call to an impure feature (see the first test in this function).
				// If the last called feature is of a read-only type, then it is safe to use it anywhere.
				if (isImmutable(directContainerChildInstance)) {
					return;
				}

				final var declaration = (XVariableDeclaration) container.get();
				if (directContainerChild.get() == declaration.getRight()) {
					// Inside the initial value of a variable.
					// If the variable has a primitive type => No problem.
					// If the keyword is used in member feature calls => Warning
					final var variableType = getActualType(sequence);
					if (!getImmutableTypeValidator().isImmutable(variableType)) {
						if (expression == null || expression == declaration.getRight()) {
							addIssue(MessageFormat.format(Messages.SARLBehaviorUnitValidator_7, keywordName),
									sequence,
									DISCOURAGED_OCCURRENCE_READONLY_USE);
							return;
						}
					}
				}
			}
		}

		if (enableWarning) {
			if (Utils.getContainerOfType(expression, container, directContainerChild,
					XAbstractFeatureCall.class, XConstructorCall.class)) {
				final var directExpressionContainer = container.get();
				final var directContainerChildInstance = directContainerChild.get();

				// We have validated that the chain of calls on the read-only variable does not contain
				// a call to an impure feature (see the first test in this function).
				// If the last called feature is of a read-only type, then it is safe to use it anywhere.
				if (isImmutable(directContainerChildInstance)) {
					return;
				}

				// Skip operators because they have no side effects
				if (directExpressionContainer instanceof XBinaryOperation
						|| directExpressionContainer instanceof XUnaryOperation) {
					return;
				}
				// Side effect Operator: occurrence in one of the operands => WARNING
				if (directExpressionContainer instanceof XPostfixOperation) {
					error(MessageFormat.format(Messages.SARLBehaviorUnitValidator_8, keywordName),
							readOnlyExpression,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							INVALID_OCCURRENCE_READONLY_USE);
					return;
				}
				// Function argument:
				// In all the other cases, the value is passed directly and could be accessed from
				// within the called function, if this function has side effect => WARNING
				final List<XExpression> arguments;
				final List<LightweightTypeReference> parameters;
				final boolean hasSideEffects;
				final boolean isVariadic;
				if (directExpressionContainer instanceof XConstructorCall cons) {
					arguments = cons.getArguments();
					final var constructor = cons.getConstructor();
					parameters = getParamTypeReferences(constructor, false, true);
					hasSideEffects = false;
					isVariadic = constructor.isVarArgs();
				} else {
					final var call = (XAbstractFeatureCall) directExpressionContainer;
					if (call.getFeature() instanceof JvmOperation operation) {
						arguments = call.getActualArguments();
						parameters = getParamTypeReferences(operation, false, true);
						hasSideEffects = getExpressionHelper().hasSideEffects(call);
						isVariadic = operation.isVarArgs();
					} else {
						arguments = null;
						parameters = null;
						hasSideEffects = false;
						isVariadic = false;
					}
				}
				if (arguments != null && hasSideEffects) {
					assert parameters != null;
					final var index = arguments.indexOf(directContainerChildInstance);
					if (index >= 0 && !parameters.isEmpty()) {
						final boolean isPrimitive;
						final var endIndex = parameters.size() - 1;
						if (index < endIndex || (!isVariadic && index == endIndex)) {
							isPrimitive = getImmutableTypeValidator().isImmutable(parameters.get(index));
						} else if (isVariadic && index == endIndex) {
							// Assume argument for the variadic parameter.
							var parameter = parameters.get(endIndex);
							assert parameter.isArray();
							parameter = parameter.getComponentType().getWrapperTypeIfPrimitive();
							isPrimitive = getImmutableTypeValidator().isImmutable(parameter);
						} else {
							// Problem in the calling syntax: invalid number of arguments.
							// Avoid to output the warning.
							isPrimitive = true;
						}
						if (!isPrimitive) {
							addIssue(MessageFormat.format(Messages.SARLBehaviorUnitValidator_9, keywordName),
									sequence,
									DISCOURAGED_OCCURRENCE_READONLY_USE);
							return;
						}
					}
				}
			}
		}
	}

	/** Check if the parameter of the behavior unit is an event.
	 *
	 * @param behaviorUnit the behavior unit to test.
	 */
	@Check(CheckType.FAST)
	public void checkBehaviorUnitEventType(SarlBehaviorUnit behaviorUnit) {
		final var event = behaviorUnit.getName();
		final var ref = toLightweightTypeReference(event);
		if (ref == null || !getInheritanceHelper().isSarlEvent(ref)) {
			error(MessageFormat.format(
					Messages.SARLBehaviorUnitValidator_11,
					event.getQualifiedName(),
					Messages.SARLBehaviorUnitValidator_10,
					getGrammarAccess().getOnKeyword()),
					event,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					TYPE_BOUNDS_MISMATCH);
		}
	}

}
