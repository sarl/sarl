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

import static io.sarl.lang.validation.IssueCodes.AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_LOOP_BREAKING_KEYWORD_USE;
import static io.sarl.lang.validation.IssueCodes.INVALID_USE_OF_LOOP_BREAKING_KEYWORD;
import static org.eclipse.xtend.core.validation.IssueCodes.INVALID_OPERATOR_SIGNATURE;
import static org.eclipse.xtend.core.validation.IssueCodes.LEFT_HAND_SIDE_MUST_BE_VARIABLE;
import static org.eclipse.xtend.core.validation.IssueCodes.TERNARY_EXPRESSION_NOT_ALLOWED;
import static org.eclipse.xtext.xbase.validation.IssueCodes.DISCOURAGED_REFERENCE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.INCOMPATIBLE_TYPES;

import java.text.MessageFormat;

import com.google.inject.Inject;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.validation.Check;
import org.eclipse.xtext.validation.CheckType;
import org.eclipse.xtext.validation.ValidationMessageAcceptor;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XAbstractWhileExpression;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBasicForLoopExpression;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XForLoopExpression;
import org.eclipse.xtext.xbase.XIfExpression;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XNumberLiteral;
import org.eclipse.xtext.xbase.XUnaryOperation;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.scoping.batch.IFeatureNames;
import org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping;

import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.typesystem.SARLExpressionHelper;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IFeatureCallValidator;
import io.sarl.lang.validation.IssueCodes;

/**
 * A specialized validator to deal with feature calls.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.14
 */
public class SARLFeatureCallsValidator extends AbstractSARLSubValidator {

	@Inject
	private IFeatureCallValidator featureCallValidator;

	@Inject
	private OperatorMapping operatorMapping;

	/** Check if the call is forbidden.
	 *
	 * <p>One example of a forbidden feature is {@link System#exit(int)}.
	 *
	 * @param expression the expression.
	 */
	@Check(CheckType.NORMAL)
	public void checkForbiddenCalls(XAbstractFeatureCall expression) {
		if (this.featureCallValidator.isDisallowedCall(expression)) {
			error(
					MessageFormat.format(
							Messages.SARLFeatureCallsValidator_1,
							expression.getFeature().getIdentifier()),
					expression,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					FORBIDDEN_REFERENCE);
		}
	}

	/** Check if the call is discouraged.
	 *
	 * <p>One example of a discouraged feature is {@link System#err}.
	 *
	 * @param expression the expression.
	 */
	@Check(CheckType.NORMAL)
	public void checkDiscouragedCalls(XAbstractFeatureCall expression) {
		if (!isIgnored(DISCOURAGED_REFERENCE)
				&& this.featureCallValidator.isDiscouragedCall(expression)) {
			addIssue(
					MessageFormat.format(Messages.SARLFeatureCallsValidator_2,
							expression.getConcreteSyntaxFeatureName()),
					expression,
					DISCOURAGED_REFERENCE);
		}
	}

	/** Check if the call to an unary minus operator may be ambiguous interpretation for the SARL
	 * developer.
	 * 
	 * <p>Let the SARL expression {@code -125.abs} that is invoking the function
	 * {@code Math.abs} with extension method notation. According to the precedence
	 * of the the SARL operator, this expression is interpreted as
	 * {@code -abs(125)} and not {@code abs(-125)}. Indeed the minus sign is an operator
	 * and not considered as a part of the number literal itself.
	 * 
	 * <p>To avoid invalid interpretation of the expression by the SARL developper, a
	 * warning is generated.
	 *
	 * @param expression the expression.
	 * @since 0.13
	 */
	@Check(CheckType.NORMAL)
	public void checkAmbiguousInterpretationMinusUnaryOperator(XUnaryOperation expression) {
		if (!isIgnored(AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER)
				&& getGrammarAccess().getHyphenMinusKeyword().equals(expression.getConcreteSyntaxFeatureName())
				&& (expression.getOperand() instanceof XFeatureCall
						|| expression.getOperand() instanceof XMemberFeatureCall)) {
			final var fc = (XAbstractFeatureCall) expression.getOperand();
			if (fc.isExtension()
					&& fc.getActualArguments() != null
					&& fc.getActualArguments().size() > 0
					&& fc.getActualArguments().get(0) instanceof XNumberLiteral cvalue) {
				addIssue(
						MessageFormat.format(Messages.SARLFeatureCallsValidator_3,
								Utils.getSarlCodeFor(expression),
								fc.getFeature().getSimpleName(),
								cvalue.getValue()),
						expression,
						AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER);
			}
		}
	}

	/** Check for usage of break inside loops.
	 *
	 * @param expression the expression to analyze.
	 */
	@Check(CheckType.NORMAL)
	public void checkBreakKeywordUse(SarlBreakExpression expression) {
		final var container = Utils.getFirstContainerForPredicate(expression,
				it -> Boolean.valueOf(!(it instanceof XExpression) || it instanceof XAbstractWhileExpression
						|| it instanceof XBasicForLoopExpression || it instanceof XForLoopExpression));
		if (container instanceof XExpression) {
			if (!isIgnored(DISCOURAGED_LOOP_BREAKING_KEYWORD_USE)
					&& container instanceof XBasicForLoopExpression) {
				addIssue(
						MessageFormat.format(Messages.SARLFeatureCallsValidator_4, getGrammarAccess().getBreakKeyword()),
						expression,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						DISCOURAGED_LOOP_BREAKING_KEYWORD_USE);
			}
		} else {
			error(MessageFormat.format(Messages.SARLFeatureCallsValidator_5, getGrammarAccess()
					.getBreakKeyword()),
					expression,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					INVALID_USE_OF_LOOP_BREAKING_KEYWORD);
		}
	}

	/** Check for usage of continue inside loops.
	 *
	 * @param expression the expression to analyze.
	 * @since 0.7
	 */
	@Check(CheckType.NORMAL)
	public void checkContinueKeywordUse(SarlContinueExpression expression) {
		final var container = Utils.getFirstContainerForPredicate(expression,
				it -> Boolean.valueOf(!(it instanceof XExpression) || it instanceof XAbstractWhileExpression
						|| it instanceof XBasicForLoopExpression || it instanceof XForLoopExpression));
		if (container instanceof XExpression) {
			if (!isIgnored(DISCOURAGED_LOOP_BREAKING_KEYWORD_USE)
					&& container instanceof XBasicForLoopExpression) {
				addIssue(
						MessageFormat.format(Messages.SARLFeatureCallsValidator_4, getGrammarAccess().getContinueKeyword()),
						expression,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						DISCOURAGED_LOOP_BREAKING_KEYWORD_USE);
			}
		} else {
			error(MessageFormat.format(Messages.SARLFeatureCallsValidator_5, getGrammarAccess().getContinueKeyword()),
					expression,
					null,
					ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
					INVALID_USE_OF_LOOP_BREAKING_KEYWORD);
		}
	}

	/** Check for usage of assert keyword.
	 *
	 * @param expression the expression to analyze.
	 */
	@Check(CheckType.NORMAL)
	public void checkAssertKeywordUse(SarlAssertExpression expression) {
		final var condition = expression.getCondition();
		if (condition != null) {
			final var fromType = getActualType(condition);
			if (!fromType.isAssignableFrom(Boolean.TYPE)) {
				error(MessageFormat.format(
						Messages.SARLFeatureCallsValidator_8,
						getNameOfTypes(fromType), boolean.class.getName()),
						condition,
						null,
						ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
						INCOMPATIBLE_TYPES);
				return;
			}
			if (getExpressionHelper() instanceof SARLExpressionHelper helper) {
				final var constant = helper.toBooleanPrimitiveWrapperConstant(condition);
				if (constant == Boolean.TRUE && !isIgnored(DISCOURAGED_BOOLEAN_EXPRESSION)) {
					addIssue(Messages.SARLFeatureCallsValidator_6,
							condition,
							null,
							ValidationMessageAcceptor.INSIGNIFICANT_INDEX,
							DISCOURAGED_BOOLEAN_EXPRESSION);
					return;
				}
			}
		}
	}

	/** Check for usage of assert keyword.
	 *
	 * @param expression the expression to analyze.
	 */
	@Check(CheckType.FAST)
	public void checkAssumeKeywordUse(SarlAssertExpression expression) {
		if (expression.isIsStatic()) {
			warning(MessageFormat.format(
					Messages.SARLFeatureCallsValidator_7,
					getGrammarAccess().getIsStaticAssumeKeyword()),
					expression,
					null,
					INSIGNIFICANT_INDEX,
					IssueCodes.UNSUPPORTED_STATEMENT);
		}
	}

	/** Check the signatures of the operator's function.
	 * 
	 * @param function the operator function to check.
	 */
	@Check(CheckType.NORMAL)
	public void checkOperatorSignature(XtendFunction function) {
		final var functionName = function.getName();
		if (functionName != null) {
			final var qualifiedName = QualifiedName.create(functionName);
			final var operator = this.operatorMapping.getOperator(qualifiedName);
			if (operator != null) {
				final var operation = getAssociations().getDirectlyInferredOperation(function);
				if (operation != null) {
					final var parameterSize = operation.getParameters().size();
					if (function.isStatic()) {
						if (this.operatorMapping.isUnaryOperator(operator) && this.operatorMapping.isBinaryOperator(operator)) {
							if (parameterSize < 1) {
								addIssue(MessageFormat.format(Messages.SARLFeatureCallsValidator_9, operator),
										function, XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_OPERATOR_SIGNATURE);
							} else if (parameterSize > 2) {
								addIssue(MessageFormat.format(Messages.SARLFeatureCallsValidator_10,  operator),
										function, XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_OPERATOR_SIGNATURE);
							}
						} else if (this.operatorMapping.isUnaryOperator(operator)) {
							if (parameterSize != 1) {
								addIssue(MessageFormat.format(Messages.SARLFeatureCallsValidator_11, operator),
										function, XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_OPERATOR_SIGNATURE);
							}
						} else  if (parameterSize != 2) {
							addIssue(MessageFormat.format(Messages.SARLFeatureCallsValidator_12,  operator),
									function, XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_OPERATOR_SIGNATURE);
						}
					} else {
						if (this.operatorMapping.isUnaryOperator(operator) && this.operatorMapping.isBinaryOperator(operator)) {
							if (parameterSize > 2) {
								addIssue(MessageFormat.format(Messages.SARLFeatureCallsValidator_13, operator),
										function, XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_OPERATOR_SIGNATURE);
							}
						} else if (this.operatorMapping.isUnaryOperator(operator)) {
							if (parameterSize > 1) {
								addIssue(MessageFormat.format(Messages.SARLFeatureCallsValidator_14, operator),
										function, XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_OPERATOR_SIGNATURE);
							}
						} else if (parameterSize == 0) {
							addIssue(MessageFormat.format(Messages.SARLFeatureCallsValidator_15, operator),
									function, XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_OPERATOR_SIGNATURE);

						} else if (parameterSize > 2) {
							addIssue(MessageFormat.format(Messages.SARLFeatureCallsValidator_16,  operator),
									function, XtendPackage.Literals.XTEND_FUNCTION__NAME, INVALID_OPERATOR_SIGNATURE);
						}
					}
				}
			}
		}
	}

	/** Check if the left side of an assignment is a valid variable.
	 *
	 * @param assignment the assignment operator to check.
	 */
	@Check(CheckType.FAST)
	public void checkLeftHandSideIsVariable(XAssignment assignment){
		final var concreteSyntaxFeatureName = assignment.getConcreteSyntaxFeatureName();
		if (concreteSyntaxFeatureName.equals(IFeatureNames.THIS.toString())) {
			error(Messages.SARLFeatureCallsValidator_17, XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE,
					LEFT_HAND_SIDE_MUST_BE_VARIABLE);
		}
	}

	/** Check if a ternary expression is defined outside another ternary expression.
	 *
	 * @param exp the if statement to check.
	 */
	@Check(CheckType.FAST)
	public void checkTernaryExpressionUsed(XIfExpression exp) {
		if (exp.isConditionalExpression()) {
			//Check if this expression is nested in another already marked ternary expression
			final var container = exp.eContainer();
			if (container instanceof XIfExpression cvalue && cvalue.isConditionalExpression()) {
				return;
			}
			addIssue(Messages.SARLFeatureCallsValidator_18, exp, TERNARY_EXPRESSION_NOT_ALLOWED);
		}
	}

}
