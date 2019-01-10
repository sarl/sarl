/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.lang.typesystem;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.google.common.collect.Iterables;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.util.AnnotationLookup;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XAbstractWhileExpression;
import org.eclipse.xtext.xbase.XAssignment;
import org.eclipse.xtext.xbase.XBasicForLoopExpression;
import org.eclipse.xtext.xbase.XBinaryOperation;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XCasePart;
import org.eclipse.xtext.xbase.XCastedExpression;
import org.eclipse.xtext.xbase.XCatchClause;
import org.eclipse.xtext.xbase.XCollectionLiteral;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XForLoopExpression;
import org.eclipse.xtext.xbase.XIfExpression;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XPostfixOperation;
import org.eclipse.xtext.xbase.XReturnExpression;
import org.eclipse.xtext.xbase.XSwitchExpression;
import org.eclipse.xtext.xbase.XSynchronizedExpression;
import org.eclipse.xtext.xbase.XThrowExpression;
import org.eclipse.xtext.xbase.XTryCatchFinallyExpression;
import org.eclipse.xtext.xbase.XUnaryOperation;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlContinueExpression;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.sarl.actionprototype.InferredPrototype;
import io.sarl.lang.sarl.actionprototype.QualifiedActionName;
import io.sarl.lang.util.Utils;

/**
 * Helper on operations.
 *
 * <p>This implementation extends the Xtend expression helper by assuming that any function
 * with a name starting with "get", "is", "has" is a pure function.
 * It also assumes that "equals", "hashCode", "clone" and "toString" are also pure functions.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
@Singleton
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class SARLOperationHelper implements IOperationHelper {

	@Inject
	private IPureOperationNameValidator nameValidator;

	@Inject
	private AnnotationLookup annotations;

	@Inject
	private CommonTypeComputationServices services;

	@Inject
	private IActionPrototypeProvider actionPrototypes;

	@Inject
	private SarlJvmModelAssociations associations;

	@Inject
	private OperatorMapping operatorMapping;

	private final PolymorphicDispatcher<Boolean> hasSideEffectsDispatcher;

	/** Constructor.
	 */
	public SARLOperationHelper() {
		this.hasSideEffectsDispatcher = new PolymorphicDispatcher<Boolean>(
				"_hasSideEffects", 2, 2, //$NON-NLS-1$
				Collections.singletonList(this)) {
			@Override
			protected Boolean handleNoSuchMethod(Object... params) {
				return Boolean.FALSE;
			}
		};
	}

	/** Replies if the given function is purable according to its modifiers and prototype.
	 *
	 * <p>Basically, an operation is purable if:<ul>
	 * <li></li>
	 * </ul>
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation is not pure according to the prototype.
	 */
	@SuppressWarnings("static-method")
	protected boolean isUnpureOperationPrototype(XtendFunction operation) {
		if (operation == null
				|| operation.isAbstract() || operation.isDispatch() || operation.isNative()
				|| operation.getExpression() == null) {
			return true;
		}
		final XtendTypeDeclaration declaringType = operation.getDeclaringType();
		return declaringType instanceof SarlCapacity;
	}

	@Override
	public boolean isPurableOperation(XtendFunction operation) {
		return isPurableOperation(operation, null);
	}

	/** Replies if the given is purable in the given context.
	 *
	 * @param operation the operation to test.
	 * @param context the context.
	 * @return {@code true} if the operation could be marked as pure.
	 */
	boolean isPurableOperation(XtendFunction operation, ISideEffectContext context) {
		if (isUnpureOperationPrototype(operation)) {
			return false;
		}
		if (this.nameValidator.isNamePatternForNotPureOperation(operation)) {
			return false;
		}
		if (this.nameValidator.isNamePatternForPureOperation(operation)) {
			return true;
		}
		if (context == null) {
			return !hasSideEffects(
					getInferredPrototype(operation),
					operation.getExpression());
		}
		final Boolean result = internalHasSideEffects(operation.getExpression(), context);
		return result == null || !result.booleanValue();
	}

	private InferredPrototype getInferredPrototype(XtendFunction operation) {
		final JvmIdentifiableElement container = this.associations.getInferredType(operation.getDeclaringType());
		final QualifiedActionName actionKey = this.actionPrototypes.createQualifiedActionName(
				container, operation.getName());
		// Compute the different action prototypes associated to the action to create.
		final boolean isVarArgs = Utils.isVarArg(operation.getParameters());
		return this.actionPrototypes.createPrototypeFromSarlModel(
				actionKey, isVarArgs, operation.getParameters());
	}

	private InferredPrototype getInferredPrototype(JvmOperation operation) {
		final XtendFunction fct = this.associations.getXtendFunction(operation);
		if (fct != null) {
			return getInferredPrototype(fct);
		}
		final QualifiedActionName actionKey = this.actionPrototypes.createQualifiedActionName(
				operation.getDeclaringType(), operation.getSimpleName());
		// Compute the different action prototypes associated to the action to create.
		return this.actionPrototypes.createPrototypeFromJvmModel(
				actionKey, operation.isVarArgs(), operation.getParameters());
	}

	private Boolean internalHasSideEffects(XExpression expr, ISideEffectContext ctx) {
		final Boolean result = hasSideEffects(expr, ctx);
		if (result != null && result.booleanValue()) {
			return Boolean.TRUE;
		}
		return Boolean.FALSE;
	}

	@Override
	public boolean hasSideEffects(InferredPrototype calledOperation, XExpression expr) {
		final SideEffectContext ctx = new SideEffectContext(calledOperation);
		return internalHasSideEffects(expr, ctx);
	}

	/** Determine if the given expression has a side effect.
	 *
	 * @param calledOperation the called operation, not yet in the call stack.
	 * @param expr the expression.
	 * @param context the context.
	 * @return {@code true} if the expression has a side effect.
	 */
	protected Boolean hasSideEffects(InferredPrototype calledOperation, XExpression expr, ISideEffectContext context) {
		final SideEffectContext ctx = new SideEffectContext(calledOperation, context);
		return internalHasSideEffects(expr, ctx);
	}

	/** Determine if the given expression has a side effect.
	 *
	 * @param expr the expression.
	 * @param context the context.
	 * @return {@code true} if the expression has a side effect.
	 */
	protected Boolean hasSideEffects(XExpression expr, ISideEffectContext context) {
		if (expr != null && !expr.eIsProxy()) {
			return this.hasSideEffectsDispatcher.invoke(expr, context);
		}
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XSynchronizedExpression expression, ISideEffectContext context) {
		return hasSideEffects(expression.getExpression(), context);
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XCastedExpression expression, ISideEffectContext context) {
		return hasSideEffects(expression.getTarget(), context);
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected boolean _hasSideEffects(XAbstractWhileExpression expression, ISideEffectContext context) {
		context.open();
		if (hasSideEffects(expression.getPredicate(), context)) {
			return true;
		}
		if (hasSideEffects(expression.getBody(), context.branch())) {
			return true;
		}
		context.close();
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XForLoopExpression expression, ISideEffectContext context) {
		context.open();
		if (hasSideEffects(expression.getForExpression(), context)) {
			return true;
		}
		context.close();
		return hasSideEffects(expression.getEachExpression(), context.branch());
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XIfExpression expression, ISideEffectContext context) {
		if (hasSideEffects(expression.getIf(), context)) {
			return true;
		}
		final Map<String, List<XExpression>> buffer1 = context.createVariableAssignmentBufferForBranch();
		if (hasSideEffects(expression.getThen(), context.branch(buffer1))) {
			return true;
		}
		final Map<String, List<XExpression>> buffer2 = context.createVariableAssignmentBufferForBranch();
		if (hasSideEffects(expression.getElse(), context.branch(buffer2))) {
			return true;
		}
		context.mergeBranchVariableAssignments(Arrays.asList(buffer1, buffer2));
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XReturnExpression expression, ISideEffectContext context) {
		return hasSideEffects(expression.getExpression(), context);
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XThrowExpression expression, ISideEffectContext context) {
		return hasSideEffects(expression.getExpression(), context);
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XTryCatchFinallyExpression expression, ISideEffectContext context) {
		final List<Map<String, List<XExpression>>> buffers = new ArrayList<>();
		Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
		if (hasSideEffects(expression.getExpression(), context.branch(buffer))) {
			return true;
		}
		buffers.add(buffer);
		for (final XCatchClause clause : expression.getCatchClauses()) {
			context.open();
			buffer = context.createVariableAssignmentBufferForBranch();
			if (hasSideEffects(clause.getExpression(), context.branch(buffer))) {
				return true;
			}
			buffers.add(buffer);
			context.close();
		}
		context.mergeBranchVariableAssignments(buffers);
		if (hasSideEffects(expression.getFinallyExpression(), context)) {
			return true;
		}
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XVariableDeclaration expression, ISideEffectContext context) {
		if (hasSideEffects(expression.getRight(), context)) {
			return true;
		}
		context.declareVariable(expression.getIdentifier(), expression.getRight());
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XBasicForLoopExpression expression, ISideEffectContext context) {
		context.open();
		for (final XExpression ex : expression.getInitExpressions()) {
			if (hasSideEffects(ex, context)) {
				return true;
			}
		}
		if (hasSideEffects(expression.getEachExpression(), context)) {
			return true;
		}
		for (final XExpression ex : expression.getUpdateExpressions()) {
			if (hasSideEffects(ex, context.branch())) {
				return true;
			}
		}
		if (hasSideEffects(expression.getExpression(), context.branch())) {
			return true;
		}
		context.close();
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XSwitchExpression expression, ISideEffectContext context) {
		context.open();
		if (hasSideEffects(expression.getSwitch(), context)) {
			return true;
		}
		final List<Map<String, List<XExpression>>> buffers = new ArrayList<>();
		for (final XCasePart ex : expression.getCases()) {
			context.open();
			if (hasSideEffects(ex.getCase(), context)) {
				return true;
			}
			final Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
			if (hasSideEffects(ex.getThen(), context.branch(buffer))) {
				return true;
			}
			buffers.add(buffer);
			context.close();
		}
		final Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
		if (hasSideEffects(expression.getDefault(), context.branch(buffer))) {
			return true;
		}
		buffers.add(buffer);
		context.mergeBranchVariableAssignments(buffers);
		context.close();
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XCollectionLiteral expression, ISideEffectContext context) {
		context.open();
		for (final XExpression ex : expression.getElements()) {
			if (hasSideEffects(ex, context)) {
				return true;
			}
		}
		context.close();
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XConstructorCall expression, ISideEffectContext context) {
		for (final XExpression ex : expression.getArguments()) {
			if (hasSideEffects(ex, context)) {
				return true;
			}
		}
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XBinaryOperation expression, ISideEffectContext context) {
		if (isReassignmentOperator(expression)) {
			if (!isLocalExpression(expression.getLeftOperand(), context, false)) {
				return true;
			}
		} else {
			if (expression.isTypeLiteral() || expression.isPackageFragment()) {
				return false;
			}
			if (hasSideEffects(expression.getLeftOperand(), context)) {
				return true;
			}
		}
		if (hasSideEffects(expression.getRightOperand(), context)) {
			return true;
		}
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XUnaryOperation expression, ISideEffectContext context) {
		if (expression.isTypeLiteral() || expression.isPackageFragment()) {
			return Boolean.FALSE;
		}
		if (hasSideEffects(expression.getOperand(), context)) {
			return true;
		}
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	@SuppressWarnings("static-method")
	protected Boolean _hasSideEffects(XPostfixOperation expression, ISideEffectContext context) {
		return Boolean.TRUE;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XFeatureCall expression, ISideEffectContext context) {
		return internalHasOperationSideEffects(expression, context, false);
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XMemberFeatureCall expression, ISideEffectContext context) {
		return internalHasOperationSideEffects(expression, context, false);
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XAssignment expression, ISideEffectContext context) {
		if (internalHasOperationSideEffects(expression, context, true)) {
			return true;
		}
		if (hasSideEffects(expression.getValue(), context)) {
			return true;
		}
		final JvmIdentifiableElement feature = expression.getFeature();
		if (feature instanceof XVariableDeclaration) {
			context.assignVariable(feature.getIdentifier(), expression.getValue());
		}
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XBlockExpression expression, ISideEffectContext context) {
		final List<XExpression> exprs = expression.getExpressions();
		if (exprs != null && !exprs.isEmpty()) {
			context.open();
			for (final XExpression ex : exprs) {
				if (hasSideEffects(ex, context)) {
					return true;
				}
			}
			context.close();
		}
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	@SuppressWarnings("static-method")
	protected Boolean _hasSideEffects(SarlAssertExpression expression, ISideEffectContext context) {
		// Assume => expression.isStatic === true
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	@SuppressWarnings("static-method")
	protected Boolean _hasSideEffects(SarlBreakExpression expression, ISideEffectContext context) {
		return false;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 * @since 0.7
	 */
	@SuppressWarnings("static-method")
	protected Boolean _hasSideEffects(SarlContinueExpression expression, ISideEffectContext context) {
		return false;
	}

	/** Replies if the given operator is a reassignment operator.
	 * A reassignment operator changes its left operand.
	 *
	 * @param operator the operator.
	 * @return {@code true} if the operator changes its left operand.
	 */
	protected boolean isReassignmentOperator(XBinaryOperation operator) {
		if (operator.isReassignFirstArgument()) {
			return true;
		}
		final QualifiedName operatorName = this.operatorMapping.getOperator(
				QualifiedName.create(operator.getFeature().getSimpleName()));
		final QualifiedName compboundOperatorName = this.operatorMapping.getSimpleOperator(operatorName);
		return compboundOperatorName != null;
	}

	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity", "checkstyle:returncount"})
	private boolean internalHasOperationSideEffects(XAbstractFeatureCall expression, ISideEffectContext context,
			boolean sideEffectForExternalElements) {
		if (expression.eIsProxy()) {
			return false;
		}
		if (expression.isTypeLiteral() || expression.isPackageFragment()) {
			return false;
		}
		JvmIdentifiableElement feature = expression.getFeature();
		if (feature == null) {
			// Assuming that if the link is not done, the feature is outside the operation's scope.
			return false;
		}
		if (feature.eIsProxy()) {
			// Assuming that if the link is not done, the feature is outside the operation's scope.
			feature = (JvmIdentifiableElement) EcoreUtil.resolve(feature, feature.eResource());
			if (feature.eIsProxy()) {
				return true;
			}
		}
		if (feature instanceof JvmOperation) {
			final JvmOperation operation = (JvmOperation) feature;
			if (isCalledOperation(operation, context.getCalledOperations())) {
				// Recursive call detected.
				// We assume no border effect in order to let the other expressions in the operation
				// to influence the pure state of the operation.
				return false;
			}
			final ISideEffectContext ctx = new SideEffectContext(
					Iterables.concat(context.getCalledOperations(),
							Collections.singleton(getInferredPrototype(operation))));
			if (this.annotations.findAnnotation(operation, Pure.class) != null
					|| evaluatePureAnnotationAdapters(operation, ctx)) {
				return false;
			}
			if (this.nameValidator.isNamePatternForNotPureOperation(operation)) {
				return true;
			}
			if (this.nameValidator.isNamePatternForPureOperation(operation)
					|| (operation.isStatic() && hasPrimitiveParameters(operation))) {
				for (final XExpression ex : expression.getActualArguments()) {
					final Boolean bool = hasSideEffects(ex, context);
					if (bool != null && bool.booleanValue()) {
						return true;
					}
				}
				return false;
			}
			// Test if the receiver is local or not
			return !isLocalExpression(expression.getActualReceiver(), context, true);
		} else if (sideEffectForExternalElements && isExternalFeature(feature)) {
			return true;
		}
		return false;
	}

	private static boolean isCalledOperation(JvmOperation operation, List<InferredPrototype> prototypes) {
		final String container = operation.getDeclaringType().getQualifiedName();
		for (final InferredPrototype prototype : prototypes) {
			if (Strings.equal(container, prototype.getActionName().getDeclaringType().getIdentifier())
					&& Strings.equal(operation.getSimpleName(), prototype.getActionName().getActionName())) {
				final String prefix = container + "."; //$NON-NLS-1$
				final String operationId = operation.getIdentifier();
				for (final ActionParameterTypes types : prototype.getParameterTypeAlternatives()) {
					String name = prefix + types.toActionPrototype(operation.getSimpleName()).toString();
					if (Strings.equal(operationId, name)) {
						return true;
					}
					name = prefix + types.toRawActionPrototype(operation.getSimpleName()).toString();
					if (Strings.equal(operationId, name)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	private static boolean isExternalFeature(JvmIdentifiableElement feature) {
		return feature instanceof JvmMember || feature instanceof JvmFormalParameter;
	}

	private static boolean isLocalExpression(XExpression expression, ISideEffectContext context, boolean dereference) {
		if (expression == null) {
			return true;
		}
		if (expression instanceof XAbstractFeatureCall) {
			return isLocalExpression((XAbstractFeatureCall) expression, context, dereference);
		}
		for (final XAbstractFeatureCall featureCall : EcoreUtil2.getAllContentsOfType(expression, XAbstractFeatureCall.class)) {
			if (!isLocalExpression(featureCall, context, dereference)) {
				return false;
			}
		}
		return true;
	}

	private static boolean isLocalExpression(XAbstractFeatureCall expression, ISideEffectContext context, boolean dereference) {
		if (expression.isTypeLiteral() || expression.isPackageFragment()) {
			return false;
		}
		final JvmIdentifiableElement feature = expression.getFeature();
		if (feature != null && (feature.eIsProxy() || isExternalFeature(feature))) {
			return false;
		}
		if (feature instanceof XVariableDeclaration) {
			if (dereference) {
				final XVariableDeclaration variable = (XVariableDeclaration) feature;
				for (final XExpression value : context.getVariableValues(variable.getIdentifier())) {
					if (!isLocalExpression(value, context, dereference)) {
						return false;
					}
				}
			}
		}
		return true;
	}

	private boolean hasPrimitiveParameters(JvmOperation op) {
		for (final JvmFormalParameter parameter : op.getParameters()) {
			final JvmTypeReference type = parameter.getParameterType();
			if (type == null || !Utils.toLightweightTypeReference(type, this.services).isPrimitive()) {
				return false;
			}
		}
		return true;
	}

	@Override
	public boolean evaluatePureAnnotationAdapters(JvmOperation operation) {
		return evaluatePureAnnotationAdapters(operation, null);
	}

	/** Evalute the Pure annotatino adapters.
	 *
	 * @param operation the operation to adapt.
	 * @param context the context.
	 * @return {@code true} if the pure annotation could be associated to the given operation.
	 */
	boolean evaluatePureAnnotationAdapters(org.eclipse.xtext.common.types.JvmOperation operation, ISideEffectContext context) {
		int index = -1;
		int i = 0;
		for (final Adapter adapter : operation.eAdapters()) {
			if (adapter.isAdapterForType(AnnotationJavaGenerationAdapter.class)) {
				index = i;
				break;
			}
			++i;
		}
		if (index >= 0) {
			final AnnotationJavaGenerationAdapter annotationAdapter = (AnnotationJavaGenerationAdapter) operation.eAdapters().get(index);
			assert annotationAdapter != null;
			return annotationAdapter.applyAdaptations(this, operation, context);
		}
		return false;
	}

	@Override
	public void attachPureAnnotationAdapter(JvmOperation operation,
			Function2<? super JvmOperation, ? super IOperationHelper, ? extends Boolean> dynamicCallback) {
		if (operation != null && dynamicCallback != null) {
			AnnotationJavaGenerationAdapter adapter = (AnnotationJavaGenerationAdapter) EcoreUtil.getAdapter(
					operation.eAdapters(), AnnotationJavaGenerationAdapter.class);
			if (adapter == null) {
				adapter = new AnnotationJavaGenerationAdapter();
				operation.eAdapters().add(adapter);
			}
			adapter.addPredicate(dynamicCallback);
		}
	}

	@Override
	public boolean isPureOperation(JvmOperation operation) {
		if (operation == null) {
			return false;
		}
		return this.annotations.findAnnotation(operation, Pure.class) != null
				|| evaluatePureAnnotationAdapters(operation);
	}

	/** Context for the side effect.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static final class SideEffectContext implements ISideEffectContext {

		private final List<InferredPrototype> calledOperations = new ArrayList<>();

		private final Deque<InternalContext> contextStack;

		private final boolean isBranchContext;

		private final Map<String, List<XExpression>> variableAssignmentBuffer;

		/** Constructor.
		 *
		 * @param calledOperation the called operation.
		 */
		SideEffectContext(InferredPrototype calledOperation) {
			if (calledOperation != null) {
				this.calledOperations.add(calledOperation);
			}
			this.variableAssignmentBuffer = null;
			this.isBranchContext = false;
			this.contextStack = new LinkedList<>();
			this.contextStack.addLast(new InternalContext(null));
		}

		/** Constructor.
		 *
		 * @param calledOperations the operations that are called before the called operation in the call stack.
		 */
		SideEffectContext(Iterable<InferredPrototype> calledOperations) {
			if (calledOperations != null) {
				for (final InferredPrototype proto : calledOperations) {
					this.calledOperations.add(proto);
				}
			}
			this.variableAssignmentBuffer = null;
			this.isBranchContext = false;
			this.contextStack = new LinkedList<>();
			this.contextStack.addLast(new InternalContext(null));
		}

		/** Constructor.
		 *
		 * @param calledOperation the operation that is called.
		 * @param context the context to be copied.
		 */
		SideEffectContext(InferredPrototype calledOperation, ISideEffectContext context) {
			this.calledOperations.addAll(context.getCalledOperations());
			if (calledOperation != null) {
				this.calledOperations.add(calledOperation);
			}
			this.variableAssignmentBuffer = null;
			this.isBranchContext = false;
			this.contextStack = new LinkedList<>();
			this.contextStack.addLast(new InternalContext(null));
		}

		private SideEffectContext(List<InferredPrototype> calledOperations, Deque<InternalContext> contextStack,
				Map<String, List<XExpression>> buffer) {
			if (calledOperations != null) {
				this.calledOperations.addAll(calledOperations);
			}
			this.variableAssignmentBuffer = buffer;
			this.isBranchContext = true;
			this.contextStack = contextStack;
		}

		@Override
		public List<InferredPrototype> getCalledOperations() {
			return Collections.unmodifiableList(this.calledOperations);
		}

		@Override
		public String toString() {
			final StringBuilder buffer = new StringBuilder();
			for (final InferredPrototype proto : getCalledOperations()) {
				buffer.append("> "); //$NON-NLS-1$
				buffer.append(proto.getActionName().getActionName());
				buffer.append("("); //$NON-NLS-1$
				buffer.append(proto.toString());
				buffer.append(")\n"); //$NON-NLS-1$
			}
			final Iterator<InternalContext> iterator = this.contextStack.descendingIterator();
			while (iterator.hasNext()) {
				final InternalContext ctx = iterator.next();
				buffer.append("-----------------------------------\n"); //$NON-NLS-1$
				buffer.append(ctx.toString());
			}
			return buffer.toString();
		}

		@Override
		public Map<String, List<XExpression>> createVariableAssignmentBufferForBranch() {
			return new HashMap<>();
		}

		@Override
		public void mergeBranchVariableAssignments(List<Map<String, List<XExpression>>> buffers) {
			@SuppressWarnings("checkstyle:all")
			class Data {
				public int occurrences;
				public final List<XExpression> expressions = new ArrayList<>();
			}
			final Map<String, Data> full = new TreeMap<>();
			for (final Map<String, List<XExpression>> definition : buffers) {
				for (final Entry<String, List<XExpression>> entry : definition.entrySet()) {
					Data doublet = full.get(entry.getKey());
					if (doublet == null) {
						doublet = new Data();
						full.put(entry.getKey(), doublet);
					}
					doublet.expressions.addAll(entry.getValue());
					doublet.occurrences = doublet.occurrences + 1;
				}
			}
			for (final Entry<String, Data> entry : full.entrySet()) {
				final Data data = entry.getValue();
				if (data.occurrences < buffers.size()) {
					// The default value should remains because a branch does not give a value to the variable
					this.contextStack.getLast().assignVariable(entry.getKey(), data.expressions, true);
				} else {
					// All branches are defining a value for the variable
					this.contextStack.getLast().assignVariable(entry.getKey(), data.expressions, false);
				}
			}
		}

		@Override
		public ISideEffectContext branch(Map<String, List<XExpression>> buffer) {
			return new SideEffectContext(getCalledOperations(), this.contextStack, buffer);
		}

		@Override
		public void open() {
			this.contextStack.addLast(new InternalContext(this.contextStack.getLast()));
		}

		@Override
		public void close() {
			if (this.contextStack.size() > 1) {
				this.contextStack.removeLast();
			}
		}

		@Override
		public void declareVariable(String id, XExpression expression) {
			this.contextStack.getLast().declareVariable(id, expression);
		}

		@Override
		public void assignVariable(String id, XExpression expression) {
			final InternalContext ctx = this.contextStack.getLast();
			if (this.variableAssignmentBuffer != null && !ctx.isLocalVariable(id)) {
				List<XExpression> expressions = this.variableAssignmentBuffer.get(id);
				if (expressions == null) {
					expressions = new ArrayList<>();
					this.variableAssignmentBuffer.put(id, expressions);
				}
				expressions.add(expression);
			} else {
				ctx.assignVariable(id, Collections.singletonList(expression), this.isBranchContext);
			}
		}

		@Override
		public List<? extends XExpression> getVariableValues(String name) {
			return this.contextStack.getLast().getVariableValues(name);
		}

		/** A context.
		 *
		 * @author $Author: sgalland$
		 * @version $FullVersion$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 * @since 0.6
		 */
		private static class InternalContext {

			private final WeakReference<InternalContext> parent;

			private Map<String, List<XExpression>> variables;

			/** Constructor.
			 *
			 * @param parent the parent context.
			 */
			InternalContext(InternalContext parent) {
				this.parent = new WeakReference<>(parent);
			}

			@Override
			public String toString() {
				final StringBuilder buffer = new StringBuilder();
				if (this.variables != null) {
					for (final Entry<String, List<XExpression>> variable : this.variables.entrySet()) {
						buffer.append(variable.getKey());
						buffer.append(" = "); //$NON-NLS-1$
						buffer.append(variable.getValue());
						buffer.append("\n"); //$NON-NLS-1$
					}
				}
				return buffer.toString();
			}

			/** Replies the parent context.
			 *
			 * @return the parent context or {@code null}.
			 */
			public InternalContext parent() {
				return this.parent.get();
			}

			/** Replies if the given name is the one of a local variable.
			 *
			 * @param id the variable identifier.
			 * @return {@code true} if the variable is local.
			 */
			public boolean isLocalVariable(String id) {
				return this.variables != null && this.variables.containsKey(id);
			}

			/** Declare a local variable.
			 *
			 * @param id the identifier of the variable.
			 * @param expression initialization expression.
			 */
			public void declareVariable(String id, XExpression expression) {
				if (this.variables == null) {
					this.variables = new HashMap<>();
				}
				final List<XExpression> values = new ArrayList<>();
				values.add(expression);
				this.variables.put(id, values);
			}

			/** Assign a local variable.
			 *
			 * @param id the identifier of the variable.
			 * @param expressions the new values.
			 * @param isBranchContext indicates if this context is a branch.
			 */
			public void assignVariable(String id, Collection<XExpression> expressions, boolean isBranchContext) {
				if (expressions != null) {
					InternalContext ctx = this;
					while (ctx != null) {
						if (ctx.variables != null) {
							final List<XExpression> localDeclaration = ctx.variables.get(id);
							if (localDeclaration != null) {
								if (isBranchContext) {
									localDeclaration.addAll(expressions);
								} else {
									localDeclaration.clear();
									localDeclaration.addAll(expressions);
								}
								return;
							}
						}
						ctx = ctx.parent();
					}
				}
			}

			/** Replies the values of the variable with the given nbame.
			 *
			 * @param id the name of the variable.
			 * @return the values.
			 */
			public List<? extends XExpression> getVariableValues(String id) {
				InternalContext ctx = this;
				while (ctx != null) {
					if (ctx.variables != null) {
						final List<XExpression> localDeclaration = ctx.variables.get(id);
						if (localDeclaration != null) {
							return Collections.unmodifiableList(localDeclaration);
						}
					}
					ctx = ctx.parent();
				}
				return Collections.emptyList();
			}

		}

	}

	/** Adapter that enables to adda annotation when generating the Java code.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class AnnotationJavaGenerationAdapter extends AdapterImpl {

		private Collection<Function2<? super JvmOperation, ? super IOperationHelper, ? extends Boolean>> predicates;

		/** Add a predicate.
		 *
		 * @param predicate the predicate.
		 */
		public void addPredicate(Function2<? super JvmOperation, ? super IOperationHelper, ? extends Boolean> predicate) {
			if (predicate != null) {
				synchronized (this) {
					if (this.predicates == null) {
						this.predicates = new ArrayList<>();
					}
					this.predicates.add(predicate);
				}
			}
		}

		/** Remove all the predicates.
		 */
		public void removeAllPredicates() {
			synchronized (this) {
				this.predicates = null;
			}
		}

		@Override
		public boolean isAdapterForType(Object type) {
			return type == AnnotationJavaGenerationAdapter.class;
		}

		/** Evaluate the predicates.
		 *
		 * @param helper the helper.
		 * @param operation the operation to adapt.
		 * @param context the context or {@code null} if none.
		 * @return the view to the operation with adaptations.
		 */
		public boolean applyAdaptations(IOperationHelper helper, JvmOperation operation, ISideEffectContext context) {
			synchronized (this) {
				if (this.predicates != null && !this.predicates.isEmpty()) {
					for (final Function2<? super JvmOperation, ? super IOperationHelper, ? extends Boolean> predicate : this.predicates) {
						final IOperationHelper hlp;
						if (context != null && helper instanceof SARLOperationHelper) {
							hlp = new SubHelper((SARLOperationHelper) helper, context);
						} else {
							hlp = helper;
						}
						final Boolean bool = predicate.apply(operation, hlp);
						if (bool != null && bool.booleanValue()) {
							return true;
						}
					}
				}
			}
			return false;
		}

	}

	/** Internal sub helper. This helper forces the use of a specific side effect context.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	private static class SubHelper implements IOperationHelper {

		private final SARLOperationHelper delegate;

		private final ISideEffectContext context;

		/** Constructor.
		 *
		 * @param original the original helper.
		 * @param context the current side effect context.
		 */
		SubHelper(SARLOperationHelper original, ISideEffectContext context) {
			this.delegate = original;
			this.context = context;
		}

		@Override
		public boolean isPurableOperation(XtendFunction operation) {
			return this.delegate.isPurableOperation(operation, this.context);
		}

		@Override
		public boolean isPureOperation(JvmOperation operation) {
			return this.delegate.isPureOperation(operation);
		}

		@Override
		public boolean hasSideEffects(InferredPrototype calledOperation, XExpression expr) {
			return this.delegate.hasSideEffects(calledOperation, expr, this.context);
		}

		@Override
		public boolean evaluatePureAnnotationAdapters(JvmOperation operation) {
			return this.delegate.evaluatePureAnnotationAdapters(operation, this.context);
		}

		@Override
		public void attachPureAnnotationAdapter(JvmOperation operation,
				Function2<? super JvmOperation, ? super IOperationHelper, ? extends Boolean> dynamicCallback) {
			this.delegate.attachPureAnnotationAdapter(operation, dynamicCallback);
		}

	}

}
