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
import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendParameter;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmConstructor;
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

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAssertExpression;
import io.sarl.lang.sarl.SarlBreakExpression;
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
	 * <p>Basically, an operation cannot be pure if:<ul>
	 * <li>native,</li>
	 * <li>the return type is void.</li>
	 * </ul>
	 *
	 * <p>This function is called usually before testing the pattern of the function's name.
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the operation is not pure according to the prototype.
	 * @since 0.10
	 */
	@SuppressWarnings("static-method")
	protected boolean isPureStateForbidden(XtendFunction operation) {
		if (operation == null
				|| operation.isNative()) {
			return true;
		}
		//  Return type with void means that the function has a side effect
		JvmTypeReference returnType = operation.getReturnType();
		if (returnType != null) {
			// The type is specified in SARL
			return Void.TYPE.getName().equals(returnType.getIdentifier());
		}
		final JvmOperation jvmOperation = this.associations.getDirectlyInferredOperation(operation);
		if (jvmOperation != null) {
			returnType = jvmOperation.getReturnType();
			if (returnType != null) {
				// The type is specified in SARL
				return Void.TYPE.getName().equals(returnType.getIdentifier());
			}
			return false;
		}
		return true;
	}

	/** Replies if it is impossible to determine the pure state of the given function .
	 *
	 * @param operation the operation to test.
	 * @return {@code true} if the pure state of the operation cannot be determined.
	 * @since 0.10
	 */
	@SuppressWarnings("static-method")
	protected boolean isPureStateAmbiguous(XtendFunction operation) {
		return operation.isAbstract() || operation.getExpression() == null;
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
		if (isPureStateForbidden(operation)) {
			return false;
		}
		if (this.nameValidator.isNamePatternForNotPureOperation(operation)) {
			return false;
		}
		if (this.nameValidator.isNamePatternForPureOperation(operation)) {
			return true;
		}
		if (isPureStateAmbiguous(operation)) {
			return false;
		}
		// Test the body
		if (context == null) {
			return !hasSideEffects(
					getInferredPrototype(operation),
					operation.getExpression());
		}
		final Boolean result = internalHasSideEffects(operation.getExpression(), context);
		return result == null || !result.booleanValue();
	}

	private InferredPrototype createInferredPrototype(QualifiedActionName executableKey, List<XtendParameter> parameters) {
		final boolean isVarArgs = Utils.isVarArg(parameters);
		return this.actionPrototypes.createPrototypeFromSarlModel(
				// TODO more general context?
				this.actionPrototypes.createContext(),
				executableKey, isVarArgs, parameters);
	}

	private InferredPrototype createInferredPrototype(QualifiedActionName executableKey, boolean isVarArgs, List<JvmFormalParameter> parameters) {
		return this.actionPrototypes.createPrototypeFromJvmModel(
				// TODO more general context?
				this.actionPrototypes.createContext(),
				executableKey, isVarArgs, parameters);
	}

	/** Replies the inferred prototype of the given operation.
	 *
	 * @param operation the operation.
	 * @return the inferred prototype.
	 * @since 0.12
	 */
	public InferredPrototype getInferredPrototype(XtendFunction operation) {
		final JvmIdentifiableElement container = this.associations.getInferredType(operation.getDeclaringType());
		final QualifiedActionName actionKey = this.actionPrototypes.createQualifiedActionName(
				container, operation.getName());
		// Compute the different action prototypes associated to the action to create.
		return createInferredPrototype(actionKey, operation.getParameters());
	}

	/** Replies the inferred prototype of the given constructor.
	 *
	 * @param constructor the constructor.
	 * @return the inferred prototype.
	 * @since 0.12
	 */
	public InferredPrototype getInferredPrototype(XtendConstructor constructor) {
		final JvmIdentifiableElement container = this.associations.getInferredType(constructor.getDeclaringType());
		final QualifiedActionName constructorKey = this.actionPrototypes.createConstructorQualifiedName(container);
		// Compute the different action prototypes associated to the action to create.
		return createInferredPrototype(constructorKey, constructor.getParameters());
	}

	/** Replies the inferred prototype of the given operation.
	 *
	 * @param operation the operation.
	 * @return the inferred prototype.
	 * @since 0.12
	 */
	public InferredPrototype getInferredPrototype(JvmOperation operation) {
		final XtendFunction fct = this.associations.getXtendFunction(operation);
		if (fct != null) {
			return getInferredPrototype(fct);
		}
		final QualifiedActionName actionKey = this.actionPrototypes.createQualifiedActionName(
				operation.getDeclaringType(), operation.getSimpleName());
		// Compute the different action prototypes associated to the action to create.
		return createInferredPrototype(actionKey, operation.isVarArgs(), operation.getParameters());
	}

	/** Replies the inferred prototype of the given constructor.
	 *
	 * @param constructor the constructor.
	 * @return the inferred prototype.
	 * @since 0.12
	 */
	public InferredPrototype getInferredPrototype(JvmConstructor constructor) {
		final XtendConstructor cons = this.associations.getXtendConstructor(constructor);
		if (cons != null) {
			return getInferredPrototype(cons);
		}
		final QualifiedActionName actionKey = this.actionPrototypes.createConstructorQualifiedName(constructor.getDeclaringType());
		// Compute the different action prototypes associated to the action to create.
		return createInferredPrototype(actionKey, constructor.isVarArgs(), constructor.getParameters());
	}

	@Override
	public Iterable<XExpression> getSideEffectExpressions(InferredPrototype calledOperation, XExpression expr) {
		final SideEffectContext ctx = new SideEffectContext(calledOperation, false);
		hasSideEffects(expr, ctx);
		return ctx.getSideEffectExpressions();
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
	protected Boolean _hasSideEffects(XAbstractWhileExpression expression, ISideEffectContext context) {
		context.open();
		try {
			if (context.isStoppingAtFirstSideEffect()) {
				if (hasSideEffects(expression.getPredicate(), context)) {
					return true;
				}
				if (hasSideEffects(expression.getBody(), context.branch())) {
					return true;
				}
				return false;
			}
			//
			final boolean r0 = hasSideEffects(expression.getPredicate(), context);
			final boolean r1 = hasSideEffects(expression.getBody(), context.branch());
			return r0 || r1;
		} finally {
			context.close();
		}
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XForLoopExpression expression, ISideEffectContext context) {
		if (context.isStoppingAtFirstSideEffect()) {
			context.open();
			try {
				if (hasSideEffects(expression.getForExpression(), context)) {
					return true;
				}
			} finally {
				context.close();
			}
			return hasSideEffects(expression.getEachExpression(), context.branch());
		}
		//
		context.open();
		boolean r0 = false;
		try {
			r0 = hasSideEffects(expression.getForExpression(), context);
		} finally {
			context.close();
		}
		return hasSideEffects(expression.getEachExpression(), context.branch()) || r0;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XIfExpression expression, ISideEffectContext context) {
		if (context.isStoppingAtFirstSideEffect()) {
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
		//
		final boolean r0 = hasSideEffects(expression.getIf(), context);
		final Map<String, List<XExpression>> buffer1 = context.createVariableAssignmentBufferForBranch();
		final boolean r1 = hasSideEffects(expression.getThen(), context.branch(buffer1));
		final Map<String, List<XExpression>> buffer2 = context.createVariableAssignmentBufferForBranch();
		final boolean r2 = hasSideEffects(expression.getElse(), context.branch(buffer2));
		context.mergeBranchVariableAssignments(Arrays.asList(buffer1, buffer2));
		return r0 || r1 || r2;
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
	@SuppressWarnings("static-method")
	protected Boolean _hasSideEffects(XThrowExpression expression, ISideEffectContext context) {
		context.registerSideEffect(expression);
		return true;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XTryCatchFinallyExpression expression, ISideEffectContext context) {
		if (context.isStoppingAtFirstSideEffect()) {
			final List<Map<String, List<XExpression>>> buffers = new ArrayList<>();
			Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
			if (hasSideEffects(expression.getExpression(), context.branch(buffer))) {
				return true;
			}
			buffers.add(buffer);
			for (final XCatchClause clause : expression.getCatchClauses()) {
				context.open();
				try {
					buffer = context.createVariableAssignmentBufferForBranch();
					if (hasSideEffects(clause.getExpression(), context.branch(buffer))) {
						return true;
					}
					buffers.add(buffer);
				} finally {
					context.close();
				}
			}
			context.mergeBranchVariableAssignments(buffers);
			if (hasSideEffects(expression.getFinallyExpression(), context)) {
				return true;
			}
			return false;
		}
		//
		final List<Map<String, List<XExpression>>> buffers = new ArrayList<>();
		Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
		final boolean r0 = hasSideEffects(expression.getExpression(), context.branch(buffer));
		buffers.add(buffer);
		boolean r1 = false;
		for (final XCatchClause clause : expression.getCatchClauses()) {
			context.open();
			try {
				buffer = context.createVariableAssignmentBufferForBranch();
				r1 = hasSideEffects(clause.getExpression(), context.branch(buffer)) || r1;
				buffers.add(buffer);
			} finally {
				context.close();
			}
		}
		context.mergeBranchVariableAssignments(buffers);
		final boolean r2 = hasSideEffects(expression.getFinallyExpression(), context);
		return r0 || r1 || r2;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XVariableDeclaration expression, ISideEffectContext context) {
		if (context.isStoppingAtFirstSideEffect()) {
			if (hasSideEffects(expression.getRight(), context)) {
				return true;
			}
			context.declareVariable(expression.getIdentifier(), expression.getRight());
			return false;
		}
		//
		final boolean r0 = hasSideEffects(expression.getRight(), context);
		context.declareVariable(expression.getIdentifier(), expression.getRight());
		return r0;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	protected Boolean _hasSideEffects(XBasicForLoopExpression expression, ISideEffectContext context) {
		context.open();
		try {
			if (context.isStoppingAtFirstSideEffect()) {
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
				return false;
			}
			//
			boolean r0 = false;
			for (final XExpression ex : expression.getInitExpressions()) {
				r0 = hasSideEffects(ex, context) || r0;
			}
			final boolean r1 = hasSideEffects(expression.getEachExpression(), context);
			boolean r2 = false;
			for (final XExpression ex : expression.getUpdateExpressions()) {
				r2 = hasSideEffects(ex, context.branch()) || r2;
			}
			final boolean r3 = hasSideEffects(expression.getExpression(), context.branch());
			return r0 || r1 || r2 || r3;
		} finally {
			context.close();
		}
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XSwitchExpression expression, ISideEffectContext context) {
		context.open();
		try {
			if (context.isStoppingAtFirstSideEffect()) {
				if (hasSideEffects(expression.getSwitch(), context)) {
					return true;
				}
				final List<Map<String, List<XExpression>>> buffers = new ArrayList<>();
				for (final XCasePart ex : expression.getCases()) {
					context.open();
					try {
						if (hasSideEffects(ex.getCase(), context)) {
							return true;
						}
						final Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
						if (hasSideEffects(ex.getThen(), context.branch(buffer))) {
							return true;
						}
						buffers.add(buffer);
					} finally {
						context.close();
					}
				}
				final Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
				if (hasSideEffects(expression.getDefault(), context.branch(buffer))) {
					return true;
				}
				buffers.add(buffer);
				context.mergeBranchVariableAssignments(buffers);
				return false;
			}
			//
			final boolean r0 = hasSideEffects(expression.getSwitch(), context);
			final List<Map<String, List<XExpression>>> buffers = new ArrayList<>();
			boolean r1 = false;
			for (final XCasePart ex : expression.getCases()) {
				context.open();
				try {
					r1 = hasSideEffects(ex.getCase(), context) || r1;
					final Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
					r1 = hasSideEffects(ex.getThen(), context.branch(buffer)) || r1;
					buffers.add(buffer);
				} finally {
					context.close();
				}
			}
			final Map<String, List<XExpression>> buffer = context.createVariableAssignmentBufferForBranch();
			final boolean r2 = hasSideEffects(expression.getDefault(), context.branch(buffer));
			buffers.add(buffer);
			context.mergeBranchVariableAssignments(buffers);
			return r0 || r1 || r2;
		} finally {
			context.close();
		}
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XCollectionLiteral expression, ISideEffectContext context) {
		context.open();
		try {
			if (context.isStoppingAtFirstSideEffect()) {
				for (final XExpression ex : expression.getElements()) {
					if (hasSideEffects(ex, context)) {
						return true;
					}
				}
				return false;
			}
			//
			boolean r0 = false;
			for (final XExpression ex : expression.getElements()) {
				r0 = hasSideEffects(ex, context) || r0;
			}
			return r0;
		} finally {
			context.close();
		}
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XConstructorCall expression, ISideEffectContext context) {
		if (context.isStoppingAtFirstSideEffect()) {
			for (final XExpression ex : expression.getArguments()) {
				if (hasSideEffects(ex, context)) {
					return true;
				}
			}
			return false;
		}
		//
		boolean r0 = false;
		for (final XExpression ex : expression.getArguments()) {
			r0 = hasSideEffects(ex, context) || r0;
		}
		return r0;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XBinaryOperation expression, ISideEffectContext context) {
		if (context.isStoppingAtFirstSideEffect()) {
			if (isReassignmentOperator(expression)) {
				if (!isLocalExpression(expression.getLeftOperand(), context, false)) {
					context.registerSideEffect(expression);
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
		//
		boolean r0 = false;
		if (isReassignmentOperator(expression)) {
			if (!isLocalExpression(expression.getLeftOperand(), context, false)) {
				context.registerSideEffect(expression);
				r0 = true;
			}
		} else {
			r0 = !(expression.isTypeLiteral() || expression.isPackageFragment());
			r0 = hasSideEffects(expression.getLeftOperand(), context) || r0;
		}
		final boolean r1 = hasSideEffects(expression.getRightOperand(), context);
		return r0 || r1;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XUnaryOperation expression, ISideEffectContext context) {
		if (context.isStoppingAtFirstSideEffect()) {
			if (expression.isTypeLiteral() || expression.isPackageFragment()) {
				return Boolean.FALSE;
			}
			if (hasSideEffects(expression.getOperand(), context)) {
				return true;
			}
			return false;
		}
		//
		final boolean r0 = !(expression.isTypeLiteral() || expression.isPackageFragment());
		final boolean r1 = hasSideEffects(expression.getOperand(), context);
		return r0 || r1;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	@SuppressWarnings("static-method")
	protected Boolean _hasSideEffects(XPostfixOperation expression, ISideEffectContext context) {
		context.registerSideEffect(expression);
		return Boolean.TRUE;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XFeatureCall expression, ISideEffectContext context) {
		return internalHasFeatureCallSideEffects(expression, context);
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XMemberFeatureCall expression, ISideEffectContext context) {
		return internalHasFeatureCallSideEffects(expression, context);
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	protected Boolean _hasSideEffects(XAssignment expression, ISideEffectContext context) {
		if (context.isStoppingAtFirstSideEffect()) {
			final JvmIdentifiableElement feature = expression.getFeature();
			if (feature instanceof XVariableDeclaration) {
				final boolean se = hasSideEffects(expression.getValue(), context);
				context.assignVariable(feature.getIdentifier(), expression.getValue());
				return se;
			}
			return true;
		}
		//
		final boolean r0 = hasSideEffects(expression.getActualReceiver(), context);
		final boolean r1 = hasSideEffects(expression.getValue(), context);
		final JvmIdentifiableElement feature = expression.getFeature();
		if (feature instanceof XVariableDeclaration) {
			context.assignVariable(feature.getIdentifier(), expression.getValue());
			return r0 || r1;
		}
		return true;
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
			try {
				if (context.isStoppingAtFirstSideEffect()) {
					for (final XExpression ex : exprs) {
						if (hasSideEffects(ex, context)) {
							return true;
						}
					}
					return false;
				}
				//
				boolean r0 = false;
				for (final XExpression ex : exprs) {
					r0 = hasSideEffects(ex, context) || r0;
				}
				return r0;
			} finally {
				context.close();
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
	@SuppressWarnings("static-method")
	protected Boolean _hasSideEffects(SarlAssertExpression expression, ISideEffectContext context) {
		// Assume => expression.isStatic === true
		return Boolean.FALSE;
	}

	/** Test if the given expression has side effects.
	 *
	 * @param expression the expression.
	 * @param context the list of context expressions.
	 * @return {@code true} if the expression has side effects.
	 */
	@SuppressWarnings("static-method")
	protected Boolean _hasSideEffects(SarlBreakExpression expression, ISideEffectContext context) {
		return Boolean.FALSE;
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
		return Boolean.FALSE;
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
	private boolean internalHasFeatureCallSideEffects(XAbstractFeatureCall expression, ISideEffectContext context) {
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
				return false;
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
			return internalHasOperationSideEffects(expression, operation, context);
		} else if (isExternalFeature(feature)) {
			return internalHasExternalFeatureSideEffects(expression, context, feature);
		}
		return false;
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private boolean internalHasOperationSideEffects(XAbstractFeatureCall originalExpression,
			JvmOperation operation, ISideEffectContext context) {
		// Test if the receiver has side effects
		boolean hasEffectReceiver = false;
		if (hasSideEffects(originalExpression.getActualReceiver(), context)) {
			if (context.isStoppingAtFirstSideEffect()) {
				return true;
			}
			hasEffectReceiver = true;
		}
		// Test the feature call itself
		final ISideEffectContext ctx = new SideEffectContext(
				Iterables.concat(context.getCalledOperations(),
						Collections.singleton(getInferredPrototype(operation))));

		boolean isPureOperation = this.annotations.findAnnotation(operation, Pure.class) != null
				|| evaluatePureAnnotationAdapters(operation, ctx);

		if (this.nameValidator.isNamePatternForNotPureOperation(operation)) {
			isPureOperation = false;
		} else if (this.nameValidator.isNamePatternForPureOperation(operation)) {
			isPureOperation = true;
		}

		/*if (isPureOperation) {
			hasEffectReceiver = !isLocalExpression(originalExpression.getActualReceiver(), context, true) || hasEffectReceiver;
		}*/

		if (!isPureOperation) {
			context.registerSideEffect(originalExpression);
		}

		boolean hasEffectArgument = false;
		if (!context.isStoppingAtFirstSideEffect() || isPureOperation) {
			for (final XExpression ex : originalExpression.getActualArguments()) {
				final Boolean bool = hasSideEffects(ex, context);
				if (bool != null && bool.booleanValue()) {
					if (context.isStoppingAtFirstSideEffect()) {
						return true;
					}
					hasEffectArgument = false;
				}
			}
		}

		return hasEffectReceiver || !isPureOperation || hasEffectArgument;
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private boolean internalHasExternalFeatureSideEffects(XAbstractFeatureCall expression, ISideEffectContext context, JvmIdentifiableElement feature) {
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

		private final boolean stopFirstSideEffect;

		private final List<InferredPrototype> calledOperations = new ArrayList<>();

		private final Deque<InternalContext> contextStack;

		private final boolean isBranchContext;

		private final Map<String, List<XExpression>> variableAssignmentBuffer;

		private final List<XExpression> sideEffectExpressions = new ArrayList<>();

		/** Constructor.
		 *
		 * @param calledOperation the called operation.
		 */
		SideEffectContext(InferredPrototype calledOperation) {
			this(calledOperation, true);
		}

		/** Constructor.
		 *
		 * @param calledOperation the called operation.
		 * @param stopFirst indicates if the side-effect exploration stops at first side-effect.
		 */
		SideEffectContext(InferredPrototype calledOperation, boolean stopFirst) {
			this.stopFirstSideEffect = stopFirst;
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
			this.stopFirstSideEffect = true;
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
			this.stopFirstSideEffect = context.isStoppingAtFirstSideEffect();
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
				Map<String, List<XExpression>> buffer, boolean stopFirst) {
			this.stopFirstSideEffect = stopFirst;
			if (calledOperations != null) {
				this.calledOperations.addAll(calledOperations);
			}
			this.variableAssignmentBuffer = buffer;
			this.isBranchContext = true;
			this.contextStack = contextStack;
		}

		@Pure
		@Override
		public boolean isStoppingAtFirstSideEffect() {
			return this.stopFirstSideEffect;
		}

		@Override
		public void registerSideEffect(XExpression expression) {
			assert expression != null;
			this.sideEffectExpressions.add(expression);
		}

		@Override
		public Iterable<XExpression> getSideEffectExpressions() {
			return Collections.unmodifiableList(this.sideEffectExpressions);
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
			return new SideEffectContext(getCalledOperations(), this.contextStack, buffer, this.stopFirstSideEffect);
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

		@Override
		public Iterable<XExpression> getSideEffectExpressions(InferredPrototype calledOperation, XExpression expr) {
			return this.delegate.getSideEffectExpressions(calledOperation, expr);
		}

	}

}
