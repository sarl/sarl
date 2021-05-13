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

package io.sarl.lang.ui.codemining;

import java.text.MessageFormat;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Predicate;

import javax.inject.Inject;

import com.google.common.base.Supplier;
import com.google.common.base.Throwables;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.codemining.ICodeMining;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendFunction;
import org.eclipse.xtend.core.xtend.XtendVariableDeclaration;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.RuleCall;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.codemining.AbstractXtextCodeMiningProvider;
import org.eclipse.xtext.ui.editor.model.XtextDocumentUtil;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.util.concurrent.CancelableUnitOfWork;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.typesystem.util.CommonTypeComputationServices;

import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.ui.SARLUiPlugin;
import io.sarl.lang.util.Utils;

/** Provider of a code mining support for SARL.
 *
 * <p>Code Mining shows inline annotations in the text editor that are not part of the text
 * itself, but derived from its contents. It can be very helpful to leverage code
 * minings for example to show inferred types, parameter names for literals and
 * other kind of meta information.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 * @see "https://blogs.itemis.com/en/code-mining-support-in-xtext"
 */
public class SARLCodeMiningProvider extends AbstractXtextCodeMiningProvider {

	@Inject
	private JvmModelAssociator jvmModelAssocitions;

	@Inject
	private SARLGrammarAccess grammar;

	@Inject
	private SARLGrammarKeywordAccess keywords;

	@Inject
	private SARLCodeminingPreferenceAccess codeminingPreferences;

	@Inject
	private IBatchTypeResolver typeResolver;

	@Inject
	private XtextDocumentUtil xtextDocumentUtil;
	
	@Inject
	private CommonTypeComputationServices services;

	private final PolymorphicDispatcher<Object> codeminingDispatcher;

	/** Constructor.
	 */
	public SARLCodeMiningProvider() {
		this.codeminingDispatcher = PolymorphicDispatcher.createForSingleTarget("_codemining", 2, 2, this);
	}

	@Override
	public CompletableFuture<List<? extends ICodeMining>> provideCodeMinings(ITextViewer viewer, IProgressMonitor monitor) {
		// Only for fixing bug 1041: exception catching around the code mining building in order to show up an error message
		if (this.codeminingPreferences.isCodeminingEnabled()) {
			final Supplier<List<? extends ICodeMining>> task = () -> {
				final CancelableUnitOfWork<List<ICodeMining>, XtextResource> uow = new CancelableUnitOfWork<List<ICodeMining>, XtextResource>() {
					@Override
					public List<ICodeMining> exec(XtextResource resource, CancelIndicator uowCancelIndicator) throws Exception {
						final CombinedCancelIndicator indicator = new CombinedCancelIndicator(monitor, uowCancelIndicator);
						return createCodeMinings(viewer.getDocument(), resource, indicator);
					}
				};
				final Supplier<List<ICodeMining>> defaultList = () -> Collections.emptyList();
				try {
					return this.xtextDocumentUtil.getXtextDocument(viewer).tryReadOnly(uow, defaultList);
				} catch (Throwable exception) {
					disableCodeMining(viewer, exception);
					return defaultList.get();
				}
			};
			final CompletableFuture<List<? extends ICodeMining>> future = CompletableFuture.supplyAsync(task);
			return future;
		}
		return null;
	}

	private void disableCodeMining(ITextViewer viewer, Throwable error) {
		this.codeminingPreferences.setCodeminingEnabled(false);
		final Throwable rootCause = Throwables.getRootCause(error);
		String message = Messages.SARLCodeMiningProvider_2;
		if (rootCause != null) {
			final String msg = rootCause.getLocalizedMessage();
			if (!Strings.isEmpty(msg)) {
				message = msg;
			}
		}
		final String errorMessage = message;
		Display.getDefault().asyncExec(() -> {
			final Shell shell = viewer.getTextWidget().getShell();
			SARLUiPlugin.openError(shell, Messages.SARLCodeMiningProvider_0,
					MessageFormat.format(Messages.SARLCodeMiningProvider_1, errorMessage),
					errorMessage, rootCause);
		});
	}

	/** Root dispatch function for code mining.
	 * 
	 * @param element the element to mine.
	 * @param acceptor the code mining receiver.
	 */
	protected void _codemining(EObject element, IAcceptor<? super ICodeMining> acceptor) {
		//
	}

	/** Add an annotation when the action's return type is implicit and inferred by the SARL compiler.
	 *
	 * @param action the action to mine.
	 * @param acceptor the code mining acceptor.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	protected void _codemining(XtendFunction action, IAcceptor<? super ICodeMining> acceptor) {
		if (this.codeminingPreferences.isCodeminingActionReturnTypeEnabled()) {
			// inline annotation only for methods with no return type
			if (action.getReturnType() != null) {
				return;
			}
			// get return type name from the generated JVM operation itself
			final JvmOperation inferredOperation = (JvmOperation) this.jvmModelAssocitions.getPrimaryJvmElement(action);
			if (inferredOperation == null) {
				return;
			}
			final JvmTypeReference returnType = inferredOperation.getReturnType();
			if (returnType == null) {
				return;
			}
			// find document offset for inline annotation
			final Keyword parenthesis = this.grammar.getAOPMemberAccess().getRightParenthesisKeyword_2_5_6_2();
			final Assignment actionName = this.grammar.getAOPMemberAccess().getNameAssignment_2_5_5();
			final CodeRegion region = findNode(action,
					candidate -> candidate instanceof RuleCall && EcoreUtil.equals(actionName, candidate.eContainer()),
					candidate -> EcoreUtil.equals(parenthesis, candidate));
			int offset = -1;
			if (region.end != null) {
				offset = region.end.getTotalEndOffset();
			} else if (region.start != null) {
				offset = region.start.getTotalEndOffset();
			}
			if (offset >= 0) {
				final String returnTypeName = Utils.toLightweightTypeReference(returnType, this.services).getHumanReadableName();
				final String text = " " + this.keywords.getColonKeyword() + " " + returnTypeName; //$NON-NLS-1$ //$NON-NLS-2$
				acceptor.accept(createNewLineContentCodeMining(offset, text));
			}
		}
	}

	/** Add an annotation when the field's type is implicit and inferred by the SARL compiler.
	 *
	 * @param field the field to mine.
	 * @param acceptor the code mining acceptor.
	 */
	protected void _codemining(XtendField field, IAcceptor<? super ICodeMining> acceptor) {
		if (this.codeminingPreferences.isCodeminingFieldTypeEnabled()) {
			createImplicitVarValType(field, acceptor, XtendField.class,
				it -> it.getType(),
				it -> {
					final JvmField inferredField = (JvmField) this.jvmModelAssocitions.getPrimaryJvmElement(it);
					if (inferredField == null || inferredField.getType() == null || inferredField.getType().eIsProxy()) {
						return null;
					}
					return inferredField.getType().getSimpleName();
				},
				null,
				() -> this.grammar.getAOPMemberAccess().getInitialValueAssignment_2_3_3_1());
		}
	}

	/** Add an annotation when the variable's type is implicit and inferred by the SARL compiler.
	 *
	 * @param variable the variable to mine.
	 * @param acceptor the code mining acceptor.
	 */
	protected void _codemining(XtendVariableDeclaration variable, IAcceptor<? super ICodeMining> acceptor) {
		if (this.codeminingPreferences.isCodeminingFieldTypeEnabled()) {
			createImplicitVarValType(variable, acceptor, XtendVariableDeclaration.class,
					it -> it.getType(),
					it -> {
						LightweightTypeReference type = getLightweightType(it.getRight());
						if (type.isAny()) {
							type = getTypeForVariableDeclaration(it.getRight());
						}
						return type.getSimpleName();
					},
					it -> it.getRight(),
					() -> this.grammar.getXVariableDeclarationAccess().getRightAssignment_3_1());
		}
	}

	/** Add an annotation for the formal argument names.
	 *
	 * @param featureCall the feature call..
	 * @param acceptor the code mining acceptor.
	 */
	protected void _codemining(XAbstractFeatureCall featureCall, IAcceptor<? super ICodeMining> acceptor) {
		createImplicitArgumentName();
	}

	/** Add an annotation for the formal argument names.
	 *
	 * @param featureCall the feature call..
	 * @param acceptor the code mining acceptor.
	 */
	protected void _codemining(XConstructorCall featureCall, IAcceptor<? super ICodeMining> acceptor) {
		createImplicitArgumentName();
	}

	private void createImplicitArgumentName() {
		if (this.codeminingPreferences.isCodeminingFeatureCallArgumentNameEnabled()) {			
		}
	}

	@Override
	protected void createCodeMinings(IDocument document, XtextResource resource, CancelIndicator indicator,
			IAcceptor<? super ICodeMining> acceptor) throws BadLocationException {
		final TreeIterator<EObject> iterator = EcoreUtil2.eAll(resource.getContents().get(0));
		while (iterator.hasNext() && !isCanceled(indicator)) {
			final EObject obj = iterator.next();
			this.codeminingDispatcher.invoke(obj, acceptor);
		}
	}

	/** Add an annotation when the var/val declaration's type is implicit and inferred by the SARL compiler.
	 */
	@SuppressWarnings({ "checkstyle:npathcomplexity" })
	private <T extends EObject> void createImplicitVarValType(T element,IAcceptor<? super ICodeMining> acceptor,
			Class<T> elementType,
			Function1<T, JvmTypeReference> declaredTypeLambda,
			Function1<T, String> inferredTypeLambda,
			Function1<T, XExpression> exprLambda,
			Function0<Assignment> assignmentLambda) {
		// inline annotation only for fields with no type
		final JvmTypeReference declaredType = declaredTypeLambda.apply(element);
		final XExpression expr = exprLambda != null ? exprLambda.apply(element) : null;
		if (declaredType != null || (exprLambda != null && expr == null)) {
			return;
		}
		// get inferred type name from JVM element
		final String inferredType = inferredTypeLambda.apply(element);
		if (Strings.isEmpty(inferredType)) {
			return;
		}
		// find document offset for inline annotation
		final Assignment reference = assignmentLambda.apply(); 
		final INode node = findNode(element,
				candidate -> candidate instanceof RuleCall && EcoreUtil.equals(reference, candidate.eContainer()));
		if (node != null) {
			final String text = this.keywords.getColonKeyword() + " " + inferredType + " "; //$NON-NLS-1$ //$NON-NLS-2$
			final int offset = node.getPreviousSibling().getTotalOffset();
			acceptor.accept(createNewLineContentCodeMining(offset, text));
		}
	}

	/** Find the grammar node for the given element.
	 *
	 * @param element is the element for which the grammar node should be find.
	 * @param candidatValidator is the lambda to use for validating a grammar candidate.
	 * @return the node or {@code null}.
	 * @since 0.12
	 */
	protected INode findNode(EObject element, Predicate<EObject> reference) {
		assert element != null;
		assert reference != null;
		final ICompositeNode node = NodeModelUtils.findActualNodeFor(element);
		//System.err.println(NodeModelUtils.compactDump(node, false));
		for (Iterator<INode> it = node.getAsTreeIterable().iterator(); it.hasNext();) {
			final INode child = it.next();
			if (child != node) {
				//System.err.println(NodeModelUtils.compactDump(child, false));
				final EObject grammarElement = child.getGrammarElement();
				if (reference.test(grammarElement)) {
					return child;
				}
			}
		}
		return null;
	}

	/** Find the grammar node for the given element.
	 *
	 * @param element is the element for which the grammar node should be find.
	 * @param grammarBeginAnchor the begin anchor to search for into the source code.
	 * @param grammarEndAnchor the end anchor to search for into the source code.
	 * @return the region.
	 * @since 0.12
	 */
	protected CodeRegion findNode(EObject element, Predicate<EObject> grammarBeginAnchor, Predicate<EObject> grammarEndAnchor) {
		final ICompositeNode node = NodeModelUtils.findActualNodeFor(element);
		assert grammarBeginAnchor != null;
		assert grammarEndAnchor != null;
		INode start = null;
		for (Iterator<INode> it = node.getAsTreeIterable().iterator(); it.hasNext();) {
			final INode child = it.next();
			if (child != node) {
				final EObject grammarElement = child.getGrammarElement();
				if (grammarBeginAnchor.test(grammarElement)) {
					start = child;
				} else if (grammarEndAnchor.test(grammarElement)) {
					return new CodeRegion(start, child);
				}
			}
		}
		return new CodeRegion(start, null);
	}

	private static boolean isCanceled(CancelIndicator indicator) {
		try {
			return indicator.isCanceled();
		} catch (Throwable exception) {
			//
		}
		return true;
	}

	/** Replies the resolved types associated to the given object.
	 *
	 * @param obj the object.
	 * @return the types.
	 */
	protected IResolvedTypes getResolvedTypes(EObject obj) {
		return this.typeResolver.resolveTypes(obj);
	}

	/** Replies the inferred type for the given expression.
	 *
	 * @param expr the expression.
	 * @return the type of the expression.
	 */
	protected LightweightTypeReference getLightweightType(XExpression expr) {
		final IResolvedTypes resolvedTypes = getResolvedTypes(expr);
		final LightweightTypeReference expressionType = resolvedTypes.getActualType(expr);
		if (expr instanceof AnonymousClass) {
			final List<LightweightTypeReference> superTypes = expressionType.getSuperTypes();
			if (superTypes.size() == 1) {
				return superTypes.get(0);
			}
		}
		return expressionType;
	}

	/** Replies the type for a variable declaration that is initialized with the given expression.
	 *
	 * @param expr the expression.
	 * @return the variable type.
	 */
	protected LightweightTypeReference getTypeForVariableDeclaration(XExpression expr) {
		final IResolvedTypes resolvedTypes = getResolvedTypes(expr);
		LightweightTypeReference actualType = resolvedTypes.getActualType(expr);
		if (actualType.isPrimitiveVoid()) {
			LightweightTypeReference expectedType = resolvedTypes.getExpectedType(expr);
			if (expectedType == null) {
				expectedType = resolvedTypes.getExpectedReturnType(expr);
				if (expectedType == null) {
					expectedType = resolvedTypes.getReturnType(expr);
				}
			}
			if (expectedType != null && !expectedType.isPrimitiveVoid()) {
				actualType = expectedType;
			}
		}
		return actualType;
	}

	/** This indicator checks if monitor is canceled or if
	 * CancelableUnitOfWork-cancelIndicator is canceled.
	 * <strong>Only for fixing bug 1041.</strong>
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	private static class CombinedCancelIndicator implements CancelIndicator {

		private IProgressMonitor monitor;

		private CancelIndicator uowCancelIndicator;

		CombinedCancelIndicator(IProgressMonitor monitor, CancelIndicator uowCancelIndicator) {
			this.monitor = monitor;
			this.uowCancelIndicator = uowCancelIndicator;
		}

		@Override
		public boolean isCanceled() {
			// uowCancelIndicator.isCanceled() will return true, when the resource was changed
			// thanks to CancelableUnitOfWork
			return (this.uowCancelIndicator != null && this.uowCancelIndicator.isCanceled())
					// monitor.isCanceled() throws an CancellationException when monitor is canceled
					|| (this.monitor != null && this.monitor.isCanceled());
		}

	}

	/** Region of the code
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.12
	 */
	protected static class CodeRegion {

		/** Start node.
		 */
		public final INode start;

		/** End node.
		 */
		public final INode end;

		/** Constructor.
		 *
		 * @param start the start node.
		 * @param end the end node.
		 */
		protected CodeRegion(INode start, INode end) {
			this.start = start;
			this.end = end;
		}

	}

}
