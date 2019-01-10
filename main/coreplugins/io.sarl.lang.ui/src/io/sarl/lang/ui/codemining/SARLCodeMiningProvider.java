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

package io.sarl.lang.ui.codemining;

import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import javax.inject.Inject;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.codemining.ICodeMining;
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
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.jvmmodel.JvmModelAssociator;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.services.SARLGrammarAccess;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

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

	@Override
	public CompletableFuture<List<? extends ICodeMining>> provideCodeMinings(ITextViewer viewer, IProgressMonitor monitor) {
		if (this.codeminingPreferences.isCodeminingEnabled()) {
			return super.provideCodeMinings(viewer, monitor);
		}
		return null;
	}

	@Override
	protected void createCodeMinings(IDocument document, XtextResource resource, CancelIndicator indicator,
			IAcceptor<? super ICodeMining> acceptor) throws BadLocationException {
		createImplicitActionReturnType(resource, acceptor);
		createImplicitFieldType(resource, acceptor);
		createImplicitVariableType(resource, acceptor);
	}

	/** Add an annotation when the var/val declaration's type is implicit and inferred by the SARL compiler.
	 *
	 * @param resource the resource to parse.
	 * @param acceptor the code mining acceptor.
	 */
	@SuppressWarnings({ "checkstyle:npathcomplexity" })
	private <T extends EObject> void createImplicitVarValType(XtextResource resource, IAcceptor<? super ICodeMining> acceptor,
			Class<T> elementType,
			Function1<T, JvmTypeReference> getDeclaredTypeLambda,
			Function1<T, String> getInferredTypeLambda,
			Function1<T, XExpression> getExprLambda,
			Function0<Assignment> getAssignmentLambda) {
		final List<T> elements = EcoreUtil2.eAllOfType(resource.getContents().get(0), elementType);

		for (final T element : elements) {
			// inline annotation only for fields with no type
			final JvmTypeReference declaredType = getDeclaredTypeLambda.apply(element);
			final XExpression expr = getExprLambda != null ? getExprLambda.apply(element) : null;
			if (declaredType != null || (getExprLambda != null && expr == null)) {
				continue;
			}
			// get inferred type name from JVM element
			final String inferredType = getInferredTypeLambda.apply(element);
			if (Strings.isEmpty(inferredType)) {
				continue;
			}
			// find document offset for inline annotation
			final ICompositeNode node = NodeModelUtils.findActualNodeFor(element);
			final Assignment elementAssignment = getAssignmentLambda.apply();
			assert elementAssignment != null;
			for (Iterator<INode> it = node.getAsTreeIterable().iterator(); it.hasNext();) {
				final INode child = it.next();
				if (child != node) {
					final EObject grammarElement = child.getGrammarElement();
					if (grammarElement instanceof RuleCall) {
						if (elementAssignment.equals(grammarElement.eContainer())) {
							final String text = this.keywords.getColonKeyword() + " " + inferredType + " "; //$NON-NLS-1$ //$NON-NLS-2$
							final int offset = child.getPreviousSibling().getTotalOffset();
							acceptor.accept(createNewLineContentCodeMining(offset, text));
							break;
						}
					}
				}
			}
		}
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

	/** Add an annotation when the variable's type is implicit and inferred by the SARL compiler.
	 *
	 * @param resource the resource to parse.
	 * @param acceptor the code mining acceptor.
	 */
	private void createImplicitVariableType(XtextResource resource, IAcceptor<? super ICodeMining> acceptor) {
		createImplicitVarValType(resource, acceptor, XtendVariableDeclaration.class,
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

	/** Add an annotation when the field's type is implicit and inferred by the SARL compiler.
	 *
	 * @param resource the resource to parse.
	 * @param acceptor the code mining acceptor.
	 */
	private void createImplicitFieldType(XtextResource resource, IAcceptor<? super ICodeMining> acceptor) {
		createImplicitVarValType(resource, acceptor, XtendField.class,
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

	/** Add an annotation when the action's return type is implicit and inferred by the SARL compiler.
	 *
	 * @param resource the resource to parse.
	 * @param acceptor the code mining acceptor.
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	private void createImplicitActionReturnType(XtextResource resource, IAcceptor<? super ICodeMining> acceptor) {
		final List<XtendFunction> actions = EcoreUtil2.eAllOfType(resource.getContents().get(0), XtendFunction.class);

		for (final XtendFunction action : actions) {
			// inline annotation only for methods with no return type
			if (action.getReturnType() != null) {
				continue;
			}
			// get return type name from operation
			final JvmOperation inferredOperation = (JvmOperation) this.jvmModelAssocitions.getPrimaryJvmElement(action);
			if (inferredOperation == null || inferredOperation.getReturnType() == null) {
				continue;
			}
			// find document offset for inline annotationn
			final ICompositeNode node = NodeModelUtils.findActualNodeFor(action);
			final Keyword parenthesis = this.grammar.getAOPMemberAccess().getRightParenthesisKeyword_2_5_6_2();
			final Assignment fctname = this.grammar.getAOPMemberAccess().getNameAssignment_2_5_5();
			int offsetFctname = -1;
			int offsetParenthesis = -1;
			for (Iterator<INode> it = node.getAsTreeIterable().iterator(); it.hasNext();) {
				final INode child = it.next();
				if (child != node) {
					final EObject grammarElement = child.getGrammarElement();
					if (grammarElement instanceof RuleCall) {
						if (fctname.equals(grammarElement.eContainer())) {
							offsetFctname = child.getTotalEndOffset();
						}
					} else if (parenthesis.equals(grammarElement)) {
						offsetParenthesis = child.getTotalEndOffset();
						break;
					}
				}
			}
			int offset = -1;
			if (offsetParenthesis >= 0) {
				offset = offsetParenthesis;
			} else if (offsetFctname >= 0) {
				offset = offsetFctname;
			}
			if (offset >= 0) {
				final String returnType = inferredOperation.getReturnType().getSimpleName();
				final String text = " " + this.keywords.getColonKeyword() + " " + returnType; //$NON-NLS-1$ //$NON-NLS-2$
				acceptor.accept(createNewLineContentCodeMining(offset, text));
			}
		}
	}

}
