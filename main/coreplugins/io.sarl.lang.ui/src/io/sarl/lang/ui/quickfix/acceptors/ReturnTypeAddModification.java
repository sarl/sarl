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

package io.sarl.lang.ui.quickfix.acceptors;

import java.text.MessageFormat;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.text.correction.IProposalRelevance;
import org.eclipse.xtend.core.xtend.XtendExecutable;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Replace the return type of an action.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class ReturnTypeAddModification extends SARLSemanticModification {

	private final String expectedType;

	private ReturnTypeAddModification(String expectedType) {
		this.expectedType = expectedType;
	}

	/** Create the quick fix if needed.
	 *
	 * <p>User data contains the name of the expected type.
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		final String[] data = issue.getData();
		if (data != null && data.length > 0) {
			final String expectedType = data[0];
			final ReturnTypeAddModification modification = new ReturnTypeAddModification(expectedType);
			modification.setIssue(issue);
			modification.setTools(provider);
			acceptor.accept(issue,
					MessageFormat.format(Messages.SARLQuickfixProvider_13, expectedType),
					Messages.SARLQuickfixProvider_14,
					JavaPluginImages.IMG_CORRECTION_ADD,
					modification,
					IProposalRelevance.CHANGE_RETURN_TYPE);
		}
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		final XtendExecutable xtendExecutable = EcoreUtil2.getContainerOfType(element, XtendExecutable.class);
		final int insertPosition;
		if (xtendExecutable.getExpression() == null) {
			final ICompositeNode functionNode = NodeModelUtils.findActualNodeFor(xtendExecutable);
			if (functionNode == null) {
				throw new IllegalStateException("functionNode may not be null"); //$NON-NLS-1$
			}
			insertPosition = functionNode.getEndOffset();
		} else {
			final ICompositeNode expressionNode = NodeModelUtils.findActualNodeFor(xtendExecutable.getExpression());
			if (expressionNode == null) {
				throw new IllegalStateException("expressionNode may not be null"); //$NON-NLS-1$
			}
			insertPosition = expressionNode.getOffset();
		}
		final ReplacingAppendable appendable = getTools().getAppendableFactory().create(context.getXtextDocument(),
				(XtextResource) xtendExecutable.eResource(), insertPosition, 0);
		if (xtendExecutable.getExpression() == null) {
			appendable.append(" "); //$NON-NLS-1$
		}
		appendable.append(getTools().getGrammarAccess().getColonKeyword());
		appendable.append(" "); //$NON-NLS-1$
		appendable.append(this.expectedType);
		if (xtendExecutable.getExpression() != null) {
			appendable.append(" "); //$NON-NLS-1$
		}
		appendable.commitChanges();
	}

}
