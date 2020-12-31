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
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Add an action.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class ActionAddModification extends SARLSemanticModification {

	private final String actionName;

	/** Constructor.
	 * @param actionName the name of the action.
	 */
	private ActionAddModification(String actionName) {
		this.actionName = actionName;
	}

	/** Create the quick fix if needed.
	 *
	 * <p>The user data contains the name of the container type, and the name of the new action.
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		final String[] data = issue.getData();
		if (data != null && data.length > 1) {
			final String actionName = data[1];
			final ActionAddModification modification = new ActionAddModification(actionName);
			modification.setIssue(issue);
			modification.setTools(provider);
			acceptor.accept(issue,
					MessageFormat.format(Messages.SARLQuickfixProvider_2, actionName),
					MessageFormat.format(Messages.SARLQuickfixProvider_3, actionName),
					JavaPluginImages.IMG_CORRECTION_ADD,
					modification,
					IProposalRelevance.ADD_UNIMPLEMENTED_METHODS);
		}
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		final XtendTypeDeclaration container = EcoreUtil2.getContainerOfType(element, XtendTypeDeclaration.class);
		if (container != null) {
			final int insertOffset = getTools().getInsertOffset(container);
			final IXtextDocument document = context.getXtextDocument();
			final int length = getTools().getSpaceSize(document, insertOffset);
			final ReplacingAppendable appendable = getTools().getAppendableFactory().create(document,
					(XtextResource) element.eResource(), insertOffset, length);
			final boolean changeIndentation = container.getMembers().isEmpty();
			if (changeIndentation) {
				appendable.increaseIndentation();
			}
			appendable.newLine();
			appendable.append(
					getTools().getGrammarAccess().getDefKeyword());
			appendable.append(" "); //$NON-NLS-1$
			appendable.append(this.actionName);
			if (changeIndentation) {
				appendable.decreaseIndentation();
			}
			appendable.newLine();
			appendable.commitChanges();
		}
	}

}
