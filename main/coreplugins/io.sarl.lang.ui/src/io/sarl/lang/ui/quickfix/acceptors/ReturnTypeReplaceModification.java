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
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Replace the return type of an action.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class ReturnTypeReplaceModification extends SARLSemanticModification {

	private final String expectedType;

	private ReturnTypeReplaceModification(String expectedType) {
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
			final ReturnTypeReplaceModification modification = new ReturnTypeReplaceModification(expectedType);
			modification.setIssue(issue);
			modification.setTools(provider);
			acceptor.accept(issue,
					MessageFormat.format(Messages.SARLQuickfixProvider_15, expectedType),
					Messages.SARLQuickfixProvider_16,
					JavaPluginImages.IMG_CORRECTION_CHANGE,
					modification,
					IProposalRelevance.CHANGE_RETURN_TYPE);
		}
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		final IXtextDocument document = context.getXtextDocument();
		document.replace(getIssue().getOffset(), getIssue().getLength(), this.expectedType);
	}

}
