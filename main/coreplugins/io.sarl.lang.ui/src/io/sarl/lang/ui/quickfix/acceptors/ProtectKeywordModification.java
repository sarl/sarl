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
import java.util.regex.Pattern;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.text.correction.IProposalRelevance;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Add the ^ character before a keywword.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class ProtectKeywordModification extends SARLSemanticModification {

	private final String oldKeyword;

	private final String newKeyword;

	private ProtectKeywordModification(String oldKeyword, String newKeyword) {
		this.oldKeyword = oldKeyword;
		this.newKeyword = newKeyword;
	}

	/** Create the quick fix if needed.
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		final String[] data = issue.getData();
		if (data != null && data.length > 1 && !Strings.isEmpty(data[0]) && !Strings.isEmpty(data[1])) {
			final ProtectKeywordModification modification = new ProtectKeywordModification(data[0], data[1]);
			modification.setIssue(issue);
			modification.setTools(provider);
			acceptor.accept(issue,
					MessageFormat.format(Messages.ProtectKeywordModification_0, data[1]),
					MessageFormat.format(Messages.ProtectKeywordModification_1, data[1]),
					JavaPluginImages.IMG_CORRECTION_RENAME,
					modification,
					IProposalRelevance.RENAME_REFACTORING_QUICK_FIX);
		}
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		final IXtextDocument document = context.getXtextDocument();
		final Issue issue = getIssue();
		final int keywordOffset = getTools().getOffsetForPattern(document, issue.getOffset(),
				".?" + Pattern.quote(this.oldKeyword)); //$NON-NLS-1$
		if (keywordOffset >= issue.getOffset()) {
			context.getXtextDocument().replace(keywordOffset - this.oldKeyword.length(),
					this.oldKeyword.length(), this.newKeyword);
		}
	}

}
