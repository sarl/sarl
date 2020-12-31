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

import com.google.common.base.Strings;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.text.correction.IProposalRelevance;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Remove an extended type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class ExtendedTypeRemoveModification extends SARLSemanticModification {

	private final RemovalType type;

	/** Constructor.
	 * @param type the type of removal.
	 */
	private ExtendedTypeRemoveModification(RemovalType type) {
		this.type = type;
	}

	/** Create the quick fix if needed.
	 *
	 * <p>The first user data is the name of the type to remove.
	 * The second parameter may be the type of the removal (see {@link RemovalType}).
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		accept(provider, issue, acceptor, null);
	}

	/** Create the quick fix if needed.
	 *
	 * <p>The first user data is the name of the type to remove.
	 * The second parameter may be the type of the removal (see {@link RemovalType}).
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 * @param type the type of the modification.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor, RemovalType type) {
		final String[] data = issue.getData();
		RemovalType removalType = type;
		String redundantName = null;
		if (data != null && data.length >= 1) {
			redundantName = data[0];
			if (removalType == null && data.length >= 2) {
				final String mode = data[1];
				if (!Strings.isNullOrEmpty(mode)) {
					try {
						removalType = RemovalType.valueOf(mode.toUpperCase());
					} catch (Throwable exception) {
						//
					}
				}
			}
		}
		if (removalType == null) {
			removalType = RemovalType.OTHER;
		}
		final String msg;
		if (Strings.isNullOrEmpty(redundantName)) {
			msg = Messages.SARLQuickfixProvider_0;
		} else {
			msg = MessageFormat.format(Messages.SARLQuickfixProvider_6, redundantName);
		}

		final ExtendedTypeRemoveModification modification = new ExtendedTypeRemoveModification(removalType);
		modification.setIssue(issue);
		modification.setTools(provider);
		acceptor.accept(issue,
				msg,
				Messages.SARLQuickfixProvider_7,
				JavaPluginImages.IMG_CORRECTION_REMOVE,
				modification,
				IProposalRelevance.REMOVE_REDUNDANT_SUPER_INTERFACE);
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		final IXtextDocument document = context.getXtextDocument();
		final SARLQuickfixProvider tools = getTools();
		final Issue issue = getIssue();
		final String sep = tools.getGrammarAccess().getCommaKeyword();
		switch (this.type) {
		case PRE:
			tools.removeToPreviousSeparator(issue, document, sep);
			break;
		case POST:
			tools.removeToNextSeparator(issue, document, sep);
			break;
		case OTHER:
		default:
			if (!tools.removeToPreviousSeparator(issue, document, sep)) {
				if (!tools.removeToNextSeparator(issue, document, sep)) {
					tools.removeToPreviousKeyword(issue, document,
							tools.getGrammarAccess().getExtendsKeyword());
				}
			}
		}
	}

	/** Type of the removal of an extended type.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public enum RemovalType {
		/** Remove to the previous separator.
		 */
		PRE,

		/** Remove to the next separator.
		 */
		POST,

		/** General type of removal.
		 */
		OTHER
	}

}
