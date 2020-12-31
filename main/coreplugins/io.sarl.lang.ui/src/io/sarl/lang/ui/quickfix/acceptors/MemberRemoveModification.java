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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.text.correction.IProposalRelevance;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Remove a member.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class MemberRemoveModification extends SARLSemanticModification {

	private final Class<? extends XtendMember> type;

	/** Constructor.
	 * @param type the type of the element to remove.
	 */
	private MemberRemoveModification(Class<? extends XtendMember> type) {
		this.type = type;
	}

	/** Construct a modification description.
	 */
	public MemberRemoveModification() {
		this(XtendMember.class);
	}

	/** Create the quick fix if needed.
	 *
	 * <p>No user data.
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 * @param type the type of the element to remove.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor,
			Class<? extends XtendMember> type) {
		final MemberRemoveModification modification = new MemberRemoveModification(type);
		modification.setIssue(issue);
		modification.setTools(provider);
		acceptor.accept(issue,
				Messages.SARLQuickfixProvider_0,
				Messages.SARLQuickfixProvider_10,
				JavaPluginImages.IMG_CORRECTION_REMOVE,
				modification,
				IProposalRelevance.REMOVE_METHOD_BODY);
	}

	/** Create the quick fix if needed.
	 *
	 * <p>No user data.
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		final MemberRemoveModification modification = new MemberRemoveModification();
		modification.setIssue(issue);
		modification.setTools(provider);
		acceptor.accept(issue,
				Messages.SARLQuickfixProvider_0,
				Messages.SARLQuickfixProvider_10,
				null,
				modification);
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		getTools().remove(element, this.type, context);
	}

}
