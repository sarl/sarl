/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.annotations.xAnnotations.XAnnotation;

import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

/**
 * Remove an annotation.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.lang.ui 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 */
@SuppressWarnings("restriction")
public final class AnnotationRemoveModification extends SARLSemanticModification {

	/** Create the quick fix if needed.
	 *
	 * <p>No user data.
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param acceptor the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		final var modification = new AnnotationRemoveModification();
		modification.setIssue(issue);
		modification.setTools(provider);
		acceptor.accept(issue,
				Messages.SARLQuickfixProvider_0,
				Messages.SARLQuickfixProvider_10,
				JavaPluginImages.IMG_CORRECTION_REMOVE,
				modification,
				IProposalRelevance.REMOVE_ANNOTATION);
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		getTools().removeIncludingWhiteSpaces(element, XAnnotation.class, context);
	}

}
