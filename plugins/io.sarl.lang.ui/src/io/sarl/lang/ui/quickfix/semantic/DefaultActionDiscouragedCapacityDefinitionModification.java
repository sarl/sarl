/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.ui.quickfix.semantic;

import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;
import io.sarl.lang.validation.IssueCodes;

import java.text.MessageFormat;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;

/**
 * Quick fixes for {@link IssueCodes#DISCOURAGED_CAPACITY_DEFINITION} by adding a default function.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "http://www.eclipse.org/Xtext/documentation.html#quickfixes"
 */
public final class DefaultActionDiscouragedCapacityDefinitionModification extends SARLSemanticModification {

	private final String defaultActionName;

	private DefaultActionDiscouragedCapacityDefinitionModification(String defaultActionName) {
		this.defaultActionName = defaultActionName;
	}

	/** Create the quick fix if needed.
	 *
	 * @param provider - the quick fix provider.
	 * @param issue - the issue to fix.
	 * @param acceptor - the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		String[] data = issue.getData();
		if (data != null && data.length > 1) {
			String defaultActionName = data[1];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_13,
					Messages.SARLQuickfixProvider_8, defaultActionName);
			DefaultActionDiscouragedCapacityDefinitionModification modification =
					new DefaultActionDiscouragedCapacityDefinitionModification(defaultActionName);
			modification.setIssue(issue);
			modification.setTools(provider);
			acceptor.accept(issue,
					msg,
					msg,
					null,
					modification);
		}
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		FeatureContainer container = EcoreUtil2.getContainerOfType(element, FeatureContainer.class);
		if (container != null) {
			int insertOffset = getTools().getInsertOffset(container);
			IXtextDocument document = context.getXtextDocument();
			int length = getTools().getSpaceSize(document, insertOffset);
			ReplacingAppendable appendable = getTools().getAppendableFactory().create(document,
					(XtextResource) element.eResource(), insertOffset, length);
			boolean changeIndentation = container.getFeatures().isEmpty();
			if (changeIndentation) {
				appendable.increaseIndentation();
			}
			appendable.newLine();
			appendable.append(
					getTools().getGrammarAccess().getActionSignatureAccess().getDefKeyword_1().getValue());
			appendable.append(" "); //$NON-NLS-1$
			appendable.append(this.defaultActionName);
			if (changeIndentation) {
				appendable.decreaseIndentation();
			}
			appendable.newLine();
			appendable.commitChanges();
		}
	}

}
