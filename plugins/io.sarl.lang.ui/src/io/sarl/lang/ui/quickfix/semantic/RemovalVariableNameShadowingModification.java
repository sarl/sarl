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

import io.sarl.lang.sarl.Attribute;
import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;

import java.text.MessageFormat;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.validation.IssueCodes;

/**
 * Quick fixes for {@link IssueCodes#VARIABLE_NAME_SHADOWING}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class RemovalVariableNameShadowingModification extends SARLSemanticModification {

	/** Create the quick fix if needed.
	 *
	 * @param provider - the quick fix provider.
	 * @param issue - the issue to fix.
	 * @param acceptor - the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		String[] data = issue.getData();
		if (data != null && data.length >= 1) {
			String redundantName = data[0];
			String msg = MessageFormat.format(
					Messages.SARLQuickfixProvider_5,
					Messages.SARLQuickfixProvider_7, redundantName);
			RemovalVariableNameShadowingModification modification = new RemovalVariableNameShadowingModification();
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
		getTools().remove(element, Attribute.class, context);
	}

}
