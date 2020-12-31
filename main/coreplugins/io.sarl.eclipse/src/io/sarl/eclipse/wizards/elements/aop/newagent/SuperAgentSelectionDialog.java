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

package io.sarl.eclipse.wizards.elements.aop.newagent;

import java.util.Collections;
import java.util.List;

import com.google.common.base.Strings;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.swt.widgets.Shell;

import io.sarl.eclipse.wizards.elements.AbstractSuperTypeSelectionDialog;
import io.sarl.eclipse.wizards.elements.SarlSpecificTypeSelectionExtension;
import io.sarl.lang.core.Agent;

/** Dialog box for selecting a SARL agent type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SuperAgentSelectionDialog extends AbstractSuperTypeSelectionDialog<NewSarlAgentWizardPage> {

	/**
	 * Creates new instance.
	 *
	 * @param parent shell to parent the dialog on.
	 * @param context context used to execute long-running operations associated with this dialog.
	 * @param project the java project which will be considered when searching for interfaces.
	 * @param wizardPage the wizard page.
	 * @param extension the wizard type selection extension.
	 * @param multi indicates if multiple elements could be selected.
	 */
	protected SuperAgentSelectionDialog(Shell parent, IRunnableContext context, IJavaProject project,
			NewSarlAgentWizardPage wizardPage, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		super(parent, context, wizardPage,
				createSearchScope(project, Agent.class, true),
				IJavaSearchConstants.CLASS, extension, multi);
	}

	@Override
	protected List<String> saveWizardPage(NewSarlAgentWizardPage wizardPage) {
		return Collections.singletonList(wizardPage.getSuperClass());
	}

	@Override
	protected void restoreWizardPage(NewSarlAgentWizardPage wizardPage, List<String> savedContent) {
		if (!savedContent.isEmpty()) {
			wizardPage.setSuperClass(savedContent.get(0), true);
		}
	}

	@Override
	protected boolean addTypeToWizardPage(NewSarlAgentWizardPage wizardPage, String qualifiedName) {
		return false;
	}

	@Override
	protected int getSuperTypeCount(NewSarlAgentWizardPage wizardPage) {
		return Strings.isNullOrEmpty(wizardPage.getSuperClass()) ? 0 : 1;
	}

}
