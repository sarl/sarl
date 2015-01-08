/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.eclipse.dialog;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.lang.core.Agent;

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.swt.widgets.Shell;

import com.google.common.annotations.Beta;

/** Dialog box for selecting an agent type.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Beta
public class SuperAgentSelectionDialog extends OpenTypeSelectionDialog {

	/** Creates new instance of SuperInterfaceSelectionDialog.
	 *
	 * @param parent - shell to parent the dialog on
	 * @param context - context used to execute long-running operations associated
	 *            with this dialog
	 * @param project - the java project which will be considered when searching for
	 *            interfaces
	 */
	public SuperAgentSelectionDialog(Shell parent, IRunnableContext context, IJavaProject project) {
		super(parent, true, context, createSearchScope(project), IJavaSearchConstants.CLASS);
		setStatusLineAboveButtons(true);
	}

	@Override
	protected IDialogSettings getDialogBoundsSettings() {
		return JavaPlugin.getDefault().getDialogSettingsSection("DialogBounds_SuperAgentSelectionDialog"); //$NON-NLS-1$
	}

	/*
	 * Creates a searching scope including only one project.
	 */
	private static IJavaSearchScope createSearchScope(IJavaProject project) {
		try {
			IType agentType = project.findType(Agent.class.getName());
			return SearchEngine.createStrictHierarchyScope(
					project,
					agentType,
					// only sub types
					true,
					// include the agent type
					true,
					null);
		} catch (JavaModelException e) {
			SARLEclipsePlugin.getDefault().log(e);
		}
		return SearchEngine.createJavaSearchScope(new IJavaElement[] {project});
	}

}
