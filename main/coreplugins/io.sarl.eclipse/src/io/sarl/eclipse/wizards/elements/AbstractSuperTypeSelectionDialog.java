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

package io.sarl.eclipse.wizards.elements;

import java.text.MessageFormat;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.TypeNameMatch;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog;
import org.eclipse.jdt.internal.ui.dialogs.StatusInfo;
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.osgi.util.TextProcessor;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.util.Utilities;

/** Dialog box for selecting an type.
 *
 * @param <T> the type of the wizard page.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSuperTypeSelectionDialog<T extends NewTypeWizardPage> extends OpenTypeSelectionDialog {

	private static final String JAVA_ELEMENT_DELIMITERS = TextProcessor.getDefaultDelimiters() + "<>(),?{} "; //$NON-NLS-1$

	private static final int ADD_ID = IDialogConstants.CLIENT_ID + 1;

	private final T typeWizardPage;

	private final List<String> oldContent;

	/**
	 * Creates new instance.
	 *
	 * @param parent shell to parent the dialog on.
	 * @param context context used to execute long-running operations associated with this dialog.
	 * @param page page that opened this dialog.
	 * @param scope the search scope.
	 * @param elementType the type of elements in the dialog box.
	 * @param extension the type selection extension.
	 * @param multi indicates if multiple elements could be selected.
	 */
	public AbstractSuperTypeSelectionDialog(Shell parent, IRunnableContext context, T page,
			IJavaSearchScope scope, int elementType, SarlSpecificTypeSelectionExtension extension,
			boolean multi) {
		super(parent, multi, context, scope, elementType, extension);
		this.typeWizardPage = page;
		this.oldContent = saveWizardPage(this.typeWizardPage);
		setStatusLineAboveButtons(true);
	}

	/** Creates a searching scope including only one project.
	 *
	 * @param project the scope of the search.
	 * @param type the expected super type.
	 * @param onlySubTypes indicates if only the subtypes of the given types are allowed. If
	 *     <code>false</code>, the super type is allowed too.
	 * @return the search scope.
	 */
	public static IJavaSearchScope createSearchScope(IJavaProject project, Class<?> type, boolean onlySubTypes) {
		try {
			final IType superType = project.findType(type.getName());
			return SearchEngine.createStrictHierarchyScope(
					project,
					superType,
					// only sub types
					onlySubTypes,
					// include the type
					true,
					null);
		} catch (JavaModelException e) {
			SARLEclipsePlugin.getDefault().log(e);
		}
		return SearchEngine.createJavaSearchScope(new IJavaElement[] {project});
	}

	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, ADD_ID, Messages.AbstractSuperTypeSelectionDialog_1, true);
		super.createButtonsForButtonBar(parent);
	}

	@Override
	protected IDialogSettings getDialogBoundsSettings() {
		return JavaPlugin.getDefault().getDialogSettingsSection("DialogBounds_" + getClass().getName()); //$NON-NLS-1$
	}

	@Override
	protected void updateButtonsEnableState(IStatus status) {
		super.updateButtonsEnableState(status);
		final Button addButton = getButton(ADD_ID);
		if (addButton != null && !addButton.isDisposed()) {
			addButton.setEnabled(!status.matches(IStatus.ERROR));
		}
	}

	/** Replies the data to save for the given wizard page.
	 *
	 * @param wizardPage the wizard page.
	 * @return the data to save.
	 * @see #restoreWizardPage(NewTypeWizardPage, List)
	 */
	protected abstract List<String> saveWizardPage(T wizardPage);

	/** Restore the state of the wizard page.
	 *
	 * @param wizardPage the wizard page to restore.
	 * @param savedContent the saved content.
	 * @see #saveWizardPage(NewTypeWizardPage)
	 */
	protected abstract void restoreWizardPage(T wizardPage, List<String> savedContent);

	@Override
	protected void handleShellCloseEvent() {
		super.handleShellCloseEvent();
		// Handle the closing of the shell by selecting the close icon
		restoreWizardPage(this.typeWizardPage, this.oldContent);
	}

	@Override
	protected void cancelPressed() {
		restoreWizardPage(this.typeWizardPage, this.oldContent);
		super.cancelPressed();
	}

	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == ADD_ID) {
			fillWizardPageWithSelectedTypes();
		} else {
			super.buttonPressed(buttonId);
		}
	}

	@Override
	protected void okPressed() {
		fillWizardPageWithSelectedTypes();
		super.okPressed();
	}

	/** Add the given qualified name to the wizard page.
	 *
	 * @param wizardPage the wizard page to update.
	 * @param qualifiedName the qualified name.
	 * @return <code>true</code> if the qualified name is new is the wizard page.
	 */
	protected abstract boolean addTypeToWizardPage(T wizardPage, String qualifiedName);

	/** Adds selected interfaces to the list.
	 */
	private void fillWizardPageWithSelectedTypes() {
		final StructuredSelection selection = getSelectedItems();
		if (selection == null) {
			return;
		}
		for (final Iterator<?> iter = selection.iterator(); iter.hasNext();) {
			final Object obj = iter.next();
			if (obj instanceof TypeNameMatch) {
				accessedHistoryItem(obj);
				final TypeNameMatch type = (TypeNameMatch) obj;
				final String qualifiedName = Utilities.getNameWithTypeParameters(type.getType());
				final String message;

				if (addTypeToWizardPage(this.typeWizardPage, qualifiedName)) {
					message = MessageFormat.format(Messages.AbstractSuperTypeSelectionDialog_2,
							TextProcessor.process(qualifiedName, JAVA_ELEMENT_DELIMITERS));
				} else {
					message = MessageFormat.format(Messages.AbstractSuperTypeSelectionDialog_3,
							TextProcessor.process(qualifiedName, JAVA_ELEMENT_DELIMITERS));
				}
				updateStatus(new StatusInfo(IStatus.INFO, message));
			}
		}
	}

	@Override
	protected void handleDoubleClick() {
		buttonPressed(ADD_ID);
	}

	/** Replies the count of super types in the given page.
	 *
	 * @param wizardPage the wizard page to restore.
	 * @return the number of types in the page.
	 */
	protected abstract int getSuperTypeCount(T wizardPage);

	@Override
	protected void handleSelected(StructuredSelection selection) {
		super.handleSelected(selection);

		if (selection.size() == 0 && getSuperTypeCount(this.typeWizardPage) > this.oldContent.size()) {
			// overrides updateStatus() from handleSelected() if
			// list of super interfaces was modified
			// the <code>super.handleSelected(selection)</code> has to be
			// called, because superclass implementation of this class updates
			// state of the table.

			updateStatus(Status.OK_STATUS);

			getButton(ADD_ID).setEnabled(false);
		} else {
			// if selection isn't empty, the add button should be enabled in
			// exactly the same scenarios as the OK button
			getButton(ADD_ID).setEnabled(getButton(OK).isEnabled());
		}
	}

}
