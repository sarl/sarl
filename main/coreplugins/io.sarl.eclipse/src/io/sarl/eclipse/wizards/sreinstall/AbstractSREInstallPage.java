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

package io.sarl.eclipse.wizards.sreinstall;

import java.text.MessageFormat;

import com.google.common.base.Strings;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;

import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.runtime.ISREInstall;

/**
 * Abstract implementation of a page for the SRE installation wizard.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSREInstallPage extends WizardPage {

	private String[] existingNames;

	private IStatus status = SARLEclipsePlugin.getDefault().createOkStatus();

	/**
	 * Constructs a new page with the given page name.
	 *
	 * @param pageName the name of the page.
	 */
	protected AbstractSREInstallPage(String pageName) {
		super(pageName);
	}

	/**
	 * Creates a new wizard page with the given name, title, and image.
	 *
	 * @param pageName the name of the page
	 * @param title the title for this wizard page,
	 *     or {@code null} if none.
	 * @param titleImage the image descriptor for the title of this wizard page,
	 *     or {@code null} if none.
	 */
	protected AbstractSREInstallPage(String pageName, String title, ImageDescriptor titleImage) {
		super(pageName, title, titleImage);
	}

	/**
	 * Called when the SRE install page wizard is closed by selecting
	 * the finish button. Implementers typically override this method to
	 * store the page result (new/changed SRE install returned in
	 * getSelection) into its model.
	 *
	 * @return if the operation was successful. Only when returned
	 * <code>true</code>, the wizard will close.
	 */
	public abstract boolean performFinish();

	/**
	 * Called when the SRE install page wizard is closed by selecting
	 * the cancel button.
	 *
	 * @return if the operation was successful. Only when returned
	 * <code>true</code>, the wizard will close.
	 */
	@SuppressWarnings("static-method")
	public boolean performCancel() {
		return true;
	}

	/**
	 * Sets the SRE install to be edited.
	 *
	 * @param sre the SRE install to edit
	 */
	public abstract void initialize(ISREInstall sre);

	/**
	 * Create a SRE install to be edited.
	 *
	 * @param id the identifier of the new SRE.
	 * @return the created SRE.
	 */
	public abstract ISREInstall createSelection(String id);

	/**
	 * Replies if the name of the SRE is valid against the names of
	 * the other SRE.
	 *
	 * @param name the name to validate.
	 * @return the validation status.
	 */
	protected IStatus validateNameAgainstOtherSREs(String name) {
		IStatus nameStatus = SARLEclipsePlugin.getDefault().createOkStatus();
		if (isDuplicateName(name)) {
			nameStatus = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
					ISREInstall.CODE_NAME,
					Messages.SREInstallWizard_1);
		} else {
			final IStatus status = ResourcesPlugin.getWorkspace().validateName(name, IResource.FILE);
			if (!status.isOK()) {
				nameStatus = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR,
						ISREInstall.CODE_NAME,
						MessageFormat.format(Messages.SREInstallWizard_2, status.getMessage()));
			}
		}
		return nameStatus;
	}

	/** Change the status associated to this page.
	 * Any previous status is overrided by the given value.
	 *
	 * <p>You must call {@link #updatePageStatus()} after
	 * invoking this methid.
	 *
	 * @param status the new status.
	 */
	protected void setPageStatus(IStatus status) {
		this.status = status == null ? SARLEclipsePlugin.getDefault().createOkStatus() : status;
	}

	/**
	 * Returns whether the name is already in use by an existing SRE.
	 *
	 * @param name new name.
	 * @return whether the name is already in use.
	 */
	private boolean isDuplicateName(String name) {
		if (this.existingNames != null) {
			final String newName = Strings.nullToEmpty(name);
			for (final String existingName : this.existingNames) {
				if (newName.equals(existingName)) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Sets the names of existing SREs, not including the SRE being edited. This method
	 * is called by the wizard and clients should not call this method.
	 *
	 * @param names existing SRE names or an empty array.
	 */
	void setExistingNames(String... names) {
		this.existingNames = names;
		for (int i = 0; i < this.existingNames.length; ++i) {
			this.existingNames[i] = Strings.nullToEmpty(this.existingNames[i]);
		}
	}

	@Override
	public IWizardPage getNextPage() {
		return null;
	}

	/**
	 * Updates the status message on the page, based on the status of the SRE and other
	 * status provided by the page.
	 */
	protected void updatePageStatus() {
		if (this.status.isOK()) {
			setMessage(null, IMessageProvider.NONE);
		} else {
			switch (this.status.getSeverity()) {
			case IStatus.ERROR:
				setMessage(this.status.getMessage(), IMessageProvider.ERROR);
				break;
			case IStatus.INFO:
				setMessage(this.status.getMessage(), IMessageProvider.INFORMATION);
				break;
			case IStatus.WARNING:
				setMessage(this.status.getMessage(), IMessageProvider.WARNING);
				break;
			default:
				break;
			}
		}
		setPageComplete(this.status.isOK() || this.status.getSeverity() == IStatus.INFO);
	}

}
