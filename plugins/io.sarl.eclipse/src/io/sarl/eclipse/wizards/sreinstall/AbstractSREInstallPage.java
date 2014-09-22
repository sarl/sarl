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
package io.sarl.eclipse.wizards.sreinstall;

import io.sarl.eclipse.launch.sre.ISREInstall;
import io.sarl.eclipse.util.PluginUtil;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;

/**
 * Abstract implementation of a page for the SRE installation wizard.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSREInstallPage extends WizardPage {

	/**
	 * Name of the original SRE being edited, or <code>null</code> if none.
	 */
	private String originalName;

	/**
	 * Status of SRE name (to notify of name already in use).
	 */
	private IStatus nameStatus = Status.OK_STATUS;

	private String[] existingNames;

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
	 *   or <code>null</code> if none
	 * @param titleImage the image descriptor for the title of this wizard page,
	 *   or <code>null</code> if none
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
	 * @param sre - the SRE install to edit
	 */
	public void setSelection(ISREInstall sre) {
		if (sre != null) {
			this.originalName = sre.getNameNoDefault();
		}
	}

	/**
	 * Create a SRE install to be edited.
	 *
	 * @param id - the identifier of the new SRE.
	 * @return the created SRE.
	 */
	public abstract ISREInstall createSelection(String id);

	/** Replies if the name is valid.
	 *
	 * @param currentName - the current value for the name.
	 * @param originalName - the original value for the name.
	 * @return <code>true</code> if the name is valid.
	 */
	@SuppressWarnings("static-method")
	protected boolean isValidName(String currentName, String originalName) {
		return currentName != null
				&& !currentName.isEmpty();
	}

	/**
	 * Updates the name status based on the new name. This method should be called
	 * by the page each time the SRE name changes.
	 *
	 * @param newName new name of SRE
	 */
	protected void nameChanged(String newName) {
		this.nameStatus = Status.OK_STATUS;
		if (!isValidName(newName, this.originalName)) {
			int sev = IStatus.ERROR;
			if (this.originalName == null || this.originalName.isEmpty()) {
				sev = IStatus.WARNING;
			}
			// TODO: Use NLS.
			this.nameStatus = PluginUtil.createStatus(sev,
					"You must specify a valid name for the SRE."); //$NON-NLS-1$
		} else {
			if (isDuplicateName(newName)) {
				// TODO: Use NLS.
				this.nameStatus = PluginUtil.createStatus(IStatus.ERROR,
						"Duplicate name for the SRE."); //$NON-NLS-1$
			} else {
				IStatus s = ResourcesPlugin.getWorkspace().validateName(newName, IResource.FILE);
				if (!s.isOK()) {
					//TODO: Use NLS.
					this.nameStatus = PluginUtil.createStatus(IStatus.ERROR,
							"Invalid format of the name: " + s.getMessage());  //$NON-NLS-1$
				}
			}
		}
		updatePageStatus();
	}

	/**
	 * Returns whether the name is already in use by an existing SRE.
	 *
	 * @param name - new name.
	 * @return whether the name is already in use.
	 */
	private boolean isDuplicateName(String name) {
		if (this.existingNames != null) {
			for (String eName : this.existingNames) {
				if (PluginUtil.equalsString(name, eName)) {
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
	}

	@Override
	public IWizardPage getNextPage() {
		return null;
	}

	/**
	 * Sets this page's message based on the status severity.
	 *
	 * @param status status with message and severity.
	 */
	protected void setStatusMessage(IStatus status) {
		if (status.isOK()) {
			setMessage(status.getMessage());
		} else {
			switch (status.getSeverity()) {
			case IStatus.ERROR:
				setMessage(status.getMessage(), IMessageProvider.ERROR);
				break;
			case IStatus.INFO:
				setMessage(status.getMessage(), IMessageProvider.INFORMATION);
				break;
			case IStatus.WARNING:
				setMessage(status.getMessage(), IMessageProvider.WARNING);
				break;
			default:
				break;
			}
		}
	}

	/**
	 * Returns the current status of the name being used for the SRE.
	 *
	 * @return the status of current SRE name.
	 */
	protected IStatus getNameStatus() {
		return this.nameStatus;
	}

	/**
	 * Updates the status message on the page, based on the status of the SRE and other
	 * status provided by the page.
	 */
	protected void updatePageStatus() {
		IStatus max = Status.OK_STATUS;
		for (IStatus status : getSREStatus()) {
			if (status.getSeverity() > max.getSeverity()) {
				max = status;
			}
		}
		if (this.nameStatus.getSeverity() > max.getSeverity()) {
			max = this.nameStatus;
		}
		if (max.isOK()) {
			setMessage(null, IMessageProvider.NONE);
		} else {
			setStatusMessage(max);
		}
		setPageComplete(max.isOK() || max.getSeverity() == IStatus.INFO);
	}

	/**
	 * Returns a collection of status messages pertaining to the current edit
	 * status of the SRE on this page. An empty collection or a collection of
	 * OK status objects indicates all is well.
	 *
	 * @return collection of status objects for this page
	 */
	protected abstract IStatus[] getSREStatus();

}
