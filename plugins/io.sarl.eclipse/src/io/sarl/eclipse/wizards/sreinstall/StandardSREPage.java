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
import io.sarl.eclipse.launch.sre.SARLRuntime;
import io.sarl.eclipse.launch.sre.SREException;
import io.sarl.eclipse.launch.sre.StandardSREInstall;
import io.sarl.eclipse.util.PluginUtil;

import java.io.File;
import java.text.MessageFormat;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.internal.debug.ui.JavaDebugImages;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Text;

/**
 * Standard implementation of a page for the SRE installation wizard.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardSREPage extends AbstractSREInstallPage {

	private IStatus[] statusForFields = new IStatus[2];
	private Text sreLibraryTextField;
	private Text sreNameTextField;
	private Text sreMainClassTextField;
	private Text sreIdTextField;

	private StandardSREInstall originalSRE;
	private StandardSREInstall workingCopy;

	/**
	 */
	public StandardSREPage() {
		super(""); //$NON-NLS-1$
		for (int i = 0; i < this.statusForFields.length; i++) {
			this.statusForFields[i] = Status.OK_STATUS;
		}
	}

	@Override
	public Image getImage() {
		return JavaDebugImages.get(JavaDebugImages.IMG_WIZBAN_LIBRARY);
	}

	@Override
	public void createControl(Composite p) {
		// create a composite with standard margins and spacing
		Composite composite = new Composite(p, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.numColumns = 3;
		composite.setLayout(layout);
		composite.setLayoutData(new GridData(GridData.FILL_BOTH));

		// SRE location
		SWTFactory.createLabel(composite, Messages.StandardSREPage_0, 1);
		this.sreLibraryTextField = SWTFactory.createSingleText(composite, 1);
		this.sreLibraryTextField.setEditable(false);
		Button folders = SWTFactory.createPushButton(composite, Messages.StandardSREPage_1, null);
		GridData data = (GridData) folders.getLayoutData();
		data.horizontalAlignment = GridData.END;
		//SRE name
		SWTFactory.createLabel(composite, Messages.StandardSREPage_2, 1);
		this.sreNameTextField = SWTFactory.createSingleText(composite, 2);
		//SRE main class
		SWTFactory.createLabel(composite, Messages.StandardSREPage_3, 1);
		this.sreMainClassTextField = SWTFactory.createSingleText(composite, 2);
		//SRE Id
		SWTFactory.createLabel(composite, Messages.StandardSREPage_8, 1);
		this.sreIdTextField = SWTFactory.createSingleText(composite, 2);
		this.sreIdTextField.setEditable(false);

		//add the listeners now to prevent them from monkeying with initialized settings
		this.sreNameTextField.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void modifyText(ModifyEvent e) {
				StandardSREPage.this.workingCopy.setName(
						StandardSREPage.this.sreNameTextField.getText());
				StandardSREPage.this.workingCopy.revalidate();
				validateSREName();
			}
		});
		this.sreLibraryTextField.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void modifyText(ModifyEvent e) {
				IPath path = Path.fromPortableString(
						StandardSREPage.this.sreLibraryTextField.getText());
				StandardSREPage.this.workingCopy.setJarFile(path);
				StandardSREPage.this.workingCopy.revalidate();
				validateSRELibrary();
				initializeFieldsFromLibraryPath();
			}
		});
		this.sreMainClassTextField.addModifyListener(new ModifyListener() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void modifyText(ModifyEvent e) {
				StandardSREPage.this.workingCopy.setMainClass(
						StandardSREPage.this.sreMainClassTextField.getText());
				StandardSREPage.this.workingCopy.revalidate();
				validateSREMainClass();
			}
		});
		folders.addSelectionListener(new SelectionAdapter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public void widgetSelected(SelectionEvent e) {
				File file = new File(StandardSREPage.this.sreLibraryTextField.getText());

				// XXX: JARFileSelectionDialog may be used for selecting a jar file in the workspace.
				FileDialog dialog = new FileDialog(getShell(), SWT.OPEN);
				dialog.setText(Messages.StandardSREPage_4);
				dialog.setFilterExtensions(new String[] {"*.jar"}); //$NON-NLS-1$
				if (file.exists()) {
					dialog.setFileName(file.getAbsolutePath());
				}
				String selectedFile = dialog.open();
				if (selectedFile != null) {
					IPath path = Path.fromOSString(selectedFile);
					IWorkspace workspace = ResourcesPlugin.getWorkspace();
					IPath workspaceLocation = workspace.getRoot().getLocation();
					if (workspaceLocation.isPrefixOf(path)) {
						path = workspaceLocation.makeRelativeTo(workspaceLocation);
					}
					StandardSREPage.this.sreLibraryTextField.setText(path.toPortableString());
				}
			}
		});
		Dialog.applyDialogFont(composite);
		setControl(composite);
		initializeFields();
		//PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(),
		//IJavaDebugHelpContextIds.EDIT_JRE_STD_VM_WIZARD_PAGE);
	}

	/**
	 * Validates the SRE location.
	 */
	private void validateSRELibrary() {
		String libraryFile = this.sreLibraryTextField.getText();
		IStatus s = null;
		IPath path = null;

		if (libraryFile.isEmpty()) {
			s = PluginUtil.createStatus(IStatus.WARNING,
					Messages.StandardSREPage_5);
		} else {
			path = new Path(libraryFile);
			if (!path.toFile().exists()) {
				s = PluginUtil.createStatus(IStatus.ERROR,
						Messages.StandardSREPage_6);
			} else {
				final IStatus[] temp = new IStatus[1];
				final IPath tmpPath = path;
				Runnable r = new Runnable() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void run() {
						StandardSREPage.this.workingCopy.setJarFile(tmpPath);
						temp[0] = StandardSREPage.this.workingCopy.revalidate();
					}
				};
				BusyIndicator.showWhile(getShell().getDisplay(), r);
				s = temp[0];
			}
		}

		setSRELocationStatus(s);
		updatePageStatus();
	}

	/**
	 * Validates the entered name of the SRE.
	 */
	private void validateSREName() {
		nameChanged(this.sreNameTextField.getText());
	}

	/**
	 * Validates the entered main class of the SRE.
	 */
	private void validateSREMainClass() {
		IStatus mainclassStatus = Status.OK_STATUS;
		String mainClass = this.sreMainClassTextField.getText();
		if (mainClass == null || mainClass.trim().isEmpty()) {
			mainclassStatus = PluginUtil.createStatus(IStatus.ERROR,
					"You must specify the main class of the SRE."); //$NON-NLS-1$
		}
		setSREMainClassStatus(mainclassStatus);
		updatePageStatus();
	}

	@Override
	public boolean performFinish() {
		try {
			String xml = SARLRuntime.getSREAsXML(this.workingCopy);
			SARLRuntime.setSREFromXML(this.originalSRE, xml);
			return true;
		} catch (CoreException e) {
			setErrorMessage(e.getLocalizedMessage());
			return false;
		}
	}

	@Override
	public void setSelection(ISREInstall sre) {
		if (!(sre instanceof StandardSREInstall)) {
			throw new SREException("Illegal SRE type: expecting StandardSREInstall."); //$NON-NLS-1$
		}
		this.originalSRE = (StandardSREInstall) sre;
		this.workingCopy = this.originalSRE.clone();
		this.workingCopy.setNotify(false);
		this.workingCopy.revalidate();
		super.setSelection(sre);
		setTitle(MessageFormat.format(Messages.StandardSREPage_7, sre.getName()));
	}

	@Override
	public ISREInstall createSelection(String id) {
		StandardSREInstall sre = new StandardSREInstall(id);
		sre.revalidate();
		setSelection(sre);
		return sre;
	}

	/**
	 * Initialize the dialogs fields.
	 */
	private void initializeFields() {
		String name = this.workingCopy.getNameNoDefault();
		this.sreNameTextField.setText(name == null ? "" : name); //$NON-NLS-1$
		IPath path = this.workingCopy.getJarFile();
		if (path != null) {
			this.sreLibraryTextField.setText(path.toPortableString());
		} else {
			this.sreLibraryTextField.setText(""); //$NON-NLS-1$
		}
		String mainClass = this.workingCopy.getMainClass();
		if (mainClass != null && !mainClass.isEmpty()) {
			this.sreMainClassTextField.setText(mainClass);
		} else {
			this.sreMainClassTextField.setText(""); //$NON-NLS-1$
		}
		this.sreIdTextField.setText(this.workingCopy.getId());
		validateSREMainClass();
		validateSREName();
		validateSRELibrary();
	}

	private void initializeFieldsFromLibraryPath() {
		// Update the name
		String name = this.sreNameTextField.getText();
		if (name == null || name.trim().isEmpty()) {
			name = this.workingCopy.getNameNoDefault();
			if (name != null && !name.isEmpty()) {
				this.sreNameTextField.setText(name);
			}
		}
		// Update the main class
		String mainClass = this.sreMainClassTextField.getText();
		if (mainClass == null || mainClass.trim().isEmpty()) {
			mainClass = this.workingCopy.getMainClass();
			if (mainClass != null && !mainClass.isEmpty()) {
				this.sreMainClassTextField.setText(mainClass);
			}
		}
	}

	/**
	 * Sets the status of the SRE location field.
	 *
	 * @param status SRE location status
	 */
	private void setSRELocationStatus(IStatus status) {
		this.statusForFields[0] = status;
	}

	/**
	 * Sets the status of the SRE main class field.
	 *
	 * @param status SRE main class status.
	 */
	private void setSREMainClassStatus(IStatus status) {
		this.statusForFields[1] = status;
	}

	@Override
	protected IStatus[] getSREStatus() {
		return this.statusForFields;
	}

}
