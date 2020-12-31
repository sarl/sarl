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

package io.sarl.eclipse.wizards.newfile;

import javax.inject.Inject;
import javax.inject.Named;

import com.google.inject.Injector;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.xtext.Constants;

/**
 * SARL new file wizard.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlFileWizard extends Wizard implements INewWizard {

	private IWorkbench workbench;

	private IStructuredSelection selection;

	private NewSarlFileWizardPage pageOne;

	private final String fileExtension;

	@Inject
	private Injector injector;

	/** Constructor.
	 * @param fileExtension the extension of the file.
	 */
	@Inject
	public NewSarlFileWizard(@Named(Constants.FILE_EXTENSIONS) String fileExtension) {
		this.fileExtension = fileExtension;
	}

	@Override
	public void init(IWorkbench iworkbench, IStructuredSelection iselection) {
		this.workbench = iworkbench;
		this.selection = iselection;
	}

	@Override
	public void addPages() {
		super.addPages();
		this.pageOne = new NewSarlFileWizardPage(this.selection, this.fileExtension);
		this.injector.injectMembers(this.pageOne);
		addPage(this.pageOne);
	}

	@Override
	public boolean performFinish() {
		final IFile file = this.pageOne.createNewFile();
		final boolean result = file != null;

		if (result) {
			try {
				IDE.openEditor(this.workbench.getActiveWorkbenchWindow().getActivePage(), file);
			} catch (PartInitException e) {
				throw new RuntimeException(e);
			}
		} // else no file created...result == false
		return result;
	}

}
