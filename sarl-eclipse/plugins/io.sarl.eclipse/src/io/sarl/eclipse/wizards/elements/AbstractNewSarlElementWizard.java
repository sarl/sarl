/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.wizards.NewElementWizard;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.xtext.ui.IImageHelper.IImageDescriptorHelper;

/**
 * Abstract implementation of a wizard for creating new SARL elements.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
@SuppressWarnings("restriction")
public abstract class AbstractNewSarlElementWizard extends NewElementWizard {

	private final AbstractNewSarlElementWizardPage page;

	/** Constructor.
	 * @param imgHelper the helper for getting images.
	 * @param page the wizard page.
	 * @param title the title of the wizard.
	 */
	public AbstractNewSarlElementWizard(IImageDescriptorHelper imgHelper, AbstractNewSarlElementWizardPage page, String title) {
		this.page = page;
		final var image = imgHelper.getImageDescriptor("sarl_64.png"); //$NON-NLS-1$
		setDefaultPageImageDescriptor(image);
		setDialogSettings(JavaPlugin.getDefault().getDialogSettings());
		setWindowTitle(title);
	}

	@Override
	public void addPages() {
		super.addPages();
		this.page.init(getSelection());
		super.addPage(this.page);
	}

	@Override
	protected void finishPage(IProgressMonitor monitor) throws InterruptedException, CoreException {
		//
	}

	@Override
	public IJavaElement getCreatedElement() {
		return null;
	}

	@Override
	public boolean performFinish() {
		final var size = this.page.asyncCreateType();
		final var resource = this.page.getResource();
		if (resource != null) {
			selectAndReveal(resource);
			final var display = getShell().getDisplay();
			display.asyncExec(() -> {
				final IEditorPart editor;
				try {
					editor = IDE.openEditor(JavaPlugin.getActivePage(), (IFile) resource);
					if (editor instanceof ITextEditor textEditor) {
						final var selectionProvider = textEditor.getSelectionProvider();
						final var selection = new TextSelection(size - 2, 0);
						selectionProvider.setSelection(selection);
					}
				} catch (PartInitException e) {
					throw new RuntimeException(e);
				}
			});
			return true;
		}
		return false;
	}

}
