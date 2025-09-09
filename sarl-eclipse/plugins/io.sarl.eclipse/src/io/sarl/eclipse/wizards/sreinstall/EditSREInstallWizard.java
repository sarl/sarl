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

package io.sarl.eclipse.wizards.sreinstall;

import java.text.MessageFormat;

import io.sarl.apputils.eclipseextensions.sreinstall.AbstractSREInstallPage;
import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * Wiazrd for SRE installation.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
public class EditSREInstallWizard extends SREInstallWizard {

	private AbstractSREInstallPage editPage;

	/**
	 * Constructs a new wizard to edit a SRE.
	 *
	 * @param editSRE the SRE being edited, or {@code null} if none.
	 * @param currentInstalls current SRE installs used to validate name changes.
	 */
	public EditSREInstallWizard(ISREInstall editSRE, ISREInstall[] currentInstalls) {
		super(editSRE, currentInstalls);
		final var name = editSRE.getName();
		final var title = MessageFormat.format(Messages.SREInstallWizard_4, name);
		setWindowTitle(title);
	}

	@Override
	public void addPages() {
		try {
			final var original = getOriginalSRE();
			this.editPage = getPage(original);
			this.editPage.initialize(original);
			addPage(this.editPage);
		} catch (Throwable ex) {
			this.editPage = null;
			SARLEclipsePlugin.getDefault().openError(getShell(),
					getWindowTitle(),
					ex.getLocalizedMessage(), null, ex);
		}
	}

	@Override
	public boolean performFinish() {
		if (this.editPage == null || this.editPage.performFinish()) {
			return super.performFinish();
		}
		return false;
	}

	@Override
	public boolean performCancel() {
		if (this.editPage == null || this.editPage.performCancel()) {
			return super.performCancel();
		}
		return false;
	}

}
