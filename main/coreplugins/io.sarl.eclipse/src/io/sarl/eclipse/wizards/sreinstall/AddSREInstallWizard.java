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

import io.sarl.eclipse.runtime.ISREInstall;

/**
 * Wizard for SRE installation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AddSREInstallWizard extends SREInstallWizard {

	private final String id;

	private AbstractSREInstallPage addPage;

	private ISREInstall createdSRE;

	/**
	 * Constructs a new wizard to add a SRE.
	 *
	 * @param id the identifier of the created SRE.
	 * @param currentInstalls current SRE installs used to validate name changes.
	 */
	public AddSREInstallWizard(String id, ISREInstall[] currentInstalls) {
		super(null, currentInstalls);
		this.id = id;
		setWindowTitle(Messages.SREInstallWizard_3);
	}

	/** Replies the created SRE.
	 *
	 * @return the created SRE.
	 */
	public ISREInstall getCreatedSRE() {
		return this.createdSRE;
	}

	@Override
	public void addPages() {
		this.addPage = getPage(getOriginalSRE());
		this.createdSRE = this.addPage.createSelection(this.id);
		addPage(this.addPage);
	}

	@Override
	public boolean performFinish() {
		if (this.addPage.performFinish()) {
			return super.performFinish();
		}
		return false;
	}

	@Override
	public boolean performCancel() {
		if (this.addPage.performCancel()) {
			return super.performCancel();
		}
		return false;
	}

}
