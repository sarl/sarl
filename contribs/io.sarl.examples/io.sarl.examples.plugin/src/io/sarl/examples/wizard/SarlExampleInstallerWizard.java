/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

package io.sarl.examples.wizard;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.emf.common.ui.wizard.ExampleInstallerWizard;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.intro.IIntroManager;
import org.eclipse.ui.intro.IIntroPart;

import io.sarl.eclipse.natures.SARLProjectConfigurator;

/** Wizard for importing SARL samples.
 *
 * <p>This wizard extends the EMF wizard with the initialization of the SARL nature on the project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlExampleInstallerWizard extends ExampleInstallerWizard {

	@Override
	protected void createProject(ProjectDescriptor projectDescriptor, IProgressMonitor monitor) throws CoreException {
		final SubMonitor mon = SubMonitor.convert(monitor, 2);
		super.createProject(projectDescriptor, mon.newChild(1));
		SARLProjectConfigurator.configureSARLProject(
				// Project to configure
				projectDescriptor.getProject(),
				// Force java configuration
				true,
				// Create folders
				true,
				// Monitor
				mon.newChild(1));
		mon.done();
	}

	@Override
	public boolean performFinish() {
		if (super.performFinish()) {
			// Close the welcome page.
			closeWelcomePage();
			return true;
		}
		return false;
	}

	/** Close the welcome page.
	 */
	protected static void closeWelcomePage() {
		final IIntroManager introManager = PlatformUI.getWorkbench().getIntroManager();
		if (introManager != null) {
			final IIntroPart intro = introManager.getIntro();
			if (intro != null) {
				introManager.closeIntro(intro);
			}
		}
	}

}
