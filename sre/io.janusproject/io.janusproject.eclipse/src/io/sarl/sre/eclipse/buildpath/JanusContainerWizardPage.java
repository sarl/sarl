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

package io.sarl.sre.eclipse.buildpath;

import java.text.MessageFormat;

import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.ui.wizards.IClasspathContainerPage;
import org.eclipse.jdt.ui.wizards.NewElementWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/** Wizard page that permits to add the Janus libraries into a project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusContainerWizardPage extends NewElementWizardPage implements IClasspathContainerPage {

	private IClasspathEntry containerEntry;

	/** Construct a wizard page for defining the SARL library container.
	 */
	public JanusContainerWizardPage() {
		super("JanusClassPathContainer"); //$NON-NLS-1$
		setTitle(Messages.JanusClasspathContainer_0);
		setImageDescriptor(JavaPluginImages.DESC_WIZBAN_ADD_LIBRARY);
		setDescription(Messages.JanusContainerWizardPage_0);
		this.containerEntry = JavaCore.newContainerEntry(
				JanusClasspathContainerInitializer.CONTAINER_ID);
	}

	@Override
	public void createControl(Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		composite.setLayout(new FillLayout());
		final Label label = new Label(composite, SWT.NONE);
		final StringBuilder text = new StringBuilder();
		for (final String entry : JanusClasspathContainer.JANUS_DEPENDENCY_BUNDLE_NAMES) {
			text.append(entry);
			text.append("\n"); //$NON-NLS-1$
		}
		label.setText(MessageFormat.format(Messages.JanusContainerWizardPage_1, text.toString()));
		setControl(composite);
	}

	@Override
	public boolean finish() {
		return true;
	}

	@Override
	public IClasspathEntry getSelection() {
		return this.containerEntry;

	}

	@Override
	public void setSelection(IClasspathEntry containerEntry) {
		//do nothing
	}

}
