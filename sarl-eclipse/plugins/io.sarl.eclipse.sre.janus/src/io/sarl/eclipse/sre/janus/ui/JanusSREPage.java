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

package io.sarl.eclipse.sre.janus.ui;

import java.text.MessageFormat;

import com.google.common.base.Strings;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jdt.internal.debug.ui.JavaDebugImages;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import io.sarl.apputils.eclipseextensions.sreinstall.AbstractSREInstallPage;
import io.sarl.apputils.eclipseextensions.sreprovider.ISREInstall;
import io.sarl.apputils.uiextensions.Utilities;
import io.sarl.eclipse.runtime.SREException;
import io.sarl.eclipse.sre.janus.sre.JanusSREInstall;

/**
 * Implementation of a page for the SRE installation wizard.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
@SuppressWarnings("restriction")
public class JanusSREPage extends AbstractSREInstallPage {

	private Text sreLibraryTextField;

	private Text sreNameTextField;

	private Text sreMainClassTextField;

	private Text sreIdTextField;

	private JanusSREInstall originalSRE;

	/** Construct a configuration page for the SREs.
	 */
	public JanusSREPage() {
		super(Utilities.EMPTY_STRING);
	}

	@Override
	public Image getImage() {
		return JavaDebugImages.get(JavaDebugImages.IMG_WIZBAN_LIBRARY);
	}

	@Override
	public void createControl(Composite parent) {
		// create a composite with standard margins and spacing
		final var composite = new Composite(parent, SWT.NONE);
		final var layout = new GridLayout();
		layout.numColumns = 2;
		composite.setLayout(layout);
		composite.setLayoutData(new GridData(GridData.FILL_BOTH));

		// SRE location
		SWTFactory.createLabel(composite, Messages.JanusSREPage_0, 1);
		this.sreLibraryTextField = SWTFactory.createSingleText(composite, 1);
		this.sreLibraryTextField.setEditable(false);
		//SRE name
		SWTFactory.createLabel(composite, Messages.JanusSREPage_2, 1);
		this.sreNameTextField = SWTFactory.createSingleText(composite, 2);
		this.sreNameTextField.setEditable(false);
		//SRE main class
		SWTFactory.createLabel(composite, Messages.JanusSREPage_3, 1);
		this.sreMainClassTextField = SWTFactory.createSingleText(composite, 2);
		this.sreMainClassTextField.setEditable(false);
		//SRE Id
		SWTFactory.createLabel(composite, Messages.JanusSREPage_8, 1);
		this.sreIdTextField = SWTFactory.createSingleText(composite, 2);
		this.sreIdTextField.setEditable(false);

		Dialog.applyDialogFont(composite);
		setControl(composite);

		setPageStatus(null);
		updatePageStatus();
		initializeFields();
	}

	@Override
	public boolean performFinish() {
		return true;
	}

	@Override
	public void initialize(ISREInstall sre) {
		if (!(sre instanceof JanusSREInstall)) {
			throw new SREException("Illegal SRE type: expecting JanusSREInstall."); //$NON-NLS-1$
		}
		setTitle(MessageFormat.format(Messages.JanusSREPage_7, sre.getName()));
		this.originalSRE = (JanusSREInstall) sre;
	}

	@Override
	public ISREInstall createSelection(String id) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Initialize the dialogs fields.
	 */
	private void initializeFields() {
		this.sreLibraryTextField.setText(Strings.nullToEmpty(this.originalSRE.getLocation()));
		//
		final var name = this.originalSRE.getName();
		this.sreNameTextField.setText(Strings.nullToEmpty(name));
		//
		final var mainClass = this.originalSRE.getMainClass();
		this.sreMainClassTextField.setText(Strings.nullToEmpty(mainClass));
		//
		this.sreIdTextField.setText(this.originalSRE.getId());
	}

}
