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

package io.sarl.eclipse.log;

import java.net.URL;
import java.text.MessageFormat;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.xtext.util.Strings;
import org.osgi.service.prefs.BackingStoreException;

import io.sarl.eclipse.SARLEclipsePlugin;

/**
 * Page for entering information on an issue to submit to the SARL tracker.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public class IssueInformationPage extends WizardPage {

	/** Preference key that stores the Github login.
	 */
	public static final String PREFERENCE_LOGIN = IssueInformationPage.class.getName() + ".githubLogin"; //$NON-NLS-1$

	private static final char ECHO_CHAR = '*';

	private final URL link;

	private Text titleField;

	private Text descriptionField;

	private Text trackerLogin;

	private Text trackerPassword;

	/** Constructor.
	 *
	 * @param link the link to put in the page.
	 */
	protected IssueInformationPage(URL link) {
		super(Messages.IssueInformationPage_0);
		this.link = link;
		setTitle(Messages.SubmitEclipseLogWizard_0);
	}

	@Override
	public void createControl(Composite parent) {
		// create a composite with standard margins and spacing
		final Composite composite = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		composite.setLayout(layout);
		composite.setLayoutData(new GridData(GridData.FILL_BOTH));

		if (this.link != null) {
			final Link linkWidget = new Link(composite, SWT.BORDER);
			linkWidget.setText(MessageFormat.format(Messages.IssueInformationPage_9, this.link.toString()));
			linkWidget.setFont(composite.getFont());
			final GridData gd = new GridData(GridData.FILL_HORIZONTAL);
			gd.horizontalSpan = 2;
			linkWidget.setLayoutData(gd);
			linkWidget.addSelectionListener(new SelectionAdapter() {
				@SuppressWarnings("synthetic-access")
				@Override
				public void widgetSelected(SelectionEvent event) {
					try {
						final IWebBrowser browser = PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser();
						browser.openURL(IssueInformationPage.this.link);
						final IWizard wizard = IssueInformationPage.this.getWizard();
						if (wizard instanceof SubmitEclipseLogWizard) {
							final WizardDialog dialog = ((SubmitEclipseLogWizard) wizard).getWizardDialog();
							if (dialog != null) {
								dialog.close();
							}
						}
					} catch (PartInitException e) {
						//
					}
				}
			});
		}

		SWTFactory.createLabel(composite, Messages.IssueInformationPage_1, 1);
		this.titleField = SWTFactory.createSingleText(composite, 1);

		SWTFactory.createLabel(composite, Messages.IssueInformationPage_2, 1);
		this.descriptionField = SWTFactory.createText(composite, SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.V_SCROLL, 1);
		this.descriptionField.setLayoutData(new GridData(GridData.FILL_BOTH));

		SWTFactory.createLabel(composite, Messages.IssueInformationPage_3, 1);
		this.trackerLogin = SWTFactory.createSingleText(composite, 1);

		SWTFactory.createLabel(composite, Messages.IssueInformationPage_4, 1);
		this.trackerPassword = SWTFactory.createSingleText(composite, 1);
		this.trackerPassword.setEchoChar(ECHO_CHAR);

		//add the listeners now to prevent them from monkeying with initialized settings
		this.titleField.addModifyListener(listeningEvent -> {
			updatePageStatus();
		});
		this.descriptionField.addModifyListener(listeningEvent -> {
			updatePageStatus();
		});
		this.trackerLogin.addModifyListener(listeningEvent -> {
			updatePageStatus();
		});
		this.trackerPassword.addModifyListener(listeningEvent -> {
			updatePageStatus();
		});

		Dialog.applyDialogFont(composite);
		setControl(composite);

		initFields();
		updatePageStatus();
	}

	/** Replies the title of the issue.
	 *
	 * @return the title.
	 */
	public String getIssueTitle() {
		return this.titleField.getText();
	}

	/** Replies the description of the issue.
	 *
	 * @return the description.
	 */
	public String getIssueDescription() {
		return this.descriptionField.getText();
	}

	/** Replies the Github login.
	 *
	 * @return the login.
	 */
	public String getGithubLogin() {
		return this.trackerLogin.getText();
	}

	/** Replies the Github password.
	 *
	 * @return the password.
	 */
	public String getGithubPassword() {
		return this.trackerPassword.getText();
	}

	/** Set the initial values to the fields.
	 */
	protected void initFields() {
		final IEclipsePreferences prefs = SARLEclipsePlugin.getDefault().getPreferences();
		this.trackerLogin.setText(prefs.get(PREFERENCE_LOGIN, "")); //$NON-NLS-1$
	}

	/** Update the page status and change the "finish" state button.
	 */
	protected void updatePageStatus() {
		final boolean ok;
		if (Strings.isEmpty(this.titleField.getText())) {
			ok = false;
			setMessage(Messages.IssueInformationPage_5, IMessageProvider.ERROR);
		} else if (Strings.isEmpty(this.trackerLogin.getText())) {
			ok = false;
			setMessage(Messages.IssueInformationPage_6, IMessageProvider.ERROR);
		} else if (Strings.isEmpty(this.trackerPassword.getText())) {
			ok = false;
			setMessage(Messages.IssueInformationPage_7, IMessageProvider.ERROR);
		} else {
			ok = true;
			if (Strings.isEmpty(this.descriptionField.getText())) {
				setMessage(Messages.IssueInformationPage_8, IMessageProvider.WARNING);
			} else {
				setMessage(null, IMessageProvider.NONE);
			}
		}
		setPageComplete(ok);
	}

	/** Invoked when the wizard is closed with the "Finish" button.
	 *
	 * @return {@code true} for closing the wizard; {@code false} for keeping it open.
	 */
	public boolean performFinish() {
		final IEclipsePreferences prefs = SARLEclipsePlugin.getDefault().getPreferences();
		final String login = this.trackerLogin.getText();
		if (Strings.isEmpty(login)) {
			prefs.remove(PREFERENCE_LOGIN);
		} else {
			prefs.put(PREFERENCE_LOGIN, login);
		}
		try {
			prefs.sync();
			return true;
		} catch (BackingStoreException e) {
			ErrorDialog.openError(getShell(), e.getLocalizedMessage(), e.getLocalizedMessage(),
					SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, e));
			return false;
		}
	}

}
