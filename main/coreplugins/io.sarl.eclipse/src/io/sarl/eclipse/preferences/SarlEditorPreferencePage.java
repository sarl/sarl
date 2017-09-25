/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.eclipse.preferences;

import javax.inject.Inject;

import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import io.sarl.lang.ui.editor.SARLSourceViewerPreferenceAccess;

/** Preference page for the SARL editors.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlEditorPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {

	/**
	 * ID for the page.
	 */
	public static final String ID = "io.sarl.eclipse.preferences.SarlEditorPreferencePage"; //$NON-NLS-1$

	@Inject
	private SARLSourceViewerPreferenceAccess preferences;

	private Button autoformattingButton;

	/**
	 * Constructor.
	 */
	public SarlEditorPreferencePage() {
		//
	}

	@Override
	public void init(IWorkbench workbench) {
		//
	}

	/** Replies the preference accessor.
	 *
	 * @return the preference accessor.
	 */
	protected SARLSourceViewerPreferenceAccess getPreferenceAccessor() {
		return this.preferences;
	}

	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		return getPreferenceAccessor().getWritablePreferenceStore();
	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);

		// define container & its gridding
		final Composite pageComponent = new Composite(parent, SWT.NULL);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		pageComponent.setLayout(layout);
		final GridData data = new GridData();
		data.verticalAlignment = GridData.FILL;
		data.horizontalAlignment = GridData.FILL;
		pageComponent.setLayoutData(data);

		// create the content
		this.autoformattingButton = SWTFactory.createCheckButton(pageComponent,
				Messages.SarlEditorPreferencePage_0,
				null,
				getPreferenceStore().getBoolean(SARLSourceViewerPreferenceAccess.AUTOFORMATTING_PROPERTY),
				1);
		this.autoformattingButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				updateValidity();
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent event) {
				//
			}
		});

		SWTFactory.createVerticalSpacer(pageComponent, 1);

		applyDialogFont(parent);

		updateValidity();

		return parent;
	}

	/** Update the validity flag from the widgets' contents.
	 */
	protected void updateValidity() {
		final boolean isValid;
		isValid = this.autoformattingButton.getSelection() != getPreferenceAccessor().isAutoFormattingEnabled();
		setValid(isValid);
	}

	@Override
	protected void performDefaults() {
		this.autoformattingButton.setSelection(SARLSourceViewerPreferenceAccess.AUTOFORMATTING_DEFAULT_VALUE);
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		final IPreferenceStore store = getPreferenceStore();
		store.setValue(
				SARLSourceViewerPreferenceAccess.AUTOFORMATTING_PROPERTY,
				this.autoformattingButton.getSelection());
		return super.performOk();
	}

}
