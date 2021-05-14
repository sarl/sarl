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
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import io.sarl.lang.ui.codemining.SARLCodeminingPreferenceAccess;
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
	private SARLSourceViewerPreferenceAccess sourceViewerPreferences;

	@Inject
	private SARLCodeminingPreferenceAccess codeminingPreferences;

	private Button autoformattingButton;

	private Button codeminingButton;

	private Button codeminingFieldTypeButton;

	private Button codeminingVariableTypeButton;

	private Button codeminingActionReturnTypeButton;

	private Button codeminingArgumentNameButton;

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

	/** Replies the preferences for the source viewer.
	 *
	 * @return the preference accessor.
	 */
	protected SARLSourceViewerPreferenceAccess getSourceViewerPreferenceAccessor() {
		return this.sourceViewerPreferences;
	}

	/** Replies the preferences for the codemining.
	 *
	 * @return the preference accessor.
	 * @since 0.8
	 */
	protected SARLCodeminingPreferenceAccess getCodeminingPreferenceAccessor() {
		return this.codeminingPreferences;
	}

	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		return getCodeminingPreferenceAccessor().getWritablePreferenceStore(null);
	}

	@Override
	protected Control createContents(Composite parent) {
		initializeDialogUnits(parent);
		//noDefaultAndApplyButton();

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

		// ensure all the accessors use the same preference store (because only one
		// store is saved when closing the dialog box).
		getSourceViewerPreferenceAccessor().setWritablePreferenceStore(getPreferenceStore());
		getCodeminingPreferenceAccessor().setWritablePreferenceStore(getPreferenceStore());

		// create the content
		this.autoformattingButton = SWTFactory.createCheckButton(pageComponent,
				Messages.SarlEditorPreferencePage_0,
				null,
				getSourceViewerPreferenceAccessor().isAutoFormattingEnabled(),
				2);

		SWTFactory.createVerticalSpacer(pageComponent, 1);

		this.codeminingButton = SWTFactory.createCheckButton(pageComponent,
				Messages.SarlEditorPreferencePage_1,
				null,
				getCodeminingPreferenceAccessor().isCodeminingEnabled(),
				2);
		this.codeminingButton.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				enableButtons();
			}

			@Override
			public void widgetDefaultSelected(SelectionEvent event) {
				enableButtons();
			}
		});
		final Group codeminingGroup = SWTFactory.createGroup(parent, Messages.SarlEditorPreferencePage_2, 1, 2, GridData.FILL_HORIZONTAL);
		this.codeminingFieldTypeButton = SWTFactory.createCheckButton(codeminingGroup,
				Messages.SarlEditorPreferencePage_3,
				null,
				getCodeminingPreferenceAccessor().isCodeminingFieldTypeEnabled(),
				1);
		this.codeminingVariableTypeButton = SWTFactory.createCheckButton(codeminingGroup,
				Messages.SarlEditorPreferencePage_4,
				null,
				getCodeminingPreferenceAccessor().isCodeminingVariableTypeEnabled(),
				1);
		this.codeminingActionReturnTypeButton = SWTFactory.createCheckButton(codeminingGroup,
				Messages.SarlEditorPreferencePage_5,
				null,
				getCodeminingPreferenceAccessor().isCodeminingActionReturnTypeEnabled(),
				1);
		this.codeminingArgumentNameButton = SWTFactory.createCheckButton(codeminingGroup,
				Messages.SarlEditorPreferencePage_6,
				null,
				getCodeminingPreferenceAccessor().isCodeminingFeatureCallArgumentNameEnabled(),
				1);

		SWTFactory.createVerticalSpacer(pageComponent, 1);

		applyDialogFont(parent);

		enableButtons();

		return parent;
	}

	/**
	 * Enables the buttons.
	 */
	private void enableButtons() {
		final boolean isCodeminingEnabled = this.codeminingButton.getSelection();
		this.codeminingFieldTypeButton.setEnabled(isCodeminingEnabled);
		this.codeminingVariableTypeButton.setEnabled(isCodeminingEnabled);
		this.codeminingActionReturnTypeButton.setEnabled(isCodeminingEnabled);
		this.codeminingArgumentNameButton.setEnabled(isCodeminingEnabled);
	}

	@Override
	protected void performDefaults() {
		this.autoformattingButton.setSelection(SARLSourceViewerPreferenceAccess.AUTOFORMATTING_DEFAULT_VALUE);
		this.codeminingFieldTypeButton.setSelection(SARLCodeminingPreferenceAccess.CODEMINING_FIELD_TYPE_VALUE);
		this.codeminingVariableTypeButton.setSelection(SARLCodeminingPreferenceAccess.CODEMINING_VARIABLE_TYPE_VALUE);
		this.codeminingActionReturnTypeButton.setSelection(SARLCodeminingPreferenceAccess.CODEMINING_ACTION_RETURN_TYPE_VALUE);
		this.codeminingArgumentNameButton.setSelection(SARLCodeminingPreferenceAccess.CODEMINING_FEATURECALL_ARGUMENT_NAME_VALUE);
		this.codeminingButton.setSelection(SARLCodeminingPreferenceAccess.CODEMINING_DEFAULT_VALUE);
		super.performDefaults();
		enableButtons();
	}

	@Override
	public boolean performOk() {
		getSourceViewerPreferenceAccessor().setAutoFormattingEnabled(this.autoformattingButton.getSelection());
		final SARLCodeminingPreferenceAccess codeminingAccessor = getCodeminingPreferenceAccessor();
		codeminingAccessor.setCodeminingFieldTypeEnabled(this.codeminingFieldTypeButton.getSelection());
		codeminingAccessor.setCodeminingVariableTypeEnabled(this.codeminingVariableTypeButton.getSelection());
		codeminingAccessor.setCodeminingActionReturnTypeEnabled(this.codeminingActionReturnTypeButton.getSelection());
		codeminingAccessor.setCodeminingFeatureCallArgumentNameEnabled(this.codeminingArgumentNameButton.getSelection());
		codeminingAccessor.setCodeminingEnabled(this.codeminingButton.getSelection());
		return super.performOk();
	}

}
