/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.ui.preferences;

import io.sarl.lang.validation.IssueCodes;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.xtext.validation.SeverityConverter;
import org.eclipse.xtext.xbase.ui.validation.XbaseValidationConfigurationBlock;

/** Preference page that permits to configure the SARL validator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLValidatorConfigurationBlock extends XbaseValidationConfigurationBlock {

	private static final String SECTION_NAME = SARLValidatorConfigurationBlock.class.getName();

	@Override
	public void dispose() {
		storeSectionExpansionStates(getDialogSettings());
		super.dispose();
	}

	@Override
	protected IDialogSettings getDialogSettings() {
		IDialogSettings dialogSettings = super.getDialogSettings();
		IDialogSettings section = dialogSettings.getSection(SECTION_NAME);
		if (section == null) {
			return dialogSettings.addNewSection(SECTION_NAME);
		}
		return section;
	}

	@Override
	protected void addAdditionalComponentsToSettingsPage(Composite settingsPage, int nColumns, int defaultIndent) {
		super.addAdditionalComponentsToSettingsPage(settingsPage, nColumns, defaultIndent);
		createHorizontalLine(settingsPage, nColumns);
		String[] values = new String[] {
				SeverityConverter.SEVERITY_ERROR,
				SeverityConverter.SEVERITY_WARNING,
				SeverityConverter.SEVERITY_IGNORE,
		};
		String[] valueLabels = new String[] {
				Messages.SARLValidatorConfigurationBlock_6,
				Messages.SARLValidatorConfigurationBlock_7,
				Messages.SARLValidatorConfigurationBlock_8,
		};
		Composite composite = new Composite(settingsPage, SWT.NONE);
		GridLayout layout = new GridLayout(nColumns, false);
		layout.marginHeight = 0;
		composite.setLayoutData(new GridData(GridData.FILL, GridData.CENTER, true, false, nColumns, 1));
		composite.setLayout(layout);
		addComboBox(composite,
				Messages.SARLValidatorConfigurationBlock_9,
				org.eclipse.xtext.xbase.validation.IssueCodes.COPY_JAVA_PROBLEMS,
				defaultIndent, values, valueLabels);
	}

	private static void createHorizontalLine(Composite settingsPage, int nColumns) {
		Label horizontalLine = new Label(settingsPage, SWT.SEPARATOR | SWT.HORIZONTAL);
		horizontalLine.setLayoutData(new GridData(GridData.FILL, GridData.FILL, true, false, nColumns, 1));
		horizontalLine.setFont(settingsPage.getFont());
	}

	@Override
	protected void fillCodingStyleSection(ComboBoxBuilder builder) {
		super.fillCodingStyleSection(builder);
		builder.addComboBox(IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, Messages.SARLValidatorConfigurationBlock_0);
		builder.addComboBox(IssueCodes.WRONG_PACKAGE, Messages.SARLValidatorConfigurationBlock_1);
		builder.addComboBox(IssueCodes.REDUNDANT_CAPACITY_USE, Messages.SARLValidatorConfigurationBlock_10);
		builder.addComboBox(IssueCodes.UNUSED_AGENT_CAPACITY, Messages.SARLValidatorConfigurationBlock_11);
	}

	@Override
	protected void fillPotentialProgrammingProblemsSection(ComboBoxBuilder builder) {
		super.fillPotentialProgrammingProblemsSection(builder);
		builder.addComboBox(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				Messages.SARLValidatorConfigurationBlock_2);
		builder.addComboBox(IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION, Messages.SARLValidatorConfigurationBlock_3);
	}

	@Override
	protected void fillUnusedCodeSection(ComboBoxBuilder builder) {
		super.fillUnusedCodeSection(builder);
		builder.addComboBox(IssueCodes.DISCOURAGED_CAPACITY_DEFINITION, Messages.SARLValidatorConfigurationBlock_4);
		builder.addComboBox(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT, Messages.SARLValidatorConfigurationBlock_5);
	}

}
