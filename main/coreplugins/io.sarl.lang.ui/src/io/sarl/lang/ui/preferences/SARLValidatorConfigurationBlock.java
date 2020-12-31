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

package io.sarl.lang.ui.preferences;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.xtend.ide.validator.preferences.XtendValidatorConfigurationBlock;

import io.sarl.lang.validation.IssueCodes;

/** Preference page that permits to configure the SARL validator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLValidatorConfigurationBlock extends XtendValidatorConfigurationBlock {

	private static final String SECTION_NAME = SARLValidatorConfigurationBlock.class.getName();

	@Override
	public void dispose() {
		storeSectionExpansionStates(getDialogSettings());
		super.dispose();
	}

	@Override
	protected IDialogSettings getDialogSettings() {
		final IDialogSettings dialogSettings = super.getDialogSettings();
		final IDialogSettings section = dialogSettings.getSection(SECTION_NAME);
		if (section == null) {
			return dialogSettings.addNewSection(SECTION_NAME);
		}
		return section;
	}

	@Override
	protected void addAdditionalComponentsToSettingsPage(Composite settingsPage, int nbColumns, int defaultIndent) {
		super.addAdditionalComponentsToSettingsPage(settingsPage, nbColumns, defaultIndent);
		// Xtend block add the "Display Java Problems in Xtend".
		// It must be replaced by "Display Java Problems in SARL".
		for (final Control ctrl : settingsPage.getChildren()) {
			if (ctrl instanceof Composite) {
				final Composite composite = (Composite) ctrl;
				for (final Control ctrl2 : composite.getChildren()) {
					if (ctrl2 instanceof Label
							&& Messages.SARLValidatorConfigurationBlock_1.equals(((Label) ctrl2).getText())) {
						((Label) ctrl2).setText(Messages.SARLValidatorConfigurationBlock_9);
						break;
					}
				}
			}
		}
	}

	@Override
	protected void fillCodingStyleSection(ComboBoxBuilder builder) {
		super.fillCodingStyleSection(builder);
		builder.addComboBox(IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION, Messages.SARLValidatorConfigurationBlock_0);
		builder.addComboBox(IssueCodes.REDUNDANT_CAPACITY_USE, Messages.SARLValidatorConfigurationBlock_10);
		builder.addComboBox(IssueCodes.UNUSED_AGENT_CAPACITY, Messages.SARLValidatorConfigurationBlock_11);
		builder.addComboBox(IssueCodes.DISCOURAGED_FUNCTION_NAME, Messages.SARLValidatorConfigurationBlock_12);
		builder.addComboBox(IssueCodes.DISCOURAGED_LOOP_BREAKING_KEYWORD_USE, Messages.SARLValidatorConfigurationBlock_17);
		builder.addComboBox(IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION, Messages.SARLValidatorConfigurationBlock_19);
	}

	@Override
	protected void fillPotentialProgrammingProblemsSection(ComboBoxBuilder builder) {
		super.fillPotentialProgrammingProblemsSection(builder);
		builder.addComboBox(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				Messages.SARLValidatorConfigurationBlock_13);
		builder.addComboBox(org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING,
				Messages.SARLValidatorConfigurationBlock_2);
		builder.addComboBox(IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION, Messages.SARLValidatorConfigurationBlock_3);
		builder.addComboBox(org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE,
				Messages.SARLValidatorConfigurationBlock_14);
		builder.addComboBox(org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT,
				Messages.SARLValidatorConfigurationBlock_15);
		builder.addComboBox(IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED,
				Messages.SARLValidatorConfigurationBlock_16);
		builder.addComboBox(IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE,
				Messages.SARLValidatorConfigurationBlock_6);
		builder.addComboBox(IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM,
				Messages.SARLValidatorConfigurationBlock_18);
	}

	@Override
	protected void fillRestrictedApiSection(ComboBoxBuilder builder) {
		super.fillRestrictedApiSection(builder);
		builder.addComboBox(IssueCodes.MANUAL_INLINE_DEFINITION,
				Messages.SARLValidatorConfigurationBlock_8);
		builder.addComboBox(IssueCodes.USED_RESERVED_SARL_ANNOTATION,
				Messages.SARLValidatorConfigurationBlock_7);
	}

	@Override
	protected void fillUnusedCodeSection(ComboBoxBuilder builder) {
		super.fillUnusedCodeSection(builder);
		builder.addComboBox(IssueCodes.DISCOURAGED_CAPACITY_DEFINITION, Messages.SARLValidatorConfigurationBlock_4);
		builder.addComboBox(IssueCodes.UNREACHABLE_BEHAVIOR_UNIT, Messages.SARLValidatorConfigurationBlock_5);
	}

}
