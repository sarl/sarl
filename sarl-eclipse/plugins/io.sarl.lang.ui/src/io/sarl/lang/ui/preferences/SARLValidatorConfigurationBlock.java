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

package io.sarl.lang.ui.preferences;

import static io.sarl.lang.validation.IssueCodes.AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_BOOLEAN_EXPRESSION;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_CAPACITY_DEFINITION;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_FUNCTION_NAME;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_LOOP_BREAKING_KEYWORD_USE;
import static io.sarl.lang.validation.IssueCodes.DISCOURAGED_OCCURRENCE_READONLY_USE;
import static io.sarl.lang.validation.IssueCodes.MANUAL_INLINE_DEFINITION;
import static io.sarl.lang.validation.IssueCodes.PARAMETER_DEFAULT_VALUE_REDFINITION;
import static io.sarl.lang.validation.IssueCodes.POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM;
import static io.sarl.lang.validation.IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION;
import static io.sarl.lang.validation.IssueCodes.POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL;
import static io.sarl.lang.validation.IssueCodes.REDUNDANT_CAPACITY_USE;
import static io.sarl.lang.validation.IssueCodes.REDUNDANT_INTERFACE_IMPLEMENTATION;
import static io.sarl.lang.validation.IssueCodes.RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED;
import static io.sarl.lang.validation.IssueCodes.UNNECESSARY_FIRED_EVENT;
import static io.sarl.lang.validation.IssueCodes.UNREACHABLE_BEHAVIOR_UNIT;
import static io.sarl.lang.validation.IssueCodes.UNUSED_AGENT_CAPACITY;
import static io.sarl.lang.validation.IssueCodes.UNUSED_TYPE_PARAMETER;
import static io.sarl.lang.validation.IssueCodes.USED_RESERVED_SARL_ANNOTATION;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_ABSTRACT;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_OVERRIDE;
import static org.eclipse.xtend.core.validation.IssueCodes.MISSING_STATIC_MODIFIER;
import static org.eclipse.xtext.xbase.validation.IssueCodes.ABSTRACT_METHOD_INVOCATION;
import static org.eclipse.xtext.xbase.validation.IssueCodes.DUPLICATE_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.IMPORT_DUPLICATE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.IMPORT_UNRESOLVED;
import static org.eclipse.xtext.xbase.validation.IssueCodes.RAW_TYPE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.VARIABLE_NAME_SHADOWING;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.xtend.ide.validator.preferences.XtendValidatorConfigurationBlock;

/** Preference page that permits to configure the SARL validator.
 *
 * @author $Author: sgalland$
 * @version io.sarl.lang.ui 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.lang.ui
 */
@SuppressWarnings("restriction")
public class SARLValidatorConfigurationBlock extends XtendValidatorConfigurationBlock {

	private static final String SECTION_NAME = SARLValidatorConfigurationBlock.class.getName();

	@Override
	public void dispose() {
		storeSectionExpansionStates(getDialogSettings());
		super.dispose();
	}

	@Override
	protected IDialogSettings getDialogSettings() {
		final var dialogSettings = super.getDialogSettings();
		final var section = dialogSettings.getSection(SECTION_NAME);
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
		for (final var ctrl : settingsPage.getChildren()) {
			if (ctrl instanceof Composite composite) {
				for (final var ctrl2 : composite.getChildren()) {
					if (ctrl2 instanceof Label cvalue
							&& Messages.SARLValidatorConfigurationBlock_1.equals(cvalue.getText())) {
						cvalue.setText(Messages.SARLValidatorConfigurationBlock_9);
						break;
					}
				}
			}
		}
	}

	@Override
	protected void fillCodingStyleSection(ComboBoxBuilder builder) {
		super.fillCodingStyleSection(builder);
		builder.addComboBox(IMPORT_DUPLICATE, Messages.SARLValidatorConfigurationBlock_28);
		builder.addComboBox(REDUNDANT_CAPACITY_USE, Messages.SARLValidatorConfigurationBlock_10);
		builder.addComboBox(UNUSED_AGENT_CAPACITY, Messages.SARLValidatorConfigurationBlock_11);
		builder.addComboBox(REDUNDANT_INTERFACE_IMPLEMENTATION, Messages.SARLValidatorConfigurationBlock_0);
		builder.addComboBox(DISCOURAGED_FUNCTION_NAME, Messages.SARLValidatorConfigurationBlock_12);
		builder.addComboBox(DISCOURAGED_LOOP_BREAKING_KEYWORD_USE, Messages.SARLValidatorConfigurationBlock_17);
		builder.addComboBox(POTENTIAL_INEFFICIENT_VALUE_CONVERSION, Messages.SARLValidatorConfigurationBlock_19);
	}

	@Override
	protected void fillPotentialProgrammingProblemsSection(ComboBoxBuilder builder) {
		super.fillPotentialProgrammingProblemsSection(builder);
		builder.addComboBox(IMPORT_UNRESOLVED, Messages.SARLValidatorConfigurationBlock_29);
		builder.addComboBox(POTENTIAL_MEMORY_SHARING_OUTSIDE_AGENT_CONTROL, Messages.SARLValidatorConfigurationBlock_21);
		builder.addComboBox(DUPLICATE_TYPE, Messages.SARLValidatorConfigurationBlock_27);
		builder.addComboBox(POTENTIAL_FIELD_SYNCHRONIZATION_PROBLEM, Messages.SARLValidatorConfigurationBlock_18);
		builder.addComboBox(MISSING_OVERRIDE, Messages.SARLValidatorConfigurationBlock_14);
		builder.addComboBox(ABSTRACT_METHOD_INVOCATION, Messages.SARLValidatorConfigurationBlock_26);
		builder.addComboBox(MISSING_ABSTRACT, Messages.SARLValidatorConfigurationBlock_15);
		builder.addComboBox(MISSING_STATIC_MODIFIER, Messages.SARLValidatorConfigurationBlock_25);
		builder.addComboBox(PARAMETER_DEFAULT_VALUE_REDFINITION, Messages.SARLValidatorConfigurationBlock_20);
		builder.addComboBox(RETURN_TYPE_SPECIFICATION_IS_RECOMMENDED, Messages.SARLValidatorConfigurationBlock_16);
		builder.addComboBox(DISCOURAGED_OCCURRENCE_READONLY_USE, Messages.SARLValidatorConfigurationBlock_6);
		builder.addComboBox(RAW_TYPE, Messages.SARLValidatorConfigurationBlock_30);
		builder.addComboBox(VARIABLE_NAME_SHADOWING, Messages.SARLValidatorConfigurationBlock_13);
		builder.addComboBox(VARIABLE_NAME_SHADOWING, Messages.SARLValidatorConfigurationBlock_2);
		builder.addComboBox(DISCOURAGED_BOOLEAN_EXPRESSION, Messages.SARLValidatorConfigurationBlock_3);
		builder.addComboBox(AMBIGUOUS_INTERPRETATION_BY_DEVELOPPER, Messages.SARLValidatorConfigurationBlock_22);
	}

	@Override
	protected void fillRestrictedApiSection(ComboBoxBuilder builder) {
		super.fillRestrictedApiSection(builder);
		builder.addComboBox(MANUAL_INLINE_DEFINITION, Messages.SARLValidatorConfigurationBlock_8);
		builder.addComboBox(USED_RESERVED_SARL_ANNOTATION, Messages.SARLValidatorConfigurationBlock_7);
	}

	@Override
	protected void fillUnusedCodeSection(ComboBoxBuilder builder) {
		super.fillUnusedCodeSection(builder);
		builder.addComboBox(DISCOURAGED_CAPACITY_DEFINITION, Messages.SARLValidatorConfigurationBlock_4);
		builder.addComboBox(UNREACHABLE_BEHAVIOR_UNIT, Messages.SARLValidatorConfigurationBlock_5);
		builder.addComboBox(UNUSED_TYPE_PARAMETER, Messages.SARLValidatorConfigurationBlock_24);
		builder.addComboBox(UNNECESSARY_FIRED_EVENT, Messages.SARLValidatorConfigurationBlock_23);
	}

}
