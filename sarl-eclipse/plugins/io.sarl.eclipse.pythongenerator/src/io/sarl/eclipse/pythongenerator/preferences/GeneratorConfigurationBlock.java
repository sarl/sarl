/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.pythongenerator.preferences;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import io.sarl.eclipse.pythongenerator.PyGeneratorUiPlugin;
import io.sarl.eclipse.pythongenerator.configuration.PyPreferenceAccess;
import io.sarl.lang.pythongenerator.PyGeneratorPlugin;
import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;
import io.sarl.lang.ui.extralanguage.properties.AbstractGeneratorConfigurationBlock;

/** Configuration block for the Python generator.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.pythongenerator 0.13.0 20230919-093100
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.pythongenerator
 * @since 0.6
 */
public class GeneratorConfigurationBlock extends AbstractGeneratorConfigurationBlock {

	private static final String PYTHON_ICON = "icons/python.png"; //$NON-NLS-1$

	/** Constructor.
	 */
	public GeneratorConfigurationBlock() {
		// The python generator is marked as experimental
		super(true);
	}

	@Override
	public IDialogSettings getDialogSettings() {
		return PyGeneratorUiPlugin.getDefault().getDialogSettings();
	}

	@Override
	public String getPreferenceID() {
		return PyGeneratorPlugin.PREFERENCE_ID;
	}

	@Override
	protected String getActivationText() {
		return Messages.GeneratorConfigurationBlock_1;
	}

	@Override
	public Image getTargetLanguageImage() {
		return PyGeneratorUiPlugin.getDefault().getImage(PYTHON_ICON);
	}

	@Override
	protected void createGeneralSectionItems(Composite composite) {
		super.createGeneralSectionItems(composite);
		addCheckBox(composite, Messages.GeneratorConfigurationBlock_0,
				ExtraLanguagePreferenceAccess.getPrefixedKey(getPreferenceID(),
						PyPreferenceAccess.JYTHON_COMPLIANCE_PROPERTY),
				BOOLEAN_VALUES, 0);
	}

}
