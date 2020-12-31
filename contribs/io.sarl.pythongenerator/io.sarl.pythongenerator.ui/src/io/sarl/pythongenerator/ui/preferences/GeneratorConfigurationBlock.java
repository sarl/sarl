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

package io.sarl.pythongenerator.ui.preferences;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;

import io.sarl.lang.ui.extralanguage.preferences.ExtraLanguagePreferenceAccess;
import io.sarl.lang.ui.extralanguage.properties.AbstractGeneratorConfigurationBlock;
import io.sarl.pythongenerator.generator.PyGeneratorPlugin;
import io.sarl.pythongenerator.ui.PyGeneratorUiPlugin;
import io.sarl.pythongenerator.ui.configuration.PyPreferenceAccess;

/** Configuration block for the Python generator.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
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
