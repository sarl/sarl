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

package io.sarl.eclipse.pythongenerator.preferences;

import com.google.inject.Inject;

import io.sarl.lang.pythongenerator.PyGeneratorPlugin;
import io.sarl.lang.ui.extralanguage.properties.AbstractExtraLanguagePropertyPage;

/** Property page for configuring the Python generator.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse.pythongenerator 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.pythongenerator
 * @since 0.6
 */
public class PropertyPage extends AbstractExtraLanguagePropertyPage {

	/** Inject the configuration block.
	 *
	 * @param block the block.
	 */
	@Inject
	public void setGeneratorConfigurationBlock(GeneratorConfigurationBlock block) {
		setInternalConfigurationBlock(block);
	}

	@Override
	protected String getPreferenceContainerID() {
		return PyGeneratorPlugin.PREFERENCE_ID;
	}

}
