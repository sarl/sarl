/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

import static io.sarl.lang.ui.preferences.SARLBuilderPreferenceAccess.PREF_GENERATE_INLINE;
import static io.sarl.lang.ui.preferences.SARLBuilderPreferenceAccess.PREF_USE_EXPRESSION_INTERPRETER;

import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.xtext.xbase.ui.builder.XbaseBuilderConfigurationBlock;

/** Preference page that permits to configure the SARL builder.
 *
 * <p>This page extends the Xbase page.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLBuilderConfigurationBlock extends XbaseBuilderConfigurationBlock {

	@Override
	protected void createGeneralSectionItems(Composite composite) {
		super.createGeneralSectionItems(composite);

		final Button generateInlineButton = addCheckBox(composite, Messages.SARLBuilderConfigurationBlock_0,
				PREF_GENERATE_INLINE, BOOLEAN_VALUES, 0);

		final Button useExpressionInterpreter = addCheckBox(composite, Messages.SARLBuilderConfigurationBlock_1,
				PREF_USE_EXPRESSION_INTERPRETER, BOOLEAN_VALUES, INDENT_AMOUNT);
		useExpressionInterpreter.setEnabled(generateInlineButton.getSelection());

		generateInlineButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				useExpressionInterpreter.setEnabled(generateInlineButton.getSelection());
			}
		});
	}

}
